pub mod value;

use std::collections::HashMap;

use crate::error::{Error, ErrorKind};

use crate::parser::syntax::{Syntax, SyntaxKind};
use SyntaxKind::*;

use crate::tree::{Result, TreeBuilder, TreeElement, Tree};
use value::{Leaf, Value, ValueKind, ValueType};

#[derive(Debug, Default)]
pub struct Analyzer<'a> {
    source: &'a str,
    errors: Vec<Error<'a>>,
    lookup: Vec<HashMap<&'a str, usize>>,
    memory: Vec<Value>,
    builder: TreeBuilder<Leaf>,
}

impl<'a> Analyzer<'a> {
    pub fn new(source: &'a str) -> Self {
        Analyzer {
            source,
            errors: Vec::new(),
            lookup: Vec::new(),
            memory: Vec::new(),
            builder: TreeBuilder::new(),
        }
    }

    pub fn analyze(mut self, syntax_node: &Tree) -> Result<'a, Leaf> {
        self.analyze_inner(syntax_node);
        Result {
            errors: self.errors,
            output: self.builder.finish(),
        }
    }

    fn analyze_inner(&mut self, node: &Syntax) {
        let mut iter = node.children_with_tokens();
        while self.deal_with_node_or_token(&mut iter).is_some() {}
    }

    fn deal_with_node_or_token(
        &mut self,
        iter: &mut impl Iterator<Item = TreeElement>,
    ) -> Option<TreeElement<usize, usize>> {
        loop {
            return Some(match iter.next()? {
                TreeElement::Node(n) if matches!(n.kind, Whitespace | Comment) => continue,
                TreeElement::Leaf(t) if matches!(t.kind, Whitespace | Comment) => continue,
                TreeElement::Node(n) => TreeElement::Node(self.deal_with_node(n)),
                TreeElement::Leaf(t) => {
                    let leaf = self.deal_with_token(t);
                    if let Leaf::Value(v) = &leaf {
                        self.memory.push(v.clone());
                    }
                    self.builder.push(leaf);
                    TreeElement::Leaf(self.builder.leaf_count() - 1)
                }
            });
        }
    }

    fn deal_with_node(&mut self, node: Syntax) -> usize {
        match node.kind() {
            Statement | Value => {
                match self.deal_with_node_or_token(&mut node.children_with_tokens()) {
                    Some(TreeElement::Leaf(i) | TreeElement::Node(i)) => i,
                    _ => 0,
                }
            }
            BinaryOp => {
                self.builder.start_node(node.clone());
                let mut iter = node.children_with_tokens();
                let a = self
                    .deal_with_node_or_token(&mut iter)
                    .unwrap()
                    .as_node()
                    .unwrap();
                let op = match self.deal_with_node_or_token(&mut iter) {
                    Some(TreeElement::Leaf(i)) => match self.builder.leaf(i) {
                        Leaf::Operator(op) => op.clone(),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };
                let b = self
                    .deal_with_node_or_token(&mut iter)
                    .unwrap()
                    .as_node()
                    .unwrap();
                if !self
                    .get_value(a)
                    .type_
                    .can_operate_binary(&op, &self.get_value(b).type_)
                {
                    self.errors.push(Error::error(
                        ErrorKind::UnsupportedOperation {
                            operation: op.kind(),
                        },
                        node.text_range().into(),
                        self.source,
                    ))
                }
                self.builder.finish_node();
                0
            }
            s => todo!("{s:?}"),
        }
    }

    fn deal_with_token(&mut self, token: Syntax) -> Leaf {
        match token.kind() {
            Mul | Div | Mod | Add | Sub | And | Or | Xor | Not | LessEqual | LessThan
            | GreaterEqual | GreaterThan | Equal | NotEqual | Shl | Shr | Inc | Dec => {
                Leaf::Operator(token)
            }
            InKw => Leaf::Input(token),
            Number => {
                match self.source[token.text_range()]
                    .parse()
                    .map(ValueKind::Number)
                {
                    Ok(value) => Leaf::Value(Value {
                        value,
                        element: TreeElement::Token(token),
                        type_: ValueType::Number,
                    }),
                    Err(_) => {
                        self.errors.push(Error::error(
                            ErrorKind::ByteOverflow,
                            token.text_range().into(),
                            self.source,
                        ));
                        Leaf::Value(Value {
                            value: ValueKind::None,
                            element: TreeElement::Token(token),
                            type_: ValueType::Number,
                        })
                    }
                }
            }
            s => todo!("{s:?}"),
        }
    }

    fn get_value(&self, mut memory: usize) -> &Value {
        loop {
            match &self.memory[memory] {
                Value {
                    value: ValueKind::Variable(m),
                    ..
                } => memory = *m,
                s => return s,
            }
        }
    }
}
