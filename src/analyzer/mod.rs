pub mod tree;
pub mod value;

use cstree::NodeOrToken;
use std::collections::HashMap;

use crate::error::{Error, ErrorKind};

use crate::parser::syntax::{SyntaxKind, SyntaxNode, SyntaxToken};
use SyntaxKind::*;

use tree::{Analysis, AnalyzedTreeBuilder};
use value::{Leaf, Value};

use self::value::ValueKind;

#[derive(Debug, Default)]
pub struct Analyzer<'a> {
    source: &'a str,
    errors: Vec<Error<'a>>,
    lookup: Vec<HashMap<&'a str, usize>>,
    memory: Vec<Value>,
    builder: AnalyzedTreeBuilder<Leaf>,
}

impl<'a> Analyzer<'a> {
    pub fn new(source: &'a str) -> Self {
        Analyzer {
            source,
            errors: Vec::new(),
            lookup: Vec::new(),
            memory: Vec::new(),
            builder: AnalyzedTreeBuilder::new(),
        }
    }

    pub fn analyze(mut self, syntax_node: &SyntaxNode) -> Analysis<'a, Leaf> {
        self.analyze_inner(syntax_node);
        Analysis {
            errors: self.errors,
            analyzed: self.builder.finish(),
        }
    }

    fn analyze_inner(&mut self, node: &SyntaxNode) {
        let mut iter = node.children_with_tokens();
        while self.deal_with_node_or_token(&mut iter).is_some() {}
    }

    fn deal_with_node_or_token<'b>(
        &mut self,
        iter: &mut impl Iterator<Item = NodeOrToken<&'b SyntaxNode, &'b SyntaxToken>>,
    ) -> Option<Option<usize>> {
        loop {
            return match iter.next() {
                None => None,
                Some(s) => Some(match s {
                    NodeOrToken::Node(n) if matches!(n.kind(), Whitespace | Comment) => continue,
                    NodeOrToken::Token(t) if matches!(t.kind(), Whitespace | Comment) => continue,
                    NodeOrToken::Node(n) => self.deal_with_node(n),
                    NodeOrToken::Token(t) => {
                        if let Some(leaf) = self.deal_with_token(t) {
                            self.builder.push(leaf);
                        }
                        None
                    }
                }),
            };
        }
    }

    fn deal_with_node(&mut self, node: &SyntaxNode) -> Option<usize> {
        match node.kind() {
            BinaryOp => {
                self.builder.start_node(node.clone());
                let mut iter = node.children_with_tokens();
                let a = self.deal_with_node_or_token(&mut iter).unwrap().unwrap();
                let op = match self
                    .deal_with_token(iter.next().unwrap().as_token().unwrap())
                    .unwrap()
                {
                    Leaf::Operator(op) => op,
                    _ => unreachable!(),
                };
                let b = self.deal_with_node_or_token(&mut iter).unwrap().unwrap();
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
                Some(0)
            }
            _ => todo!(),
        }
    }

    fn deal_with_token(&mut self, token: &SyntaxToken) -> Option<Leaf> {
        match token.kind() {
            Whitespace | Comment => None,
            Error => None,
            Mul | Div | Mod | Add | Sub | And | Or | Xor | Not | LessEqual | LessThan
            | GreaterEqual | GreaterThan | Equal | NotEqual | Shl | Shr | Inc | Dec => {
                Some(Leaf::Operator(token.clone()))
            }
            InKw => Some(Leaf::Input(token.clone())),
            _ => unreachable!(),
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
