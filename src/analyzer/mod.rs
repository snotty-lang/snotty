pub mod value;

use std::collections::HashMap;

use crate::error::{Error, ErrorKind};
use crate::parser::syntax::{ParseTree, SyntaxKind};
use crate::tree::{Leaf, LeafId, Node, NodeId, Result, TreeBuilder, TreeElement};

use value::{LeafType, Value, ValueData, ValueType};
use SyntaxKind::*;

#[derive(Debug, Default)]
pub struct Analyzer<'a> {
    source: &'a str,
    errors: Vec<Error<'a>>,
    lookup: Vec<HashMap<&'a str, usize>>,
    memory: Vec<Value>,
    builder: TreeBuilder<Value, LeafType>,
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

    pub fn analyze(mut self, tree: &ParseTree) -> Result<'a, Value, LeafType> {
        let root = tree.node(tree.root());
        self.builder.start_node(root.kind(), root.span().start);
        for child in root.children_with_leaves(tree) {
            self.analyze_element(tree, child);
        }
        self.builder.finish_node(root.span().end, |_| None);

        Result {
            errors: self.errors,
            output: self.builder.finish(),
        }
    }

    fn analyze_element(
        &mut self,
        tree: &ParseTree,
        element: TreeElement<NodeId, LeafId>,
    ) -> TreeElement<NodeId, LeafId> {
        match element {
            TreeElement::Node(id) => TreeElement::Node(self.analyze_node(tree, id, tree.node(id))),
            TreeElement::Leaf(id) => TreeElement::Leaf(self.analyze_leaf(tree, id, tree.leaf(id))),
        }
    }

    fn analyze_node(&mut self, tree: &ParseTree, _id: NodeId, node: &Node<()>) -> NodeId {
        match node.kind() {
            Statement => {
                self.builder.start_node(node.kind(), node.span().start);
                for &child in node.children() {
                    self.analyze_node(tree, _id, tree.node(child));
                }
                self.builder.finish_node(node.span().end, |_| None)
            }
            BinaryOp => {
                self.builder.start_node(node.kind(), node.span().start);
                let mut iter = node.children_with_leaves(tree);
                let a = self
                    .analyze_element(tree, iter.next().unwrap())
                    .as_node()
                    .unwrap();
                let op = self
                    .analyze_element(tree, iter.next().unwrap())
                    .as_leaf()
                    .unwrap();
                let op = self.builder.leaf(op).kind();
                let b = self
                    .analyze_element(tree, iter.next().unwrap())
                    .as_node()
                    .unwrap();
                let type_a = self.builder.node(a).data().as_ref().unwrap().type_;
                let type_b = self.builder.node(b).data().as_ref().unwrap().type_;
                let type_ = match type_a.operate_binary(op, type_b) {
                    Some(t) => t,
                    None => {
                        self.errors.push(Error::error(
                            ErrorKind::UnsupportedOperation { operation: op },
                            node.span(),
                            self.source,
                        ));
                        ValueType::Posisoned
                    }
                };
                self.builder.finish_node(node.span().end, |id| {
                    Some(Value {
                        value: None,
                        syntax: TreeElement::Node(id),
                        type_,
                    })
                })
            }
            Value => {
                self.builder.start_node(node.kind(), node.span().start);
                let &LeafType::Value(Value { type_, .. }) = self.analyze_element(tree, node.children_with_leaves(tree).next().unwrap()).get_from_builder(&self.builder).as_leaf().unwrap().data().as_ref().unwrap() else { unreachable!() };
                self.builder.finish_node(node.span().end, |id| {
                    Some(Value {
                        value: None,
                        syntax: TreeElement::Node(id),
                        type_,
                    })
                })
            }
            s => todo!("{s}"),
        }
    }

    fn analyze_leaf(&mut self, _tree: &ParseTree, id: LeafId, leaf: &Leaf<SyntaxKind>) -> LeafId {
        match leaf.kind() {
            Add | Mul | Div | Sub | Mod | Inc | Dec | And | Or | Not | Shl | Shr | Equal
            | NotEqual | Xor | LessThan | LessEqual | GreaterThan | GreaterEqual => {
                self.builder.push(leaf.kind(), leaf.span(), |_| None)
            }
            Number => self.builder.push(leaf.kind(), leaf.span(), |_| {
                Some(LeafType::Value(Value {
                    value: Some(ValueData::Number(self.source[leaf.span()].parse().unwrap())),
                    syntax: TreeElement::Leaf(id),
                    type_: ValueType::Number,
                }))
            }),
            SemiColon => self.builder.push(leaf.kind(), leaf.span(), |_| {
                Some(LeafType::Value(Value {
                    value: Some(ValueData::None),
                    syntax: TreeElement::Leaf(id),
                    type_: ValueType::None,
                }))
            }),
            Char => {
                let s = &self.source[leaf.span()];
                let mut s = s[1..s.len() - 1].chars();
                let c = match s.next().unwrap() {
                    '\\' => match s.next().unwrap() {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        c @ ('\\' | '\'' | '\"') => c,
                        'x' => {
                            todo!()
                        }
                        '0'..='7' => {
                            todo!()
                        }
                        _ => unreachable!(),
                    },
                    c => c,
                };
                self.builder.push(leaf.kind(), leaf.span(), |_| {
                    Some(LeafType::Value(Value {
                        value: Some(ValueData::Char(c as u8)),
                        syntax: TreeElement::Leaf(id),
                        type_: ValueType::Number,
                    }))
                })
            }
            String => {
                let mut new = Vec::new();
                let s = &self.source[leaf.span()];
                let mut s = s[1..s.len() - 1].chars();
                while let Some(c) = s.next() {
                    new.push(match c {
                        '\\' => match s.next().unwrap() {
                            'n' => '\n',
                            't' => '\t',
                            'r' => '\r',
                            c @ ('\\' | '\'' | '\"') => c,
                            'x' => {
                                todo!()
                            }
                            '0'..='7' => {
                                todo!()
                            }
                            _ => unreachable!(),
                        },
                        c => c,
                    } as u8);
                }
                self.builder.push(leaf.kind(), leaf.span(), move |_| {
                    Some(LeafType::Value(Value {
                        value: Some(ValueData::String(new)),
                        syntax: TreeElement::Leaf(id),
                        type_: ValueType::Pointer(0),
                    }))
                })
            }
            s => todo!("{s}"),
        }
    }
}
