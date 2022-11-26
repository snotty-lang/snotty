pub mod value;

use std::collections::HashMap;

use crate::error::{Error, ErrorKind};
use crate::parser::syntax::{ParseTree, SyntaxKind};
use crate::tree::{Leaf, LeafId, Node, NodeId, Result, TreeBuilder, TreeElement};

use value::{LeafType, Value, ValueData, ValueType};
use SyntaxKind::*;

use self::value::NodeType;

#[derive(Debug, Default)]
pub struct Analyzer<'a> {
    source: &'a str,
    errors: Vec<Error<'a>>,
    lookup: Vec<HashMap<&'a str, usize>>,
    memory: Vec<Value>,
    builder: TreeBuilder<NodeType, LeafType>,
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

    pub fn analyze(mut self, tree: &ParseTree) -> Result<'a, NodeType, LeafType> {
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
            TreeElement::Node(id) => TreeElement::Node(self.analyze_node(tree, tree.node(id))),
            TreeElement::Leaf(id) => TreeElement::Leaf(self.analyze_leaf(tree, tree.leaf(id))),
        }
    }

    fn analyze_node(&mut self, tree: &ParseTree, node: &Node<()>) -> NodeId {
        match node.kind() {
            Statement | Scope => {
                self.builder.start_node(node.kind(), node.span().start);
                for &child in node.children() {
                    self.analyze_node(tree, tree.node(child));
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
                let type_a = self.builder.node(a).data().as_ref().unwrap().type_();
                let type_b = self.builder.node(b).data().as_ref().unwrap().type_();
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
                    Some(NodeType::Value(Value {
                        value: None,
                        syntax: TreeElement::Node(id),
                        type_,
                    }))
                })
            }
            UnaryOp => {
                self.builder.start_node(node.kind(), node.span().start);
                let mut iter = node.children_with_leaves(tree);
                let a = self.analyze_element(tree, iter.next().unwrap());
                let b = self.analyze_element(tree, iter.next().unwrap());

                let (a, op) = match (a, b) {
                    (TreeElement::Node(a), TreeElement::Leaf(op))
                    | (TreeElement::Leaf(op), TreeElement::Node(a)) => (
                        a.get_from_builder(&self.builder),
                        op.get_from_builder(&self.builder).kind(),
                    ),
                    _ => unreachable!(),
                };

                let type_a = a.data().as_ref().unwrap().type_();
                let type_ = match type_a.operate_unary(op) {
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
                    Some(NodeType::Value(Value {
                        value: None,
                        syntax: TreeElement::Node(id),
                        type_,
                    }))
                })
            }
            // Cast => {
            //     self.builder.start_node(node.kind(), node.span().start);
            //     let mut iter = node.children_with_leaves(tree);
            //     let kind = self.analyze_element(tree, iter.next().unwrap()).as_node().unwrap();
            //     let a = self.analyze_element(tree, iter.next().unwrap()).as_node().unwrap();
            //     self.builder.finish_node(node.span().end, |id| {
            //         Some(Value {
            //             value: None,
            //             syntax: TreeElement::Node(id),
            //             type_,
            //         })
            //     })
            // }
            If => {
                self.builder.start_node(node.kind(), node.span().start);
                let mut iter = node.children_with_leaves(tree);
                let cond = self
                    .analyze_element(tree, iter.next().unwrap())
                    .as_node()
                    .unwrap();
                self.analyze_element(tree, iter.next().unwrap());
                iter.next().map(|e| self.analyze_element(tree, e));
                let cond = cond.get_from_builder(&self.builder);
                let type_ = cond.data().as_ref().unwrap().type_();
                if !type_.can_be_bool() {
                    self.errors.push(Error::error(
                        ErrorKind::TypeError {
                            type_: type_.clone(),
                        },
                        cond.span(),
                        self.source,
                    ))
                }
                self.builder.finish_node(node.span().end, |_| None)
            }
            Ternary => {
                self.builder.start_node(node.kind(), node.span().start);
                let mut iter = node.children_with_leaves(tree);
                let cond = self
                    .analyze_element(tree, iter.next().unwrap())
                    .as_node()
                    .unwrap();
                let then = self
                    .analyze_element(tree, iter.next().unwrap())
                    .as_node()
                    .unwrap();
                let else_ = self
                    .analyze_element(tree, iter.next().unwrap())
                    .as_node()
                    .unwrap();

                let cond = cond.get_from_builder(&self.builder);
                let type_cond = cond.data().as_ref().unwrap().type_();
                if !type_cond.can_be_bool() {
                    self.errors.push(Error::error(
                        ErrorKind::TypeError {
                            type_: type_cond.clone(),
                        },
                        cond.span(),
                        self.source,
                    ))
                }

                let type_then = then
                    .get_from_builder(&self.builder)
                    .data()
                    .as_ref()
                    .unwrap()
                    .type_()
                    .clone();

                let else_ = else_.get_from_builder(&self.builder);
                let type_else = else_.data().as_ref().unwrap().type_();
                if *type_else != type_then {
                    self.errors.push(Error::error(
                        ErrorKind::TypeError {
                            type_: type_else.clone(),
                        },
                        else_.span(),
                        self.source,
                    ))
                }

                self.builder.finish_node(node.span().end, |id| {
                    Some(NodeType::Value(Value {
                        value: None,
                        syntax: TreeElement::Node(id),
                        type_: type_then,
                    }))
                })
            }
            Value => {
                self.builder.start_node(node.kind(), node.span().start);
                let type_ = match self
                    .analyze_element(tree, node.children_with_leaves(tree).next().unwrap())
                    .get_from_builder(&self.builder)
                {
                    TreeElement::Leaf(leaf) => match leaf.data().as_ref().unwrap() {
                        LeafType::Value(Value { type_, .. }) => type_,
                        _ => unreachable!(),
                    },
                    TreeElement::Node(node) => node.data().as_ref().unwrap().type_(),
                }
                .clone();
                self.builder.finish_node(node.span().end, |id| {
                    Some(NodeType::Value(Value {
                        value: None,
                        syntax: TreeElement::Node(id),
                        type_,
                    }))
                })
            }
            Error => {
                self.builder.start_node(node.kind(), node.span().start);
                for element in node.children_with_leaves(tree) {
                    self.analyze_element(tree, element);
                }
                self.builder.finish_node(node.span().end, |id| {
                    Some(NodeType::Value(Value {
                        value: None,
                        syntax: TreeElement::Node(id),
                        type_: ValueType::Unknown,
                    }))
                })
            }
            s => todo!("{s}"),
        }
    }

    fn analyze_leaf(&mut self, _tree: &ParseTree, leaf: &Leaf<SyntaxKind>) -> LeafId {
        match leaf.kind() {
            Add | Mul | Div | Sub | Mod | Inc | Dec | And | Or | Not | Shl | Shr | Equal
            | NotEqual | Xor | LessThan | LessEqual | GreaterThan | GreaterEqual => {
                self.builder.push(leaf.kind(), leaf.span(), |_| None)
            }
            Number => self.builder.push(leaf.kind(), leaf.span(), |id| {
                Some(LeafType::Value(Value {
                    value: Some(ValueData::Number(self.source[leaf.span()].parse().unwrap())),
                    syntax: TreeElement::Leaf(id),
                    type_: ValueType::Number,
                }))
            }),
            SemiColon => self.builder.push(leaf.kind(), leaf.span(), |id| {
                Some(LeafType::Value(Value {
                    value: Some(ValueData::None),
                    syntax: TreeElement::Leaf(id),
                    type_: ValueType::None,
                }))
            }),
            Error => self.builder.push(leaf.kind(), leaf.span(), |id| {
                Some(LeafType::Value(Value {
                    value: None,
                    syntax: TreeElement::Leaf(id),
                    type_: ValueType::Unknown,
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
                self.builder.push(leaf.kind(), leaf.span(), |id| {
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
                self.builder.push(leaf.kind(), leaf.span(), move |id| {
                    Some(LeafType::Value(Value {
                        value: Some(ValueData::String(new)),
                        syntax: TreeElement::Leaf(id),
                        type_: ValueType::Pointer(Box::new(ValueType::Number)),
                    }))
                })
            }
            s => todo!("{s}"),
        }
    }
}
