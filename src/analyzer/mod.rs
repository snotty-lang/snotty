pub mod value;

use std::collections::HashMap;
use std::num::IntErrorKind;

use crate::error::{Error, ErrorKind};
use crate::parser::syntax::{ParseTree, SyntaxKind};
use crate::tree::{Leaf, LeafId, Node, NodeId, Tree, TreeBuilder, TreeElement};

use value::{LeafType, NodeType, Value, ValueData, ValueType};
use SyntaxKind::*;

pub struct AnalysisResult<'a> {
    pub errors: Vec<Error<'a>>,
    pub analyzed: Tree<NodeType, LeafType>,
}

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
            lookup: vec![HashMap::new()],
            memory: Vec::new(),
            builder: TreeBuilder::new(),
        }
    }

    #[inline]
    fn get(&self, ident: &'a str) -> Option<&Value> {
        self.lookup
            .iter()
            .rev()
            .find_map(|map| map.get(ident))
            .map(|&i| &self.memory[i])
    }

    #[inline]
    fn get_mut(&mut self, ident: &'a str) -> Option<&mut Value> {
        self.lookup
            .iter_mut()
            .rev()
            .find_map(|map| map.get_mut(ident))
            .map(|&mut i| &mut self.memory[i])
    }

    #[inline]
    fn insert(&mut self, ident: &'a str, value: Value) {
        self.memory.push(value);
        self.lookup
            .last_mut()
            .unwrap()
            .insert(ident, self.memory.len() - 1);
    }

    pub fn analyze(mut self, tree: &ParseTree) -> AnalysisResult<'a> {
        let root = tree.node(tree.root());
        self.builder.start_node(root.kind(), root.span().start);
        for child in root.children_with_leaves(tree) {
            self.analyze_element(tree, child);
        }
        self.builder.finish_node(root.span().end, |_| None);

        AnalysisResult {
            errors: self.errors,
            analyzed: self.builder.finish(),
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
            Statement => {
                self.builder.start_node(node.kind(), node.span().start);
                for &child in node.children() {
                    self.analyze_node(tree, tree.node(child));
                }
                self.builder.finish_node(node.span().end, |_| None)
            }
            OutKw => {
                self.builder.start_node(node.kind(), node.span().start);
                let child = self
                    .analyze_element(tree, node.children_with_leaves(tree).next().unwrap())
                    .get_from_builder(&self.builder);
                let type_ = child.type_();
                if !type_.can_be_displayed() {
                    self.errors.push(Error::error(
                        ErrorKind::TypeError {
                            type_: type_.clone(),
                        },
                        child.span(),
                        self.source,
                    ))
                }
                self.builder.finish_node(node.span().end, |_| None)
            }
            Scope => {
                self.builder.start_node(node.kind(), node.span().start);
                self.lookup.push(HashMap::new());
                for &child in node.children() {
                    self.analyze_node(tree, tree.node(child));
                }
                self.lookup.pop();
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
                let a = self.builder.node(a);
                let b = self.builder.node(b);
                let type_a = a.data().as_ref().unwrap().type_();
                let type_b = b.data().as_ref().unwrap().type_();
                let type_ = if *type_a == ValueType::Unknown {
                    self.errors
                        .push(Error::error(ErrorKind::UnknownType, a.span(), self.source));
                    ValueType::Unknown
                } else if *type_b == ValueType::Unknown {
                    self.errors
                        .push(Error::error(ErrorKind::UnknownType, b.span(), self.source));
                    ValueType::Unknown
                } else {
                    match type_a.operate_binary(op, type_b) {
                        Some(t) => t,
                        None => {
                            self.errors.push(Error::error(
                                ErrorKind::UnsupportedOperation { operation: op },
                                node.span(),
                                self.source,
                            ));
                            ValueType::Posisoned
                        }
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
                let type_ = if *type_a == ValueType::Unknown {
                    self.errors
                        .push(Error::error(ErrorKind::UnknownType, a.span(), self.source));
                    ValueType::Unknown
                } else {
                    match type_a.operate_unary(op) {
                        Some(t) => t,
                        None => {
                            self.errors.push(Error::error(
                                ErrorKind::UnsupportedOperation { operation: op },
                                node.span(),
                                self.source,
                            ));
                            ValueType::Posisoned
                        }
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
            Cast => {
                self.builder.start_node(node.kind(), node.span().start);
                let mut iter = node.children_with_leaves(tree);
                let type_ = self
                    .analyze_element(tree, iter.next().unwrap())
                    .as_node()
                    .unwrap()
                    .get_from_builder(&self.builder)
                    .data()
                    .as_ref()
                    .unwrap()
                    .type_()
                    .clone();
                self.analyze_element(tree, iter.next().unwrap());
                self.builder.finish_node(node.span().end, |id| {
                    Some(NodeType::Value(Value {
                        value: None,
                        syntax: TreeElement::Node(id),
                        type_,
                    }))
                })
            }
            Let => {
                self.builder.start_node(node.kind(), node.span().start);
                let mut iter = node.children_with_leaves(tree);
                let ident = iter.next().unwrap().as_leaf().unwrap().get(tree);
                let id = self.builder.push(ident.kind(), ident.span(), |_| None);
                let ident = &self.source[ident.span()];
                let type_ = self
                    .analyze_element(tree, iter.next().unwrap())
                    .get_from_builder(&self.builder)
                    .type_()
                    .clone();
                self.insert(
                    ident,
                    Value {
                        value: None,
                        syntax: TreeElement::Leaf(id),
                        type_,
                    },
                );
                self.builder.finish_node(node.span().end, |_| None)
            }
            Pointer => {
                self.builder.start_node(node.kind(), node.span().start);
                let type_ = ValueType::Pointer(Box::new(
                    self.analyze_element(tree, node.children_with_leaves(tree).next().unwrap())
                        .get_from_builder(&self.builder)
                        .type_()
                        .clone(),
                ));
                self.builder.finish_node(node.span().end, |id| {
                    Some(NodeType::Value(Value {
                        value: None,
                        syntax: TreeElement::Node(id),
                        type_,
                    }))
                })
            }
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
                let type_ = self
                    .analyze_element(tree, node.children_with_leaves(tree).next().unwrap())
                    .get_from_builder(&self.builder)
                    .type_()
                    .clone();
                self.builder.finish_node(node.span().end, |id| {
                    Some(NodeType::Value(Value {
                        value: None,
                        syntax: TreeElement::Node(id),
                        type_,
                    }))
                })
            }
            Kind => {
                self.builder.start_node(node.kind(), node.span().start);
                let type_ = match node
                    .children_with_leaves(tree)
                    .next()
                    .unwrap()
                    .as_leaf()
                    .unwrap()
                    .get(tree)
                    .kind()
                {
                    ByteKw => ValueType::Number,
                    SemiColon => ValueType::None,
                    Identifier => todo!(),
                    _ => unreachable!(),
                };
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
                        type_: ValueType::Posisoned,
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
            InKw => self.builder.push(leaf.kind(), leaf.span(), |id| {
                Some(LeafType::Value(Value {
                    value: None,
                    syntax: TreeElement::Leaf(id),
                    type_: ValueType::Number,
                }))
            }),
            Error => self.builder.push(leaf.kind(), leaf.span(), |id| {
                Some(LeafType::Value(Value {
                    value: None,
                    syntax: TreeElement::Leaf(id),
                    type_: ValueType::Posisoned,
                }))
            }),
            Char => {
                let s = &self.source[leaf.span()];
                let mut chars = s[1..s.len() - 1].chars();
                let c = match chars.next().unwrap() {
                    '\\' => match chars.next().unwrap() {
                        'n' => b'\n',
                        't' => b'\t',
                        'r' => b'\r',
                        c @ ('\\' | '\'' | '\"') => c as u8,
                        'x' => u8::from_str_radix(&s[3..s.len() - 1], 16).unwrap_or_else(|err| {
                            match err.kind() {
                                IntErrorKind::PosOverflow => {
                                    self.errors.push(Error::error(
                                        ErrorKind::ByteOverflow,
                                        leaf.span(),
                                        self.source,
                                    ));
                                    0
                                }
                                _ => unreachable!(),
                            }
                        }),
                        '0'..='7' => u8::from_str_radix(&s[2..s.len() - 1], 8).unwrap_or_else(
                            |err| match err.kind() {
                                IntErrorKind::PosOverflow => {
                                    self.errors.push(Error::error(
                                        ErrorKind::ByteOverflow,
                                        leaf.span(),
                                        self.source,
                                    ));
                                    0
                                }
                                _ => unreachable!(),
                            },
                        ),
                        _ => unreachable!(),
                    },
                    c => c as u8,
                };
                self.builder.push(leaf.kind(), leaf.span(), |id| {
                    Some(LeafType::Value(Value {
                        value: Some(ValueData::Char(c)),
                        syntax: TreeElement::Leaf(id),
                        type_: ValueType::Number,
                    }))
                })
            }
            String => {
                let mut new = Vec::new();
                let s = &self.source[leaf.span()];
                let mut chars = s[1..s.len() - 1].chars().enumerate().peekable();
                while let Some((i, c)) = chars.next() {
                    new.push(match c {
                        '\\' => match chars.next().unwrap().1 {
                            'n' => b'\n',
                            't' => b'\t',
                            'r' => b'\r',
                            c @ ('\\' | '\'' | '\"') => c as u8,
                            'x' => {
                                let i = i + 3;
                                let mut j = i;
                                while matches!(chars.peek(), Some((_, c)) if c.is_ascii_hexdigit())
                                {
                                    j += 1;
                                    chars.next();
                                }
                                u8::from_str_radix(&s[i..j], 16).unwrap_or_else(|err| {
                                    match err.kind() {
                                        IntErrorKind::PosOverflow => {
                                            self.errors.push(Error::error(
                                                ErrorKind::ByteOverflow,
                                                leaf.span(),
                                                self.source,
                                            ));
                                            0
                                        }
                                        e => unreachable!("{e:?}"),
                                    }
                                })
                            }
                            '0'..='7' => {
                                let i = i + 2;
                                let mut j = i + 1;
                                while matches!(chars.peek(), Some((_, '0'..='7'))) {
                                    j += 1;
                                    chars.next();
                                }
                                u8::from_str_radix(&s[i..j], 8).unwrap_or_else(|err| {
                                    match err.kind() {
                                        IntErrorKind::PosOverflow => {
                                            self.errors.push(Error::error(
                                                ErrorKind::ByteOverflow,
                                                leaf.span(),
                                                self.source,
                                            ));
                                            0
                                        }
                                        _ => unreachable!(),
                                    }
                                })
                            }
                            _ => unreachable!(),
                        },
                        c => c as u8,
                    });
                }
                self.builder.push(leaf.kind(), leaf.span(), move |id| {
                    Some(LeafType::Value(Value {
                        value: Some(ValueData::String(new)),
                        syntax: TreeElement::Leaf(id),
                        type_: ValueType::Pointer(Box::new(ValueType::Number)),
                    }))
                })
            }
            Identifier => {
                let type_ = match self.get(&self.source[leaf.span()]) {
                    Some(v) => v.type_.clone(),
                    None => {
                        self.errors.push(Error::error(
                            ErrorKind::UndefinedReference,
                            leaf.span(),
                            self.source,
                        ));
                        ValueType::Posisoned
                    }
                };
                self.builder.push(leaf.kind(), leaf.span(), |id| {
                    Some(LeafType::Value(Value {
                        value: None,
                        syntax: TreeElement::Leaf(id),
                        type_,
                    }))
                })
            }
            s => unreachable!("{s}"),
        }
    }
}
