pub mod value;

use std::collections::HashMap;
use std::num::IntErrorKind;

use crate::error::{Error, ErrorKind};
use crate::parser::syntax::{ParseTree, SyntaxKind};
use crate::tree::{Leaf, LeafId, Node, NodeId, Tree, TreeBuilder, TreeElement};

use value::{LeafData, LeafKind, NodeData, NodeKind, Value, ValueData, ValueType};
use SyntaxKind::*;

use self::value::MaybeTyped;

pub struct AnalysisResult<'a> {
    pub errors: Vec<Error<'a>>,
    pub analyzed: Analyzed,
}

pub struct Analyzed {
    pub tree: Tree<NodeData, LeafData>,
}

#[derive(Debug, Default)]
pub struct Analyzer<'a> {
    source: &'a str,
    errors: Vec<Error<'a>>,
    lookup: Vec<HashMap<&'a str, usize>>,
    current_scope: usize,
    memory: Vec<Value>,
    builder: TreeBuilder<NodeData, LeafData>,
}

impl<'a> Analyzer<'a> {
    pub fn new(source: &'a str) -> Self {
        Analyzer {
            source,
            errors: Vec::new(),
            lookup: vec![HashMap::new()],
            current_scope: 0,
            memory: Vec::new(),
            builder: TreeBuilder::new(),
        }
    }

    #[inline]
    fn get(&self, ident: &'a str) -> Option<&Value> {
        self.lookup[..=self.current_scope]
            .iter()
            .rev()
            .find_map(|map| map.get(ident))
            .map(|&i| &self.memory[i])
    }

    #[inline]
    fn get_mut(&mut self, ident: &'a str) -> Option<&mut Value> {
        self.lookup[..=self.current_scope]
            .iter()
            .rev()
            .find_map(|map| map.get(ident))
            .map(|&i| &mut self.memory[i])
    }

    #[inline]
    fn get_loc(&self, ident: &'a str) -> Option<usize> {
        self.lookup[..=self.current_scope]
            .iter()
            .rev()
            .find_map(|map| map.get(ident))
            .copied()
    }

    #[inline]
    fn insert(&mut self, ident: &'a str, value: Value) {
        self.memory.push(value);
        self.lookup[self.current_scope].insert(ident, self.memory.len() - 1);
    }

    fn resolve_types(&mut self) {
        for i in 0..self.lookup.len() {
            self.current_scope = i;
            for &loc in self.lookup[i].clone().values() {
                self.resolve_type(loc);
            }
        }
    }

    fn resolve_type(&mut self, loc: usize) {
        match &self.memory[loc].type_ {
            MaybeTyped::UnTyped(_) => {
                let MaybeTyped::UnTyped(id) = std::mem::replace(&mut self.memory[loc].type_, MaybeTyped::InProgress) else {unreachable!()};
                self.memory[loc].type_ = MaybeTyped::Typed(self.compute_type(id));
            }
            MaybeTyped::InProgress => panic!("Loopoz"),
            MaybeTyped::Typed(_) => (),
        }
    }

    fn compute_type(&mut self, id: TreeElement<NodeId, LeafId>) -> ValueType {
        let ast = id.get_from_builder(&self.builder);
        match ast.kind() {
            Identifier => {
                let ident = &self.source[ast.span()];
                self.resolve_type(self.get_loc(ident).unwrap());
                self.get(ident).unwrap().type_.type_().unwrap().clone()
            }
            _ => unreachable!(),
        }
    }

    pub fn analyze(mut self, tree: &ParseTree) -> AnalysisResult<'a> {
        let root = tree.node(ParseTree::ROOT);
        self.builder.start_node(root.kind(), root.span().start);
        for &child in root.children() {
            self.analyze_node(tree, tree.node(child));
        }
        self.builder.finish_node(root.span().end, |_| None);
        self.resolve_types();
        let mut builder = std::mem::take(&mut self.builder);
        let root = builder.node(Tree::<NodeData, LeafData>::ROOT);
        for child in root.children().clone() {
            self.analyze_node2(
                unsafe { &mut *(&mut builder as *mut _) },
                builder.node_mut(child),
            );
        }

        AnalysisResult {
            errors: self.errors,
            analyzed: Analyzed {
                tree: self.builder.finish(),
            },
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
                self.analyze_element(tree, node.children_with_leaves(tree).next().unwrap())
                    .get_from_builder(&self.builder);
                self.builder.finish_node(node.span().end, |_| None)
            }
            Scope => {
                self.builder.start_node(node.kind(), node.span().start);
                self.lookup.push(HashMap::new());
                self.current_scope += 1;
                for &child in node.children() {
                    self.analyze_node(tree, tree.node(child));
                }
                self.current_scope -= 1;
                self.builder.finish_node(node.span().end, |_| None)
            }
            BinaryOp => {
                self.builder.start_node(node.kind(), node.span().start);
                let mut iter = node.children_with_leaves(tree);
                let a = self
                    .analyze_element(tree, iter.next().unwrap())
                    .into_node()
                    .unwrap();
                let op = self
                    .analyze_element(tree, iter.next().unwrap())
                    .into_leaf()
                    .unwrap();
                let op = self.builder.leaf(op).kind();
                let b = self
                    .analyze_element(tree, iter.next().unwrap())
                    .into_node()
                    .unwrap();
                let a = self.builder.node(a);
                let b = self.builder.node(b);
                let type_a = a.data().as_ref().unwrap().type_();
                let type_b = b.data().as_ref().unwrap().type_();
                let type_ = if let (Some(a), Some(b)) = (type_a.type_(), type_b.type_()) {
                    Some(MaybeTyped::Typed(
                        a.operate_binary(op, b).unwrap_or(ValueType::Poisoned),
                    ))
                } else {
                    None
                };
                self.builder.finish_node(node.span().end, |id| {
                    Some(NodeData::new(NodeKind::Value(Value {
                        value: None,
                        syntax: TreeElement::Node(id),
                        type_: type_
                            .unwrap_or_else(|| MaybeTyped::UnTyped(TreeElement::Node(node.id()))),
                    })))
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
                let type_ = if let Some(a) = type_a.type_() {
                    Some(MaybeTyped::Typed(
                        a.operate_unary(op).unwrap_or(ValueType::Poisoned),
                    ))
                } else {
                    None
                };

                self.builder.finish_node(node.span().end, |id| {
                    Some(
                        NodeData::new(NodeKind::Value(Value {
                            value: None,
                            syntax: TreeElement::Node(id),
                            type_: type_.unwrap_or_else(|| {
                                MaybeTyped::UnTyped(TreeElement::Node(node.id()))
                            }),
                        }))
                        .assignable(op == SyntaxKind::Mul),
                    )
                })
            }
            Cast => {
                self.builder.start_node(node.kind(), node.span().start);
                let mut iter = node.children_with_leaves(tree);
                let type_ = self
                    .analyze_element(tree, iter.next().unwrap())
                    .into_node()
                    .unwrap()
                    .get_from_builder(&self.builder)
                    .data()
                    .as_ref()
                    .unwrap()
                    .type_()
                    .clone();
                self.analyze_element(tree, iter.next().unwrap());
                self.builder.finish_node(node.span().end, |id| {
                    Some(NodeData::new(NodeKind::Value(Value {
                        value: None,
                        syntax: TreeElement::Node(id),
                        type_,
                    })))
                })
            }
            Let => {
                self.builder.start_node(node.kind(), node.span().start);
                let mut iter = node.children_with_leaves(tree);
                let ident = iter.next().unwrap().into_leaf().unwrap().get(tree);
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
            ReLet => {
                self.builder.start_node(node.kind(), node.span().start);
                let mut iter = node.children_with_leaves(tree);
                let lhs = self
                    .analyze_element(tree, iter.next().unwrap())
                    .into_node()
                    .unwrap();
                iter.next()
                    .unwrap()
                    .into_leaf()
                    .unwrap()
                    .get(tree)
                    .kind()
                    .op_assignment();
                self.analyze_element(tree, iter.next().unwrap())
                    .into_node()
                    .unwrap();
                let lhs = lhs.get_from_builder(&self.builder);
                let data_lhs = lhs.data().as_ref().unwrap();
                if !data_lhs.assignable {
                    self.errors
                        .push(Error::error(ErrorKind::InvalidLHS, lhs.span(), self.source));
                }
                self.builder.finish_node(node.span().end, |_| None)
            }
            Pointer => {
                self.builder.start_node(node.kind(), node.span().start);
                let type_ = self
                    .analyze_element(tree, node.children_with_leaves(tree).next().unwrap())
                    .get_from_builder(&self.builder)
                    .type_()
                    .clone()
                    .map(|v| ValueType::Pointer(Box::new(v)));
                self.builder.finish_node(node.span().end, |id| {
                    Some(NodeData::new(NodeKind::Value(Value {
                        value: None,
                        syntax: TreeElement::Node(id),
                        type_,
                    })))
                })
            }
            Loop => {
                self.builder.start_node(node.kind(), node.span().start);
                let mut elements = node.children_with_leaves(tree).collect::<Vec<_>>();
                self.analyze_element(tree, elements.pop().unwrap());
                let mut iter = elements.split(|s| {
                    s.into_leaf()
                        .map(|s| s.get(tree).kind() == SemiColon)
                        .unwrap_or(false)
                });
                if let Some(&[e]) = iter.next() {
                    self.analyze_element(tree, e);
                } else {
                    self.builder.push(Stuffing, 0..0, |id| {
                        Some(LeafData::new(LeafKind::Value(Value {
                            value: None,
                            syntax: TreeElement::Leaf(id),
                            type_: MaybeTyped::Typed(ValueType::None),
                        })))
                    });
                }

                if let Some(&[e]) = iter.next() {
                    self.analyze_element(tree, e).into_node().unwrap();
                } else {
                    self.builder.push(Stuffing, 0..0, |id| {
                        Some(LeafData::new(LeafKind::Value(Value {
                            value: None,
                            syntax: TreeElement::Leaf(id),
                            type_: MaybeTyped::Typed(ValueType::Number),
                        })))
                    });
                }

                if let Some(&[e]) = iter.next() {
                    self.analyze_element(tree, e);
                } else {
                    self.builder.push(Stuffing, 0..0, |id| {
                        Some(LeafData::new(LeafKind::Value(Value {
                            value: None,
                            syntax: TreeElement::Leaf(id),
                            type_: MaybeTyped::Typed(ValueType::None),
                        })))
                    });
                }
                self.builder.finish_node(node.span().end, |_| None)
            }
            If => {
                self.builder.start_node(node.kind(), node.span().start);
                let mut iter = node.children_with_leaves(tree);
                self.analyze_element(tree, iter.next().unwrap())
                    .into_node()
                    .unwrap();
                self.analyze_element(tree, iter.next().unwrap());
                iter.next().map(|e| self.analyze_element(tree, e));
                self.builder.finish_node(node.span().end, |_| None)
            }
            Ternary => {
                self.builder.start_node(node.kind(), node.span().start);
                let mut iter = node.children_with_leaves(tree);
                self.analyze_element(tree, iter.next().unwrap())
                    .into_node()
                    .unwrap();
                let then = self
                    .analyze_element(tree, iter.next().unwrap())
                    .into_node()
                    .unwrap();
                self.analyze_element(tree, iter.next().unwrap())
                    .into_node()
                    .unwrap();

                let type_then = then
                    .get_from_builder(&self.builder)
                    .data()
                    .as_ref()
                    .unwrap()
                    .type_()
                    .clone();
                self.builder.finish_node(node.span().end, |id| {
                    Some(NodeData::new(NodeKind::Value(Value {
                        value: None,
                        syntax: TreeElement::Node(id),
                        type_: type_then,
                    })))
                })
            }
            Value => {
                self.builder.start_node(node.kind(), node.span().start);
                let a = self
                    .analyze_element(tree, node.children_with_leaves(tree).next().unwrap())
                    .get_from_builder(&self.builder);

                let type_ = a.type_().clone();
                let assignable = a.assignable();

                self.builder.finish_node(node.span().end, |id| {
                    Some(
                        NodeData::new(NodeKind::Value(Value {
                            value: None,
                            syntax: TreeElement::Node(id),
                            type_,
                        }))
                        .assignable(assignable),
                    )
                })
            }
            Kind => {
                self.builder.start_node(node.kind(), node.span().start);
                let type_ = match node
                    .children_with_leaves(tree)
                    .next()
                    .unwrap()
                    .into_leaf()
                    .unwrap()
                    .get(tree)
                    .kind()
                {
                    ByteKw => MaybeTyped::Typed(ValueType::Number),
                    SemiColon => MaybeTyped::Typed(ValueType::None),
                    Identifier => todo!(),
                    _ => unreachable!(),
                };
                self.builder.finish_node(node.span().end, |id| {
                    Some(NodeData::new(NodeKind::Value(Value {
                        value: None,
                        syntax: TreeElement::Node(id),
                        type_,
                    })))
                })
            }
            Error => {
                self.builder.start_node(node.kind(), node.span().start);
                for element in node.children_with_leaves(tree) {
                    self.analyze_element(tree, element);
                }
                self.builder.finish_node(node.span().end, |id| {
                    Some(NodeData::new(NodeKind::Value(Value {
                        value: None,
                        syntax: TreeElement::Node(id),
                        type_: MaybeTyped::Typed(ValueType::Poisoned),
                    })))
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
            AddAssign | SubAssign | MulAssign | DivAssign | ModAssign | AndAssign | OrAssign
            | XorAssign | ShlAssign | ShrAssign => {
                self.builder
                    .push(leaf.kind().op_assignment().unwrap(), leaf.span(), |_| None)
            }
            Number => self.builder.push(leaf.kind(), leaf.span(), |id| {
                Some(LeafData::new(LeafKind::Value(Value {
                    value: Some(ValueData::Number(self.source[leaf.span()].parse().unwrap())),
                    syntax: TreeElement::Leaf(id),
                    type_: MaybeTyped::Typed(ValueType::Number),
                })))
            }),
            SemiColon => self.builder.push(leaf.kind(), leaf.span(), |id| {
                Some(LeafData::new(LeafKind::Value(Value {
                    value: Some(ValueData::None),
                    syntax: TreeElement::Leaf(id),
                    type_: MaybeTyped::Typed(ValueType::None),
                })))
            }),
            InKw => self.builder.push(leaf.kind(), leaf.span(), |id| {
                Some(LeafData::new(LeafKind::Value(Value {
                    value: None,
                    syntax: TreeElement::Leaf(id),
                    type_: MaybeTyped::Typed(ValueType::Number),
                })))
            }),
            Error => self.builder.push(leaf.kind(), leaf.span(), |id| {
                Some(LeafData::new(LeafKind::Value(Value {
                    value: None,
                    syntax: TreeElement::Leaf(id),
                    type_: MaybeTyped::Typed(ValueType::Poisoned),
                })))
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
                    Some(LeafData::new(LeafKind::Value(Value {
                        value: Some(ValueData::Char(c)),
                        syntax: TreeElement::Leaf(id),
                        type_: MaybeTyped::Typed(ValueType::Number),
                    })))
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
                    Some(LeafData::new(LeafKind::Value(Value {
                        value: Some(ValueData::String(new)),
                        syntax: TreeElement::Leaf(id),
                        type_: MaybeTyped::Typed(ValueType::Pointer(Box::new(ValueType::Number))),
                    })))
                })
            }
            Identifier => {
                let type_ = self.get(&self.source[leaf.span()]).map(|v| v.type_.clone());
                self.builder.push(leaf.kind(), leaf.span(), |id| {
                    Some(
                        LeafData::new(LeafKind::Value(Value {
                            value: None,
                            syntax: TreeElement::Leaf(id),
                            type_: type_.unwrap_or_else(|| {
                                MaybeTyped::UnTyped(TreeElement::Leaf(leaf.id()))
                            }),
                        }))
                        .assignable(true),
                    )
                })
            }
            s => unreachable!("{s}"),
        }
    }

    fn analyze_element2(
        &mut self,
        builder: &mut TreeBuilder<NodeData, LeafData>,
        element: TreeElement<NodeId, LeafId>,
    ) -> TreeElement<NodeId, LeafId> {
        match element {
            TreeElement::Node(id) => TreeElement::Node(
                self.analyze_node2(unsafe { &mut *(builder as *mut _) }, builder.node_mut(id)),
            ),
            TreeElement::Leaf(id) => TreeElement::Leaf(
                self.analyze_leaf2(unsafe { &mut *(builder as *mut _) }, builder.leaf_mut(id)),
            ),
        }
    }

    fn analyze_node2(
        &mut self,
        builder: &mut TreeBuilder<NodeData, LeafData>,
        node: &mut Node<NodeData>,
    ) -> NodeId {
        match node.kind() {
            Statement => {
                for &child in node.children() {
                    self.analyze_node2(
                        unsafe { &mut *(builder as *mut _) },
                        builder.node_mut(child),
                    );
                }
            }
            OutKw => {
                let child = self
                    .analyze_element2(
                        builder,
                        node.children_with_leaves_builder(&self.builder)
                            .next()
                            .unwrap(),
                    )
                    .get_from_builder(&self.builder);
                if let Some(type_) = child.type_().type_() {
                    if !type_.can_be_displayed() {
                        self.errors.push(Error::error(
                            ErrorKind::TypeError {
                                type_: type_.clone(),
                            },
                            child.span(),
                            self.source,
                        ))
                    }
                }
            }
            Scope => {
                self.current_scope += 1;
                for &child in node.children() {
                    self.analyze_node2(
                        unsafe { &mut *(builder as *mut _) },
                        builder.node_mut(child),
                    );
                }
                self.current_scope -= 1;
            }
            BinaryOp => {
                let mut iter = node.children_with_leaves_builder(&self.builder);
                let a = iter.next().unwrap();
                let op = iter.next().unwrap();
                let b = iter.next().unwrap();
                let a = self.analyze_element2(builder, a).into_node().unwrap();
                let op = self.analyze_element2(builder, op).into_leaf().unwrap();
                let op = self.builder.leaf(op).kind();
                let b = self.analyze_element2(builder, b).into_node().unwrap();
                let a = self.builder.node(a);
                let b = self.builder.node(b);
                let type_a = a.data().as_ref().unwrap().type_();
                let type_b = b.data().as_ref().unwrap().type_();
                let type_ = if let (Some(a), Some(b)) = (type_a.type_(), type_b.type_()) {
                    MaybeTyped::Typed(match a.operate_binary(op, b) {
                        Some(t) => t,
                        None => {
                            self.errors.push(Error::error(
                                ErrorKind::UnsupportedOperation { operation: op },
                                node.span(),
                                self.source,
                            ));
                            ValueType::Poisoned
                        }
                    })
                } else {
                    unreachable!()
                };
                if let NodeKind::Value(v) = &mut node.data_mut().as_mut().unwrap().kind {
                    v.type_ = type_;
                }
            }
            UnaryOp => {
                let mut iter = node.children_with_leaves_builder(&self.builder);
                let a = iter.next().unwrap();
                let b = iter.next().unwrap();
                let a = self.analyze_element2(builder, a);
                let b = self.analyze_element2(builder, b);

                let (a, op) = match (a, b) {
                    (TreeElement::Node(a), TreeElement::Leaf(op))
                    | (TreeElement::Leaf(op), TreeElement::Node(a)) => (
                        a.get_from_builder(&self.builder),
                        op.get_from_builder(&self.builder).kind(),
                    ),
                    _ => unreachable!(),
                };

                let type_a = a.data().as_ref().unwrap().type_();
                let type_ = if let Some(a) = type_a.type_() {
                    MaybeTyped::Typed(match a.operate_unary(op) {
                        Some(t) => t,
                        None => {
                            self.errors.push(Error::error(
                                ErrorKind::UnsupportedOperation { operation: op },
                                node.span(),
                                self.source,
                            ));
                            ValueType::Poisoned
                        }
                    })
                } else {
                    unreachable!()
                };
                if let NodeKind::Value(v) = &mut node.data_mut().as_mut().unwrap().kind {
                    v.type_ = type_;
                }
            }
            Cast => {
                let mut iter = node.children_with_leaves_builder(&self.builder);
                let a = iter.next().unwrap();
                let b = iter.next().unwrap();
                let type_ = self
                    .analyze_element2(builder, a)
                    .into_node()
                    .unwrap()
                    .get_from_builder(&self.builder)
                    .data()
                    .as_ref()
                    .unwrap()
                    .type_()
                    .clone();
                self.analyze_element2(builder, b);
                if let NodeKind::Value(v) = &mut node.data_mut().as_mut().unwrap().kind {
                    v.type_ = type_;
                }
            }
            Let => {
                let mut iter = node.children_with_leaves_builder(&self.builder);
                self.analyze_element2(builder, iter.next().unwrap());
            }
            ReLet => {
                let mut iter = node.children_with_leaves_builder(&self.builder);
                let lhs = iter.next().unwrap();
                let op = iter.next().unwrap();
                let rhs = iter.next().unwrap();
                let lhs = self.analyze_element2(builder, lhs).into_node().unwrap();
                let op = op
                    .into_leaf()
                    .unwrap()
                    .get_from_builder(&self.builder)
                    .kind()
                    .op_assignment();
                let rhs = self.analyze_element2(builder, rhs).into_node().unwrap();
                let lhs = lhs.get_from_builder(&self.builder);
                let rhs = rhs.get_from_builder(&self.builder);
                let data_lhs = lhs.data().as_ref().unwrap();
                let type_lhs = data_lhs.type_();
                let type_rhs = rhs.data().as_ref().unwrap().type_();

                if let (Some(a), Some(b)) = (type_lhs.type_(), type_rhs.type_()) {
                    match op {
                        Some(op) => {
                            if a.operate_binary(op, b).is_none() {
                                self.errors.push(Error::error(
                                    ErrorKind::UnsupportedOperation { operation: op },
                                    node.span(),
                                    self.source,
                                ));
                            }
                        }
                        None => {
                            if a != b && ![a, b].contains(&&ValueType::Poisoned) {
                                self.errors.push(Error::error(
                                    ErrorKind::TypeError { type_: b.clone() },
                                    rhs.span(),
                                    self.source,
                                ));
                            }
                        }
                    }
                }
            }
            Pointer => {
                let type_ = self
                    .analyze_element2(
                        builder,
                        node.children_with_leaves_builder(&self.builder)
                            .next()
                            .unwrap(),
                    )
                    .get_from_builder(&self.builder)
                    .type_()
                    .clone()
                    .map(|v| ValueType::Pointer(Box::new(v)));
                if let NodeKind::Value(v) = &mut node.data_mut().as_mut().unwrap().kind {
                    v.type_ = type_;
                }
            }
            Loop => {
                let mut elements = node
                    .children_with_leaves_builder(&self.builder)
                    .collect::<Vec<_>>();
                self.analyze_element2(builder, elements.pop().unwrap());
                let mut iter = elements.split(|s| {
                    s.into_leaf()
                        .map(|s| s.get_from_builder(&self.builder).kind() == SemiColon)
                        .unwrap_or(false)
                });
                let a = iter.next();
                let b = iter.next();
                let c = iter.next();
                if let Some(&[e]) = a {
                    self.analyze_element2(builder, e);
                } else {
                    self.builder.push(Stuffing, 0..0, |id| {
                        Some(LeafData::new(LeafKind::Value(Value {
                            value: None,
                            syntax: TreeElement::Leaf(id),
                            type_: MaybeTyped::Typed(ValueType::None),
                        })))
                    });
                }

                if let Some(&[e]) = b {
                    let cond = self.analyze_element2(builder, e).into_node().unwrap();
                    let cond = cond.get_from_builder(&self.builder);
                    let type_ = cond.data().as_ref().unwrap().type_().type_();
                    if let Some(a) = type_ {
                        if !a.can_be_bool() {
                            self.errors.push(Error::error(
                                ErrorKind::TypeError { type_: a.clone() },
                                cond.span(),
                                self.source,
                            ))
                        }
                    }
                }

                if let Some(&[e]) = c {
                    self.analyze_element2(builder, e);
                }
            }
            If => {
                let mut iter = node.children_with_leaves_builder(&self.builder);
                let a = iter.next().unwrap();
                let b = iter.next().unwrap();
                let c = iter.next();
                let cond = self.analyze_element2(builder, a).into_node().unwrap();
                self.analyze_element2(builder, b);
                c.map(|e| self.analyze_element2(builder, e));
                let cond = cond.get_from_builder(&self.builder);
                let type_ = cond.data().as_ref().unwrap().type_();
                if let Some(a) = type_.type_() {
                    if !a.can_be_bool() {
                        self.errors.push(Error::error(
                            ErrorKind::TypeError { type_: a.clone() },
                            cond.span(),
                            self.source,
                        ))
                    }
                }
            }
            Ternary => {
                let mut iter = node.children_with_leaves_builder(&self.builder);
                let a = iter.next().unwrap();
                let b = iter.next().unwrap();
                let c = iter.next().unwrap();
                let cond = self.analyze_element2(builder, a).into_node().unwrap();
                let then = self.analyze_element2(builder, b).into_node().unwrap();
                let else_ = self.analyze_element2(builder, c).into_node().unwrap();

                let cond = cond.get_from_builder(&self.builder);
                let type_cond = cond.data().as_ref().unwrap().type_();
                if let Some(a) = type_cond.type_() {
                    if !a.can_be_bool() {
                        self.errors.push(Error::error(
                            ErrorKind::TypeError { type_: a.clone() },
                            cond.span(),
                            self.source,
                        ))
                    }
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
                if let (Some(a), Some(b)) = (type_then.type_(), type_else.type_()) {
                    if a != b {
                        self.errors.push(Error::error(
                            ErrorKind::TypeError { type_: b.clone() },
                            else_.span(),
                            self.source,
                        ))
                    }
                }

                if let NodeKind::Value(v) = &mut node.data_mut().as_mut().unwrap().kind {
                    v.type_ = type_then;
                }
            }
            Value => {
                let a = self
                    .analyze_element2(
                        builder,
                        node.children_with_leaves_builder(&self.builder)
                            .next()
                            .unwrap(),
                    )
                    .get_from_builder(&self.builder);

                let type_ = a.type_().clone();
                if let NodeKind::Value(v) = &mut node.data_mut().as_mut().unwrap().kind {
                    v.type_ = type_;
                }
            }
            Kind => {
                let type_ = match node
                    .children_with_leaves_builder(&self.builder)
                    .next()
                    .unwrap()
                    .into_leaf()
                    .unwrap()
                    .get_from_builder(&self.builder)
                    .kind()
                {
                    ByteKw => MaybeTyped::Typed(ValueType::Number),
                    SemiColon => MaybeTyped::Typed(ValueType::None),
                    Identifier => todo!(),
                    _ => unreachable!(),
                };
                if let NodeKind::Value(v) = &mut node.data_mut().as_mut().unwrap().kind {
                    v.type_ = type_;
                }
            }
            Error => (),
            s => todo!("{s}"),
        }
        node.id()
    }

    fn analyze_leaf2(
        &mut self,
        _builder: &mut TreeBuilder<NodeData, LeafData>,
        leaf: &mut Leaf<LeafData>,
    ) -> LeafId {
        match leaf.kind() {
            Identifier => {
                let v = self.get_mut(&self.source[leaf.span()]).unwrap();
                let t = v.type_.clone();
                if let LeafKind::Value(v) = &mut leaf.data_mut().as_mut().unwrap().kind {
                    v.type_ = t;
                }
            }
            _ => (),
        }
        leaf.id()
    }
}
