use std::collections::HashMap;

use crate::{
    error::{Error, ErrorKind},
    parser::syntax::SyntaxKind,
    tree::{Leaf, LeafId, Node, NodeId, TreeElement},
};

use super::{
    value::{LeafData, LeafKind, MaybeTyped, NodeData, NodeKind, Value, ValueType, BUILT_INS},
    AnalysisResult, Analyzed, AnalyzedTree, AnalyzedTreeBuilder,
};

use SyntaxKind::*;

#[derive(Debug)]
pub struct TypeChecker<'a> {
    source: &'a str,
    errors: Vec<Error<'a>>,
    lookup: Vec<HashMap<&'a str, usize>>,
    current_scope: usize,
    memory: Vec<Value>,
    tree: Option<AnalyzedTree>,
    builder: AnalyzedTreeBuilder,
}

impl<'a> TypeChecker<'a> {
    pub fn new(source: &'a str, result: AnalysisResult<'a>) -> Self {
        TypeChecker {
            source,
            errors: result.errors,
            lookup: result.analyzed.lookup,
            current_scope: 0,
            memory: result.analyzed.memory,
            tree: Some(result.analyzed.tree),
            builder: AnalyzedTreeBuilder::new(),
        }
    }

    pub fn analyze(mut self) -> AnalysisResult<'a> {
        let tree = &self.tree.take().unwrap();
        let root = tree.node(AnalyzedTree::ROOT);
        self.builder.start_node(root.kind(), root.span().start);
        for &child in root.children() {
            self.analyze_node(tree, tree.node(child));
        }
        self.builder.finish_node(root.span().end, |_| None);
        let tree = self.builder.finish();

        AnalysisResult {
            errors: self.errors,
            analyzed: Analyzed {
                tree,
                memory: self.memory,
                lookup: self.lookup,
            },
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

    fn analyze_element(
        &mut self,
        tree: &AnalyzedTree,
        element: TreeElement<NodeId, LeafId>,
    ) -> TreeElement<NodeId, LeafId> {
        match element {
            TreeElement::Node(id) => TreeElement::Node(self.analyze_node(tree, tree.node(id))),
            TreeElement::Leaf(id) => TreeElement::Leaf(self.analyze_leaf(tree, tree.leaf(id))),
        }
    }

    fn analyze_node(&mut self, tree: &AnalyzedTree, node: &Node<NodeData>) -> NodeId {
        match node.kind() {
            Statement => {
                self.builder.start_node(node.kind(), node.span().start);
                for &child in node.children() {
                    self.analyze_node(tree, tree.node(child));
                }
                self.builder.finish_node(node.span().end, |_| None)
            }
            Scope => {
                self.builder.start_node(node.kind(), node.span().start);
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
                let a = iter.next().unwrap();
                let op = iter.next().unwrap();
                let b = iter.next().unwrap();
                let a = self.analyze_element(tree, a).into_node().unwrap();
                let op = self.analyze_element(tree, op).into_leaf().unwrap();
                let op = self.builder.leaf(op).kind();
                let b = self.analyze_element(tree, b).into_node().unwrap();
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
                self.builder.finish_node(node.span().end, |id| {
                    Some(NodeData::new(NodeKind::Value(Value {
                        value: None,
                        syntax: TreeElement::Node(id),
                        type_,
                    })))
                })
            }
            UnaryOp => {
                self.builder.start_node(node.kind(), node.span().start);
                let mut iter = node.children_with_leaves(tree);
                let op = iter.next().unwrap();
                let a = iter.next().unwrap();
                let op = self
                    .analyze_element(tree, op)
                    .get_from_builder(&self.builder)
                    .kind();
                let a = self
                    .analyze_element(tree, a)
                    .into_node()
                    .unwrap()
                    .get_from_builder(&self.builder);
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
                self.builder.finish_node(node.span().end, |id| {
                    Some(NodeData::new(NodeKind::Value(Value {
                        value: None,
                        syntax: TreeElement::Node(id),
                        type_,
                    })))
                })
            }
            Cast => {
                self.builder.start_node(node.kind(), node.span().start);
                let mut iter = node.children_with_leaves(tree);
                let a = iter.next().unwrap();
                let b = iter.next().unwrap();
                let type_ = self
                    .analyze_element(tree, a)
                    .into_node()
                    .unwrap()
                    .get_from_builder(&self.builder)
                    .data()
                    .as_ref()
                    .unwrap()
                    .type_()
                    .clone();
                self.analyze_element(tree, b);
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
                self.builder.push(ident.kind(), ident.span(), |_| None);
                self.analyze_element(tree, iter.next().unwrap());
                self.builder.finish_node(node.span().end, |_| None)
            }
            ReLet => {
                self.builder.start_node(node.kind(), node.span().start);
                let mut iter = node.children_with_leaves(tree);
                let lhs = iter.next().unwrap();
                let op = iter.next().unwrap();
                let rhs = iter.next().unwrap();
                let lhs = self.analyze_element(tree, lhs).into_node().unwrap();
                let op = self
                    .analyze_element(tree, op)
                    .into_leaf()
                    .unwrap()
                    .get_from_builder(&self.builder)
                    .kind()
                    .op_assignment();
                let rhs = self.analyze_element(tree, rhs).into_node().unwrap();
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
                self.builder.finish_node(node.span().start, |id| {
                    Some(NodeData::new(NodeKind::Value(Value {
                        value: None,
                        syntax: TreeElement::Node(id),
                        type_,
                    })))
                })
            }
            Loop => {
                self.builder.start_node(node.kind(), node.span().start);
                let mut iter = node.children_with_leaves(tree);
                let a = iter.next().unwrap();
                let b = iter.next().unwrap();
                let c = iter.next().unwrap();
                let d = iter.next().unwrap();
                self.analyze_element(tree, a);
                let cond = self.analyze_element(tree, b);
                let cond = cond.get_from_builder(&self.builder);
                let type_ = cond.type_().type_();
                if let Some(a) = type_ {
                    if !a.can_be_bool() {
                        self.errors.push(Error::error(
                            ErrorKind::TypeError { type_: a.clone() },
                            cond.span(),
                            self.source,
                        ))
                    }
                }
                self.analyze_element(tree, c);
                self.analyze_element(tree, d);
                self.builder.finish_node(node.span().end, |_| None)
            }
            If => {
                self.builder.start_node(node.kind(), node.span().start);
                let mut iter = node.children_with_leaves(tree);
                let a = iter.next().unwrap();
                let b = iter.next().unwrap();
                let c = iter.next();
                let cond = self.analyze_element(tree, a).into_node().unwrap();
                self.analyze_element(tree, b);
                c.map(|e| self.analyze_element(tree, e));
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
                self.builder.finish_node(node.span().end, |_| None)
            }
            Ternary => {
                self.builder.start_node(node.kind(), node.span().start);
                let mut iter = node.children_with_leaves(tree);
                let a = iter.next().unwrap();
                let b = iter.next().unwrap();
                let c = iter.next().unwrap();
                let cond = self.analyze_element(tree, a).into_node().unwrap();
                let then = self.analyze_element(tree, b).into_node().unwrap();
                let else_ = self.analyze_element(tree, c).into_node().unwrap();

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

                self.builder.finish_node(node.span().end, |id| {
                    Some(NodeData::new(NodeKind::Value(Value {
                        value: None,
                        syntax: TreeElement::Node(id),
                        type_: type_then,
                    })))
                })
            }
            Call => {
                self.builder.start_node(node.kind(), node.span().start);
                let mut iter = node.children_with_leaves(tree);
                let f = self.analyze_element(tree, iter.next().unwrap());
                let args = iter
                    .map(|v| self.analyze_element(tree, v))
                    .collect::<Vec<_>>();
                let f = f.get_from_builder(&self.builder);
                let type_ = if let MaybeTyped::Typed(ValueType::FnPtr(v)) = f.type_() {
                    if args.len() != v.len() - 1 {
                        self.errors.push(Error::error(
                            ErrorKind::TooManyArgs {
                                expected: v.len() - 1,
                                found: args.len(),
                            },
                            node.span(),
                            self.source,
                        ))
                    }
                    for (arg, t) in args.into_iter().zip(v[..v.len() - 1].iter()) {
                        let arg = arg.get_from_builder(&self.builder);
                        let arg_t = arg.type_().type_().unwrap();
                        if arg_t != t && *arg_t != ValueType::Poisoned {
                            self.errors.push(Error::error(
                                ErrorKind::WrongType {
                                    expected: t.clone(),
                                    found: arg_t.clone(),
                                },
                                arg.span(),
                                self.source,
                            ));
                        }
                    }
                    MaybeTyped::Typed(v.last().cloned().unwrap())
                } else {
                    self.errors
                        .push(Error::error(ErrorKind::NotCallable, f.span(), self.source));
                    MaybeTyped::Typed(ValueType::Poisoned)
                };
                self.builder.finish_node(node.span().end, |id| {
                    Some(NodeData::new(NodeKind::Value(Value {
                        value: None,
                        syntax: TreeElement::Node(id),
                        type_,
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
                    .get_from_builder(&self.builder)
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

    fn analyze_leaf(&mut self, _tree: &AnalyzedTree, leaf: &Leaf<LeafData>) -> LeafId {
        let mut data = leaf.data().clone();
        let t = if leaf.kind() == Identifier {
            Some(
                match self
                    .get(&self.source[leaf.span()])
                    .and_then(|v| v.type_.type_().cloned())
                    .or_else(|| {
                        BUILT_INS
                            .get(&self.source[leaf.span()])
                            .map(|b| b.value_type())
                    })
                    .map(MaybeTyped::Typed)
                {
                    Some(v) => v,
                    None => {
                        self.errors.push(Error::error(
                            ErrorKind::UndefinedReference,
                            leaf.span(),
                            self.source,
                        ));
                        MaybeTyped::Typed(ValueType::Poisoned)
                    }
                },
            )
        } else {
            None
        };
        self.builder.push(leaf.kind(), leaf.span(), |id| {
            if let Some(t) = t {
                if let Some(LeafKind::Value(v)) = data.as_mut().map(|v| &mut v.kind) {
                    v.type_ = t;
                    v.syntax = TreeElement::Leaf(id);
                }
            }
            data
        })
    }
}
