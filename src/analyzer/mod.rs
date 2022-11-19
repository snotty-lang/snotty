pub mod value;

use std::collections::HashMap;

use crate::error::Error;

use crate::parser::syntax::{ParseTree, SyntaxKind};
use SyntaxKind::*;

use crate::tree::{Leaf, LeafId, Node, NodeId, Result, TreeBuilder, TreeElement};

use self::value::{LeafType, Value, ValueKind, ValueType};

#[derive(Debug, Default)]
pub struct Analyzer<'a> {
    source: &'a str,
    errors: Vec<Error<'a>>,
    lookup: Vec<HashMap<&'a str, usize>>,
    memory: Vec<Value>,
    builder: TreeBuilder<(), LeafType>,
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

    pub fn analyze(mut self, tree: &ParseTree) -> Result<'a, (), LeafType> {
        let root = tree.node(tree.root());
        for child in root.children_with_leaves(tree) {
            self.analyze_element(tree, child);
        }

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
            TreeElement::Node(node) => TreeElement::Node(self.analyze_node(tree, tree.node(node))),
            TreeElement::Leaf(leaf) => TreeElement::Leaf(self.analyze_leaf(tree.leaf(leaf))),
        }
    }

    fn analyze_node(&mut self, tree: &ParseTree, node: &Node<()>) -> NodeId {
        match node.kind() {
            Statement => {
                self.builder.start_node(node.kind(), node.span().start);
                for &child in node.children() {
                    self.analyze_node(tree, tree.node(child));
                }
                self.builder.finish_node(node.span().end, None)
            }
            BinaryOp => {
                self.builder.start_node(node.kind(), node.span().start);
                let mut iter = node.children_with_leaves(tree);
                let a = self.analyze_element(tree, iter.next().unwrap()).get(tree);
                let op = self.analyze_element(tree, iter.next().unwrap()).get(tree);
                let b = self.analyze_element(tree, iter.next().unwrap()).get(tree);
                todo!("{a:?} {op:?} {b:?}")
            }
            Value => {
                self.builder.start_node(node.kind(), node.span().start);
                self.analyze_element(tree, node.children_with_leaves(tree).next().unwrap());
                self.builder.finish_node(node.span().end, None)
            }
            s => unreachable!("{s}"),
        }
    }

    fn analyze_leaf(&mut self, leaf: &Leaf<SyntaxKind>) -> LeafId {
        match leaf.kind() {
            Add | Mul | Div | Sub | Mod | Inc | Dec | And | Or | Not | Shl | Shr | Equal
            | NotEqual | Xor | LessThan | LessEqual | GreaterThan | GreaterEqual => self
                .builder
                .push(leaf.kind(), leaf.span(), Some(LeafType::Operator)),
            Number => self.builder.push(
                leaf.kind(),
                leaf.span(),
                Some(LeafType::Value(Value {
                    value: ValueKind::Number(self.source[leaf.span()].parse().unwrap()),
                    syntax: leaf.clone(),
                    type_: ValueType::Number,
                })),
            ),
            s => unreachable!("{s}"),
        }
    }
}
