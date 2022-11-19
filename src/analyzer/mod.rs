pub mod value;

use std::collections::HashMap;

use crate::error::Error;

use crate::parser::syntax::{Syntax, SyntaxKind};
use SyntaxKind::*;

use crate::tree::{Node, Result, Tree, TreeBuilder};

use self::value::{Leaf, Value};

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

    pub fn analyze(mut self, tree: &Tree<Syntax>) -> Result<'a, Leaf> {
        let root = tree.node(tree.root());
        for &child in root.children() {
            self.analyze_node(tree, tree.node(child));
        }

        Result {
            errors: self.errors,
            output: self.builder.finish(),
        }
    }

    fn analyze_node(&mut self, tree: &Tree<Syntax>, node: &Node) {
        match node.kind() {
            Statement => {
                for &child in node.children() {
                    self.analyze_node(tree, tree.node(child));
                }
            }
            BinaryOp => {
                todo!()
            }
            s => unreachable!("{s}"),
        }
    }
}
