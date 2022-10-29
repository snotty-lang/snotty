pub mod tree;

use rowan::NodeOrToken;
use std::collections::HashMap;

use crate::error::Error;

use crate::parser::syntax::{SyntaxKind, SyntaxNode, SyntaxToken};
use SyntaxKind::*;

use tree::Analysis;

use self::tree::{AnalyzedTreeBuilder, Leaf};

pub struct Analyzer<'a> {
    errors: Vec<Error<'a>>,
    table: Vec<HashMap<&'a str, ()>>,
    builder: AnalyzedTreeBuilder,
}

impl<'a> Analyzer<'a> {
    pub fn new() -> Self {
        Analyzer {
            errors: Vec::new(),
            table: Vec::new(),
            builder: AnalyzedTreeBuilder::new(),
        }
    }

    pub fn analyze(mut self, syntax_node: &SyntaxNode) -> Analysis<'a> {
        self.analyze_inner(syntax_node);
        Analysis {
            errors: self.errors,
            analyzed: self.builder.finish(),
        }
    }

    fn analyze_inner(&mut self, node: &SyntaxNode) {
        for node in node.children_with_tokens() {
            match node {
                NodeOrToken::Node(n) => self.deal_with_node(n),
                NodeOrToken::Token(t) => {
                    if let Some(leaf) = self.deal_with_token(t) {
                        self.builder.push(leaf);
                    }
                }
            }
        }
    }

    fn deal_with_node(&mut self, node: SyntaxNode) {
        match node.kind() {
            Kind => {
                self.builder.start_node(node.clone());
                self.analyze_inner(&node);
                self.builder.finish_node();
            }
            _ => todo!(),
        }
    }

    fn deal_with_token(&mut self, token: SyntaxToken) -> Option<Leaf> {
        match token.kind() {
            Whitespace | Comment => None,
            Error => None,
            Mul | Div | Mod | Add | Sub | And | Or | Xor | Not | LessEqual | LessThan
            | GreaterEqual | GreaterThan | Equal | NotEqual | Shl | Shr | Inc | Dec => {
                Some(Leaf::Operator(token))
            }
            _ => unreachable!(),
        }
    }
}
