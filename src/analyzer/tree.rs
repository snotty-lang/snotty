use std::collections::VecDeque;

use crate::{
    error::Error,
    parser::syntax::{SyntaxKind, SyntaxNode, SyntaxToken},
    Span,
};

pub struct Analysis<'a> {
    pub errors: Vec<Error<'a>>,
    pub analyzed: AnalyzedTree,
}

#[derive(Default, Debug)]
pub struct AnalyzedTree {
    leaves: Vec<Leaf>,
    nodes: Vec<Node>,
}

#[derive(Default, Debug)]
pub struct AnalyzedTreeBuilder {
    leaves: Vec<Leaf>,
    nodes: Vec<Node>,
    current: Vec<usize>,
}

#[derive(Debug)]
pub struct Node {
    kind: SyntaxKind,
    span: Span,
    leaf_span: Span,
    parent: Option<usize>,
    children: Vec<usize>,
    prev: Option<usize>,
    next: Option<usize>,
    id: usize,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct NodeId(usize);

#[derive(Debug)]
pub enum Leaf {
    Operator(SyntaxToken),
    Literal(SyntaxToken),
    Identifier(SyntaxToken),
}

impl AnalyzedTreeBuilder {
    pub fn new() -> AnalyzedTreeBuilder {
        Self::default()
    }

    pub fn start_node(&mut self, node: SyntaxNode) {
        let current = self.nodes.len();
        let parent = self.current.last().copied();
        self.current.push(current);
        let mut prev = None;
        if let Some(parent) = parent {
            if let Some(p) = self.nodes[parent].children.last().copied() {
                prev = Some(p);
                self.nodes[p].next = Some(current);
            }

            self.nodes[parent].children.push(current);
        }
        self.nodes.push(Node {
            kind: node.kind(),
            span: node.text_range().into(),
            parent,
            leaf_span: self.leaves.len() - 1..0,
            children: Vec::new(),
            prev,
            next: None,
            id: current,
        });
    }

    pub fn push(&mut self, leaf: Leaf) {
        self.leaves.push(leaf);
    }

    pub fn finish_node(&mut self) {
        let current = self.current.pop().unwrap();
        self.nodes[current].leaf_span.end = self.leaves.len();
    }

    pub fn finish(self) -> AnalyzedTree {
        assert!(self.current.is_empty());
        assert!(!self.nodes.is_empty());
        AnalyzedTree {
            leaves: self.leaves,
            nodes: self.nodes,
        }
    }
}

impl Node {
    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }

    pub fn span(&self) -> Span {
        self.span.clone()
    }

    pub fn parent(&self) -> Option<NodeId> {
        self.parent.map(NodeId)
    }

    pub fn next(&self) -> Option<NodeId> {
        self.next.map(NodeId)
    }

    pub fn prev(&self) -> Option<NodeId> {
        self.prev.map(NodeId)
    }

    pub fn children(&self) -> &Vec<NodeId> {
        unsafe { std::mem::transmute(&self.children) }
    }

    pub fn iter_bfs<'a>(&self, tree: &'a AnalyzedTree) -> AnalyzedTreeIterBfs<'a> {
        AnalyzedTreeIterBfs {
            tree,
            queue: VecDeque::from([self.id]),
        }
    }

    pub fn iter_dfs<'a>(&self, tree: &'a AnalyzedTree) -> AnalyzedTreeIterDfs<'a> {
        AnalyzedTreeIterDfs {
            tree,
            stack: Vec::from([self.id]),
        }
    }
}

impl AnalyzedTree {
    pub fn node(&self, id: NodeId) -> Option<&Node> {
        self.nodes.get(id.0)
    }

    pub fn iter_bfs(&self) -> AnalyzedTreeIterBfs<'_> {
        AnalyzedTreeIterBfs {
            tree: self,
            queue: VecDeque::from([0]),
        }
    }

    pub fn iter_dfs(&self) -> AnalyzedTreeIterDfs<'_> {
        AnalyzedTreeIterDfs {
            tree: self,
            stack: Vec::from([0]),
        }
    }
}

pub struct AnalyzedTreeIterBfs<'a> {
    tree: &'a AnalyzedTree,
    queue: VecDeque<usize>,
}

impl<'a> Iterator for AnalyzedTreeIterBfs<'a> {
    type Item = &'a Node;
    fn next(&mut self) -> Option<Self::Item> {
        let node = self.queue.pop_front()?;
        let node = &self.tree.nodes[node];
        self.queue.extend(&node.children);
        Some(node)
    }
}

pub struct AnalyzedTreeIterDfs<'a> {
    tree: &'a AnalyzedTree,
    stack: Vec<usize>,
}

impl<'a> Iterator for AnalyzedTreeIterDfs<'a> {
    type Item = &'a Node;
    fn next(&mut self) -> Option<Self::Item> {
        let node = self.stack.pop()?;
        let node = &self.tree.nodes[node];
        self.stack.extend(&node.children);
        Some(node)
    }
}
