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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct LeafId(usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum NodeOrLeaf<N, L> {
    Node(N),
    Leaf(L),
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

    pub fn id(&self) -> NodeId {
        NodeId(self.id)
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

    pub fn children_with_leaves<'a>(&self, tree: &'a AnalyzedTree) -> ChildLeafIter<'a> {
        ChildLeafIter {
            tree,
            leaf: self.leaf_span.start,
            child: 0,
            node: self.id,
        }
    }

    pub fn leaves<'a>(&self, tree: &'a AnalyzedTree) -> &'a [Leaf] {
        &tree.leaves[self.leaf_span.clone()]
    }

    pub fn iter_bfs<'a>(&self, tree: &'a AnalyzedTree) -> AnalyzedTreeIterBfs<'a> {
        AnalyzedTreeIterBfs {
            tree,
            queue: VecDeque::from([NodeOrLeaf::Node(NodeId(self.id))]),
        }
    }

    pub fn iter_dfs<'a>(&self, tree: &'a AnalyzedTree) -> AnalyzedTreeIterDfs<'a> {
        AnalyzedTreeIterDfs {
            tree,
            stack: Vec::from([NodeOrLeaf::Node(NodeId(self.id))]),
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
            queue: VecDeque::from([NodeOrLeaf::Node(NodeId(0))]),
        }
    }

    pub fn iter_dfs(&self) -> AnalyzedTreeIterDfs<'_> {
        AnalyzedTreeIterDfs {
            tree: self,
            stack: Vec::from([NodeOrLeaf::Node(NodeId(0))]),
        }
    }
}

pub struct AnalyzedTreeIterBfs<'a> {
    tree: &'a AnalyzedTree,
    queue: VecDeque<NodeOrLeaf<NodeId, LeafId>>,
}

impl<'a> Iterator for AnalyzedTreeIterBfs<'a> {
    type Item = NodeOrLeaf<&'a Node, &'a Leaf>;
    fn next(&mut self) -> Option<Self::Item> {
        let node = match self.queue.pop_front()? {
            NodeOrLeaf::Leaf(LeafId(l)) => return Some(NodeOrLeaf::Leaf(&self.tree.leaves[l])),
            NodeOrLeaf::Node(NodeId(n)) => &self.tree.nodes[n],
        };
        self.queue.extend(node.children_with_leaves(self.tree));
        Some(NodeOrLeaf::Node(node))
    }
}

pub struct AnalyzedTreeIterDfs<'a> {
    tree: &'a AnalyzedTree,
    stack: Vec<NodeOrLeaf<NodeId, LeafId>>,
}

impl<'a> Iterator for AnalyzedTreeIterDfs<'a> {
    type Item = NodeOrLeaf<&'a Node, &'a Leaf>;
    fn next(&mut self) -> Option<Self::Item> {
        let node = match self.stack.pop()? {
            NodeOrLeaf::Leaf(LeafId(l)) => return Some(NodeOrLeaf::Leaf(&self.tree.leaves[l])),
            NodeOrLeaf::Node(NodeId(n)) => &self.tree.nodes[n],
        };
        self.stack.extend(node.children_with_leaves(self.tree));
        Some(NodeOrLeaf::Node(node))
    }
}

pub struct ChildLeafIter<'a> {
    tree: &'a AnalyzedTree,
    leaf: usize,
    child: usize,
    node: usize,
}

impl<'a> Iterator for ChildLeafIter<'a> {
    type Item = NodeOrLeaf<NodeId, LeafId>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.leaf >= self.tree.nodes[self.node].leaf_span.end {
            return None;
        }
        let child_id = self.tree.nodes[self.node].children[self.child];
        let child = &self.tree.nodes[child_id];
        if child.leaf_span.start != self.leaf {
            let out = self.leaf;
            self.leaf += 1;
            Some(NodeOrLeaf::Leaf(LeafId(out)))
        } else {
            self.leaf = child.leaf_span.end;
            self.child += 1;
            Some(NodeOrLeaf::Node(NodeId(child_id)))
        }
    }
}
