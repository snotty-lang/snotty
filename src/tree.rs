use std::collections::VecDeque;

use crate::{error::Error, parser::syntax::SyntaxKind, Span};

use crate::parser::syntax::Syntax;

#[derive(Default, Debug)]
pub struct Result<'a, L: Leaf = Syntax> {
    pub errors: Vec<Error<'a>>,
    pub output: Tree<L>,
}

#[derive(Default, Debug)]
pub struct Tree<L: Leaf = Syntax> {
    leaves: Vec<L>,
    nodes: Vec<Node>,
}

impl<L: Leaf> Tree<L> {
    pub fn node(&self, id: NodeId) -> Option<&Node> {
        self.nodes.get(id.0)
    }

    pub fn iter_bfs(&self) -> TreeIterBfs<'_, L> {
        TreeIterBfs {
            tree: self,
            queue: VecDeque::from([TreeElement::Node(NodeId(0))]),
        }
    }

    pub fn iter_dfs(&self) -> TreeIterDfs<'_, L> {
        TreeIterDfs {
            tree: self,
            stack: Vec::from([TreeElement::Node(NodeId(0))]),
        }
    }
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

    pub fn children_with_leaves<'a, L: Leaf>(&self, tree: &'a Tree<L>) -> ChildLeafIter<'a, L> {
        ChildLeafIter {
            tree,
            leaf: self.leaf_span.start,
            child: 0,
            node: self.id,
        }
    }

    pub fn leaves<'a, L: Leaf>(&self, tree: &'a Tree<L>) -> &'a [L] {
        &tree.leaves[self.leaf_span.clone()]
    }

    pub fn iter_bfs<'a, L: Leaf>(&self, tree: &'a Tree<L>) -> TreeIterBfs<'a, L> {
        TreeIterBfs {
            tree,
            queue: VecDeque::from([TreeElement::Node(NodeId(self.id))]),
        }
    }

    pub fn iter_dfs<'a, L: Leaf>(&self, tree: &'a Tree<L>) -> TreeIterDfs<'a, L> {
        TreeIterDfs {
            tree,
            stack: Vec::from([TreeElement::Node(NodeId(self.id))]),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct NodeId(usize);

pub trait Leaf {}
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct LeafId(usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TreeElement<N = Syntax, L = Syntax> {
    Node(N),
    Leaf(L),
}

impl<N, L> TreeElement<N, L> {
    pub fn as_leaf(self) -> Option<L> {
        if let Self::Leaf(l) = self {
            Some(l)
        } else {
            None
        }
    }

    pub fn as_node(self) -> Option<N> {
        if let Self::Node(n) = self {
            Some(n)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct TreeBuilder<L: Leaf = Syntax> {
    leaves: Vec<L>,
    nodes: Vec<Node>,
    current: Vec<usize>,
}

impl<L: Leaf> Default for TreeBuilder<L> {
    fn default() -> Self {
        Self {
            leaves: Vec::new(),
            nodes: Vec::new(),
            current: Vec::new(),
        }
    }
}

impl<L: Leaf> TreeBuilder<L> {
    pub fn new() -> TreeBuilder<L> {
        Self::default()
    }

    pub fn leaf_count(&self) -> usize {
        self.leaves.len()
    }

    pub fn leaf(&self, i: usize) -> &L {
        &self.leaves[i]
    }

    pub fn start_node(&mut self, kind: SyntaxKind, start: usize) {
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
            kind,
            span: Span { start, end: start },
            parent,
            leaf_span: self.leaves.len()..0,
            children: Vec::new(),
            prev,
            next: None,
            id: current,
        });
    }

    pub fn push(&mut self, leaf: L) {
        self.leaves.push(leaf);
    }

    pub fn finish_node(&mut self, end: usize) {
        let current = self.current.pop().unwrap();
        self.nodes[current].leaf_span.end = self.leaves.len();
        self.nodes[current].span.end = end;
    }

    pub fn finish(self) -> Tree<L> {
        assert!(self.current.is_empty());
        assert!(!self.nodes.is_empty());
        Tree {
            leaves: self.leaves,
            nodes: self.nodes,
        }
    }

    pub fn checkpoint(&self, start: usize) -> Checkpoint {
        Checkpoint {
            current: self.nodes.len(),
            start,
            parent: self.current.last().copied(),
        }
    }

    pub fn start_node_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) {
        let Checkpoint {
            current,
            start,
            parent,
        } = checkpoint;
        // self.current.push(current);
        let mut prev = None;
        if let Some(parent) = parent {
            if let Some(p) = self.nodes[parent].children.last().copied() {
                prev = Some(p);
                self.nodes[p].next = Some(current);
            }

            self.nodes[parent].children.push(current);
        }
        self.nodes.push(Node {
            kind,
            span: Span { start, end: start },
            parent,
            leaf_span: self.leaves.len()..0,
            children: Vec::new(),
            prev,
            next: None,
            id: current,
        });
    }
}

#[derive(Copy, Clone)]
pub struct Checkpoint {
    current: usize,
    start: usize,
    parent: Option<usize>,
}

pub struct TreeIterBfs<'a, L: Leaf> {
    tree: &'a Tree<L>,
    queue: VecDeque<TreeElement<NodeId, LeafId>>,
}

impl<'a, L: Leaf> Iterator for TreeIterBfs<'a, L> {
    type Item = TreeElement<&'a Node, &'a L>;
    fn next(&mut self) -> Option<Self::Item> {
        let node = match self.queue.pop_front()? {
            TreeElement::Leaf(LeafId(l)) => return Some(TreeElement::Leaf(&self.tree.leaves[l])),
            TreeElement::Node(NodeId(n)) => &self.tree.nodes[n],
        };
        self.queue.extend(node.children_with_leaves(self.tree));
        Some(TreeElement::Node(node))
    }
}

pub struct TreeIterDfs<'a, L: Leaf> {
    tree: &'a Tree<L>,
    stack: Vec<TreeElement<NodeId, LeafId>>,
}

impl<'a, L: Leaf> Iterator for TreeIterDfs<'a, L> {
    type Item = TreeElement<&'a Node, &'a L>;
    fn next(&mut self) -> Option<Self::Item> {
        let node = match self.stack.pop()? {
            TreeElement::Leaf(LeafId(l)) => return Some(TreeElement::Leaf(&self.tree.leaves[l])),
            TreeElement::Node(NodeId(n)) => &self.tree.nodes[n],
        };
        self.stack.extend(node.children_with_leaves(self.tree));
        Some(TreeElement::Node(node))
    }
}

pub struct ChildLeafIter<'a, L: Leaf> {
    tree: &'a Tree<L>,
    leaf: usize,
    child: usize,
    node: usize,
}

impl<'a, L: Leaf> Iterator for ChildLeafIter<'a, L> {
    type Item = TreeElement<NodeId, LeafId>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.leaf >= self.tree.nodes[self.node].leaf_span.end {
            return None;
        }
        let child_id = self.tree.nodes[self.node].children[self.child];
        let child = &self.tree.nodes[child_id];
        if child.leaf_span.start != self.leaf {
            let out = self.leaf;
            self.leaf += 1;
            Some(TreeElement::Leaf(LeafId(out)))
        } else {
            self.leaf = child.leaf_span.end;
            self.child += 1;
            Some(TreeElement::Node(NodeId(child_id)))
        }
    }
}
