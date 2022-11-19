use std::collections::VecDeque;
use std::fmt::Display;

use crate::{error::Error, parser::syntax::SyntaxKind, Span};

#[derive(Default, Debug)]
pub struct Result<'a, L: Leaf> {
    pub errors: Vec<Error<'a>>,
    pub output: Tree<L>,
}

#[derive(Default, Debug)]
pub struct Tree<L: Leaf> {
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

impl<L: Leaf + Display> Display for Tree<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn write_children<L: Leaf + Display>(
            tree: &Tree<L>,
            node: usize,
            indent: usize,
            f: &mut std::fmt::Formatter<'_>,
        ) -> std::fmt::Result {
            for child in tree.nodes[node].children_with_leaves(tree) {
                match child {
                    TreeElement::Leaf(l) => {
                        writeln!(f, "{}{}", " ".repeat(indent * 2), tree.leaves[l.0])?
                    }
                    TreeElement::Node(n) => {
                        writeln!(
                            f,
                            "{}{}:",
                            " ".repeat(indent * 2),
                            tree.nodes[n.0].kind.to_string().to_uppercase()
                        )?;
                        write_children(tree, n.0, indent + 1, f)?
                    }
                }
            }
            Ok(())
        }
        write_children(self, 0, 0, f)
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
pub enum TreeElement<N, L> {
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
pub struct TreeBuilder<L: Leaf> {
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
            child_no: self.nodes[self.current.last().copied().unwrap_or_default()]
                .children
                .len(),
            leaf: self.leaves.len(),
            start,
            parent: self.current.last().copied(),
        }
    }

    pub fn start_node_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) {
        let Checkpoint {
            child_no,
            start,
            leaf,
            parent,
        } = checkpoint;
        let current = self.nodes.len();
        self.current.push(current);

        let mut prev = None;
        let mut children = Vec::new();

        if let Some(parent) = parent {
            let nodes = self.nodes[parent]
                .children
                .drain(child_no..)
                .collect::<Vec<_>>();
            let mut nodes = nodes.iter();

            if let Some(&first) = nodes.next() {
                self.nodes[first].prev = None;
                self.nodes[first].parent = Some(current);
                children.push(first);
            }

            for &node in nodes {
                self.nodes[node].parent = Some(current);
                children.push(node);
            }

            if child_no != 0 {
                if let Some(&p) = self.nodes[parent].children.get(child_no - 1) {
                    prev = Some(p);
                    self.nodes[p].next = Some(current);
                }
            }

            self.nodes[parent].children.push(current);
        }
        self.nodes.push(Node {
            kind,
            span: Span { start, end: start },
            parent,
            leaf_span: leaf..0,
            children,
            prev,
            next: None,
            id: current,
        });
    }
}

#[derive(Copy, Clone)]
pub struct Checkpoint {
    child_no: usize,
    leaf: usize,
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
        let current = &self.tree.nodes[self.node];
        if self.leaf >= current.leaf_span.end {
            return None;
        }
        let child_id = current.children.get(self.child).copied();
        let child_span = child_id.map(|id| self.tree.nodes[id].leaf_span.clone());
        match (child_id, child_span) {
            (Some(id), Some(Span { start, end })) if start == self.leaf => {
                self.leaf = end;
                self.child += 1;
                Some(TreeElement::Node(NodeId(id)))
            }
            _ => {
                let out = self.leaf;
                self.leaf += 1;
                Some(TreeElement::Leaf(LeafId(out)))
            }
        }
    }
}
