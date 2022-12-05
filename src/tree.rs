use std::fmt::Display;
use std::{collections::VecDeque, fmt::Debug};

use crate::{parser::syntax::SyntaxKind, Span};

#[derive(Default)]
pub struct Tree<N, L> {
    leaves: Vec<Leaf<L>>,
    nodes: Vec<Node<N>>,
}

impl<N, L> Tree<N, L> {
    pub const ROOT: NodeId = NodeId(0);

    pub fn node(&self, id: NodeId) -> &Node<N> {
        &self.nodes[id.0]
    }

    pub fn leaf(&self, id: LeafId) -> &Leaf<L> {
        &self.leaves[id.0]
    }

    pub fn iter_bfs(&self) -> TreeIterBfs<'_, N, L> {
        TreeIterBfs {
            tree: self,
            queue: VecDeque::from([TreeElement::Node(NodeId(0))]),
        }
    }

    pub fn iter_dfs(&self) -> TreeIterDfs<'_, N, L> {
        TreeIterDfs {
            tree: self,
            stack: Vec::from([TreeElement::Node(NodeId(0))]),
        }
    }
}

impl<N: Display, L: Display> Display for Tree<N, L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn write_children<N: Display, L: Display>(
            tree: &Tree<N, L>,
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
                        writeln!(f, "{}{}:", " ".repeat(indent * 2), tree.nodes[n.0])?;
                        write_children(tree, n.0, indent + 1, f)?
                    }
                }
            }
            Ok(())
        }
        write_children(self, 0, 0, f)
    }
}

impl<N: Debug, L: Debug> Debug for Tree<N, L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn write_children<N: Debug, L: Debug>(
            tree: &Tree<N, L>,
            node: usize,
            indent: usize,
            f: &mut std::fmt::Formatter<'_>,
        ) -> std::fmt::Result {
            for child in tree.nodes[node].children_with_leaves(tree) {
                match child {
                    TreeElement::Leaf(l) => {
                        writeln!(f, "{}{:?}", " ".repeat(indent * 2), tree.leaves[l.0])?
                    }
                    TreeElement::Node(n) => {
                        writeln!(f, "{}{:?}:", " ".repeat(indent * 2), tree.nodes[n.0])?;
                        write_children(tree, n.0, indent + 1, f)?
                    }
                }
            }
            Ok(())
        }
        write_children(self, 0, 0, f)
    }
}

#[derive(Clone)]
pub struct Node<N> {
    kind: SyntaxKind,
    span: Span,
    leaf_span: Span,
    parent: Option<usize>,
    children: Vec<usize>,
    prev: Option<usize>,
    next: Option<usize>,
    id: usize,
    data: Option<N>,
}

impl<N> Node<N> {
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

    pub fn children_with_leaves<'a, L>(&self, tree: &'a Tree<N, L>) -> ChildLeafIter<'a, N, L> {
        ChildLeafIter {
            tree,
            leaf: self.leaf_span.start,
            child: 0,
            node: self.id,
        }
    }

    pub fn children_with_leaves_builder<'a, L>(
        &self,
        builder: &'a TreeBuilder<N, L>,
    ) -> BuilderChildLeafIter<'a, N, L> {
        BuilderChildLeafIter {
            builder,
            leaf: self.leaf_span.start,
            child: 0,
            node: self.id,
        }
    }

    pub fn leaves<'a, L>(&self, tree: &'a Tree<N, L>) -> &'a [Leaf<L>] {
        &tree.leaves[self.leaf_span.clone()]
    }

    pub fn iter_bfs<'a, L>(&self, tree: &'a Tree<N, L>) -> TreeIterBfs<'a, N, L> {
        TreeIterBfs {
            tree,
            queue: VecDeque::from([TreeElement::Node(NodeId(self.id))]),
        }
    }

    pub fn iter_dfs<'a, L>(&self, tree: &'a Tree<N, L>) -> TreeIterDfs<'a, N, L> {
        TreeIterDfs {
            tree,
            stack: Vec::from([TreeElement::Node(NodeId(self.id))]),
        }
    }

    pub fn data(&self) -> &Option<N> {
        &self.data
    }

    pub fn data_mut(&mut self) -> &mut Option<N> {
        &mut self.data
    }
}

impl<N: Display> Display for Node<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}@{}..{}", self.kind, self.span.start, self.span.end)?;
        if let Some(data) = &self.data {
            write!(f, " = {}", data)?;
        }
        Ok(())
    }
}

impl<N: Debug> Debug for Node<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}@{}..{}", self.kind, self.span.start, self.span.end)?;
        if let Some(data) = &self.data {
            write!(f, " = {:?}", data)?;
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct Leaf<L> {
    kind: SyntaxKind,
    span: Span,
    id: usize,
    data: Option<L>,
}

impl<L> Leaf<L> {
    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }

    pub fn span(&self) -> Span {
        self.span.clone()
    }

    pub fn id(&self) -> LeafId {
        LeafId(self.id)
    }

    pub fn data(&self) -> &Option<L> {
        &self.data
    }

    pub fn data_mut(&mut self) -> &mut Option<L> {
        &mut self.data
    }
}

impl<L: Display> Display for Leaf<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}@{}..{}", self.kind, self.span.start, self.span.end)?;
        if let Some(data) = &self.data {
            write!(f, " = {}", data)?;
        }
        Ok(())
    }
}

impl<L: Debug> Debug for Leaf<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}@{}..{}", self.kind, self.span.start, self.span.end)?;
        if let Some(data) = &self.data {
            write!(f, " = {:?}", data)?;
        }
        Ok(())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct NodeId(usize);

impl NodeId {
    pub fn get<N, L>(self, tree: &Tree<N, L>) -> &Node<N> {
        &tree.nodes[self.0]
    }

    pub fn get_from_builder<N, L>(self, tree: &TreeBuilder<N, L>) -> &Node<N> {
        &tree.nodes[self.0]
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct LeafId(usize);

impl LeafId {
    pub fn get<N, L>(self, tree: &Tree<N, L>) -> &Leaf<L> {
        &tree.leaves[self.0]
    }

    pub fn get_from_builder<N, L>(self, tree: &TreeBuilder<N, L>) -> &Leaf<L> {
        &tree.leaves[self.0]
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TreeElement<N, L> {
    Node(N),
    Leaf(L),
}

impl<N, L> TreeElement<N, L> {
    pub fn into_leaf(self) -> Option<L> {
        if let Self::Leaf(l) = self {
            Some(l)
        } else {
            None
        }
    }

    pub fn into_node(self) -> Option<N> {
        if let Self::Node(n) = self {
            Some(n)
        } else {
            None
        }
    }
}

impl TreeElement<NodeId, LeafId> {
    pub fn get<'a, N, L>(&self, tree: &'a Tree<N, L>) -> TreeElement<&'a Node<N>, &'a Leaf<L>> {
        match self {
            TreeElement::Node(node) => TreeElement::Node(&tree.nodes[node.0]),
            TreeElement::Leaf(leaf) => TreeElement::Leaf(&tree.leaves[leaf.0]),
        }
    }

    pub fn get_from_builder<'a, N, L>(
        &self,
        builder: &'a TreeBuilder<N, L>,
    ) -> TreeElement<&'a Node<N>, &'a Leaf<L>> {
        match self {
            TreeElement::Node(node) => TreeElement::Node(&builder.nodes[node.0]),
            TreeElement::Leaf(leaf) => TreeElement::Leaf(&builder.leaves[leaf.0]),
        }
    }
}

#[derive(Debug)]
pub struct TreeBuilder<N, L> {
    leaves: Vec<Leaf<L>>,
    nodes: Vec<Node<N>>,
    current: Vec<usize>,
}

impl<N, L> Default for TreeBuilder<N, L> {
    fn default() -> Self {
        Self {
            leaves: Vec::new(),
            nodes: Vec::new(),
            current: Vec::new(),
        }
    }
}

impl<N: Debug, L> TreeBuilder<N, L> {
    pub fn new() -> TreeBuilder<N, L> {
        Self::default()
    }

    pub fn leaf(&self, id: LeafId) -> &Leaf<L> {
        &self.leaves[id.0]
    }

    pub fn leaf_mut(&mut self, id: LeafId) -> &mut Leaf<L> {
        &mut self.leaves[id.0]
    }

    pub fn node(&self, id: NodeId) -> &Node<N> {
        &self.nodes[id.0]
    }

    pub fn node_mut(&mut self, id: NodeId) -> &mut Node<N> {
        &mut self.nodes[id.0]
    }

    pub fn undo_leaf(&mut self) {
        self.leaves.pop();
    }

    pub fn undo_node(&mut self) {
        self.nodes.pop();
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
            data: None,
        });
    }

    pub fn push(
        &mut self,
        kind: SyntaxKind,
        span: Span,
        data: impl FnOnce(LeafId) -> Option<L>,
    ) -> LeafId {
        let current = self.leaves.len();
        let id = LeafId(current);
        self.leaves.push(Leaf {
            kind,
            span,
            id: current,
            data: data(id),
        });
        id
    }

    #[track_caller]
    pub fn finish_node(&mut self, end: usize, data: impl FnOnce(NodeId) -> Option<N>) -> NodeId {
        let current = self.current.pop().expect("No node to finish");
        let id = NodeId(current);
        self.nodes[current].leaf_span.end = self.leaves.len();
        self.nodes[current].span.end = end;
        self.nodes[current].data = data(id);
        id
    }

    pub fn finish(self) -> Tree<N, L> {
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
            data: None,
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

pub struct TreeIterBfs<'a, N, L> {
    tree: &'a Tree<N, L>,
    queue: VecDeque<TreeElement<NodeId, LeafId>>,
}

impl<'a, N, L> Iterator for TreeIterBfs<'a, N, L> {
    type Item = TreeElement<&'a Node<N>, &'a Leaf<L>>;
    fn next(&mut self) -> Option<Self::Item> {
        let node = match self.queue.pop_front()? {
            TreeElement::Leaf(LeafId(l)) => return Some(TreeElement::Leaf(&self.tree.leaves[l])),
            TreeElement::Node(NodeId(n)) => &self.tree.nodes[n],
        };
        self.queue.extend(node.children_with_leaves(self.tree));
        Some(TreeElement::Node(node))
    }
}

pub struct TreeIterDfs<'a, N, L> {
    tree: &'a Tree<N, L>,
    stack: Vec<TreeElement<NodeId, LeafId>>,
}

impl<'a, N, L> Iterator for TreeIterDfs<'a, N, L> {
    type Item = TreeElement<&'a Node<N>, &'a Leaf<L>>;
    fn next(&mut self) -> Option<Self::Item> {
        let node = match self.stack.pop()? {
            TreeElement::Leaf(LeafId(l)) => return Some(TreeElement::Leaf(&self.tree.leaves[l])),
            TreeElement::Node(NodeId(n)) => &self.tree.nodes[n],
        };
        self.stack.extend(node.children_with_leaves(self.tree));
        Some(TreeElement::Node(node))
    }
}

pub struct ChildLeafIter<'a, N, L> {
    tree: &'a Tree<N, L>,
    leaf: usize,
    child: usize,
    node: usize,
}

impl<'a, N, L> Iterator for ChildLeafIter<'a, N, L> {
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

pub struct BuilderChildLeafIter<'a, N, L> {
    builder: &'a TreeBuilder<N, L>,
    leaf: usize,
    child: usize,
    node: usize,
}

impl<'a, N, L> Iterator for BuilderChildLeafIter<'a, N, L> {
    type Item = TreeElement<NodeId, LeafId>;
    fn next(&mut self) -> Option<Self::Item> {
        let current = &self.builder.nodes[self.node];
        if self.leaf >= current.leaf_span.end {
            return None;
        }
        let child_id = current.children.get(self.child).copied();
        let child_span = child_id.map(|id| self.builder.nodes[id].leaf_span.clone());
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
