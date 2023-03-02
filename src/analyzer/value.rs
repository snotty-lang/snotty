use std::{collections::HashMap, fmt::Display, ops::Deref};

use once_cell::sync::Lazy;

use crate::{
    parser::syntax::SyntaxKind,
    tree::{Leaf, LeafId, Node, NodeId, TreeElement},
    Span,
};

#[derive(Debug, Clone)]
pub struct LeafData {
    pub kind: LeafKind,
    pub assignable: AssignLHS,
}

impl LeafData {
    pub fn new(kind: LeafKind) -> Self {
        LeafData {
            kind,
            assignable: AssignLHS::Invalid,
        }
    }

    pub fn assignable(mut self, assignable: AssignLHS) -> Self {
        self.assignable = assignable;
        self
    }
}

impl Deref for LeafData {
    type Target = LeafKind;
    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

impl Display for LeafData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, Clone)]
pub enum LeafKind {
    Value(Value),
    Other,
}

impl Display for LeafKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LeafKind::Other => Ok(()),
            LeafKind::Value(v) => write!(f, "{}", v),
        }
    }
}

impl LeafKind {
    pub fn into_value(&self) -> &Value {
        match self {
            Self::Value(v) => v,
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct NodeData {
    pub kind: NodeKind,
    pub assignable: AssignLHS,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignLHS {
    Invalid,
    Ident,
    Deref(TreeElement<NodeId, LeafId>),
}

impl NodeData {
    pub fn new(kind: NodeKind) -> Self {
        NodeData {
            kind,
            assignable: AssignLHS::Invalid,
        }
    }

    pub fn assignable(mut self, assignable: AssignLHS) -> Self {
        self.assignable = assignable;
        self
    }
}

impl Deref for NodeData {
    type Target = NodeKind;
    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

impl Display for NodeData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, Clone)]
pub enum NodeKind {
    Value(Value),
    Kind(ValueType),
}

impl Display for NodeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NodeKind::Kind(v) => write!(f, "`{}`", v),
            NodeKind::Value(v) => write!(f, "{}", v),
        }
    }
}

impl NodeKind {
    pub fn type_(&self) -> &MaybeTyped {
        match self {
            NodeKind::Kind(_v) => todo!(),
            NodeKind::Value(v) => &v.type_,
        }
    }

    pub fn into_value(&self) -> &Value {
        match self {
            Self::Value(v) => v,
            _ => panic!(),
        }
    }
}

impl<'a> TreeElement<&'a Node<NodeData>, &'a Leaf<LeafData>> {
    /// **PANICS** when data of the node/leaf is none or type of leaf is not `LeafType::Value`
    pub fn type_(&self) -> &'a MaybeTyped {
        match self {
            TreeElement::Leaf(leaf) => match &**leaf.data().as_ref().unwrap() {
                LeafKind::Value(Value { type_, .. }) => type_,
                _ => panic!(),
            },
            TreeElement::Node(node) => node.data().as_ref().unwrap().type_(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            TreeElement::Node(node) => node.span(),
            TreeElement::Leaf(leaf) => leaf.span(),
        }
    }

    pub fn kind(&self) -> SyntaxKind {
        match self {
            TreeElement::Node(node) => node.kind(),
            TreeElement::Leaf(leaf) => leaf.kind(),
        }
    }

    pub fn data(&self) -> TreeElement<&'a NodeData, &'a LeafData> {
        match self {
            TreeElement::Node(node) => TreeElement::Node(node.data().as_ref().unwrap()),
            TreeElement::Leaf(leaf) => TreeElement::Leaf(leaf.data().as_ref().unwrap()),
        }
    }

    pub fn assignable(&self) -> AssignLHS {
        match self.data() {
            TreeElement::Node(node) => &node.assignable,
            TreeElement::Leaf(leaf) => &leaf.assignable,
        }
        .clone()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MaybeTyped {
    UnTyped(TreeElement<NodeId, LeafId>),
    InProgress,
    Typed(ValueType),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ValueType {
    None,
    Number,
    Poisoned,
    Pointer(Box<ValueType>),
    FnPtr(Vec<ValueType>),
}

#[derive(Debug, Clone)]
pub enum ValueData {
    None,
    Number(u32),
    Char(u8),
    String(Vec<u8>),
    /// References to the array elements
    Array(Vec<usize>),
    /// Reference to the variable
    Pointer(usize),
    /// Reference to the variable
    Variable(usize),
}

impl Display for MaybeTyped {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MaybeTyped::InProgress => write!(f, "Loading"),
            MaybeTyped::UnTyped(_) => write!(f, "?"),
            MaybeTyped::Typed(t) => write!(f, "{t}"),
        }
    }
}

impl ValueType {
    pub fn operate_binary(&self, op: SyntaxKind, other: &ValueType) -> Option<ValueType> {
        match (&self, op, other) {
            (ValueType::Number, _, ValueType::Number) => Some(ValueType::Number),
            (ValueType::Poisoned, _, _) => Some(ValueType::Poisoned),
            _ => None,
        }
    }

    pub fn operate_unary(&self, op: SyntaxKind) -> Option<ValueType> {
        match (self, op) {
            (ValueType::Number, SyntaxKind::Not | SyntaxKind::Sub) => Some(ValueType::Number),
            (ValueType::Poisoned, _) => Some(ValueType::Poisoned),
            (ValueType::Pointer(t), SyntaxKind::Mul) => Some((**t).clone()),
            _ => None,
        }
    }

    pub fn can_be_bool(&self) -> bool {
        matches!(self, ValueType::Number)
    }

    pub fn can_be_displayed(&self) -> bool {
        match self {
            ValueType::Pointer(x) if **x == ValueType::Number => true,
            ValueType::Number => true,
            _ => false,
        }
    }
}

impl MaybeTyped {
    pub fn type_(&self) -> Option<&ValueType> {
        if let MaybeTyped::Typed(type_) = self {
            Some(type_)
        } else {
            None
        }
    }

    pub fn is_typed(&self) -> bool {
        matches!(self, MaybeTyped::Typed(_))
    }

    pub fn map(self, f: impl Fn(ValueType) -> ValueType) -> Self {
        match self {
            MaybeTyped::Typed(t) => MaybeTyped::Typed(f(t)),
            t => t,
        }
    }
}

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::None => write!(f, "None"),
            ValueType::Number => write!(f, "Number"),
            ValueType::Pointer(t) => write!(f, "*{t}"),
            ValueType::Poisoned => write!(f, "\u{1F480}"),
            ValueType::FnPtr(v) => write!(f, "fn {v:?}"),
        }
    }
}

impl Display for ValueData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueData::None => write!(f, "None"),
            ValueData::Number(n) => write!(f, "{n}"),
            &ValueData::Char(c) => write!(f, "{:?}", c as char),
            ValueData::String(s) => write!(f, "{:?}", std::str::from_utf8(s).unwrap()),
            ValueData::Array(_) => todo!(),
            ValueData::Pointer(_) => todo!(),
            ValueData::Variable(_) => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Value {
    pub value: Option<ValueData>,
    pub syntax: TreeElement<NodeId, LeafId>,
    pub type_: MaybeTyped,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            Some(value) => write!(f, "{value}"),
            None => write!(f, "`{}`", self.type_),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BuiltInFunc {
    pub args: Vec<ValueType>,
    pub ret: Option<ValueType>,
}

impl BuiltInFunc {
    pub fn value_type(&self) -> ValueType {
        let mut t = self.args.clone();
        t.extend(self.ret.clone());
        ValueType::FnPtr(t)
    }
}

pub static BUILT_INS: Lazy<HashMap<&'static str, BuiltInFunc>> = Lazy::new(|| {
    let mut built_in = HashMap::new();
    built_in.insert(
        "puts",
        BuiltInFunc {
            args: vec![ValueType::Pointer(Box::new(ValueType::Number))],
            ret: Some(ValueType::Number),
        },
    );
    built_in.insert(
        "putchar",
        BuiltInFunc {
            args: vec![ValueType::Number],
            ret: Some(ValueType::Number),
        },
    );
    built_in.insert(
        "getchar",
        BuiltInFunc {
            args: vec![],
            ret: Some(ValueType::Number),
        },
    );
    built_in
});
