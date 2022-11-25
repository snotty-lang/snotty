use std::fmt::Display;

use crate::{
    parser::syntax::SyntaxKind,
    tree::{LeafId, NodeId, TreeElement},
};

#[derive(Debug, Clone)]
pub enum LeafType {
    Value(Value),
    Other,
}

impl Display for LeafType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LeafType::Other => Ok(()),
            LeafType::Value(v) => write!(f, "{}", v),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u16)]
pub enum ValueType {
    None,
    Number,
    Unknown,
    Posisoned,
    Pointer(u16),
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

impl ValueType {
    pub fn operate_binary(self, op: SyntaxKind, other: ValueType) -> Option<ValueType> {
        match (self, op, other) {
            (ValueType::Number, _, ValueType::Number) => Some(ValueType::Number),
            (ValueType::Posisoned, _, _) => Some(ValueType::Posisoned),
            _ => None,
        }
    }
}

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::None => write!(f, "None"),
            ValueType::Number => write!(f, "Number"),
            ValueType::Pointer(t) => write!(f, "*{{{t}}}"),
            ValueType::Unknown => write!(f, "?"),
            ValueType::Posisoned => write!(f, "\u{1F480}"),
        }
    }
}

impl Display for ValueData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueData::None => write!(f, "None"),
            ValueData::Number(n) => write!(f, "{n}"),
            &ValueData::Char(c) => write!(f, "{:?}", c as char),
            ValueData::String(s) => write!(f, "{}", std::str::from_utf8(s).unwrap()),
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
    pub type_: ValueType,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            Some(value) => write!(f, "{value}"),
            None => write!(f, "`{}`", self.type_),
        }
    }
}
