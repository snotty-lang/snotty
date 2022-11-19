use std::fmt::Display;

use crate::{parser::syntax::SyntaxKind, tree::Leaf};

#[derive(Debug, Clone)]
pub enum LeafType {
    Operator,
    Value(Value),
    Other,
}

impl Display for LeafType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LeafType::Operator | LeafType::Other => Ok(()),
            LeafType::Value(v) => write!(f, "{}", v),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u16)]
pub enum ValueType {
    None,
    Number,
    Pointer(u16),
}

#[derive(Debug, Clone)]
pub enum ValueKind {
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
    pub fn can_operate_binary(&self, op: &SyntaxKind, other: &ValueType) -> bool {
        match (self, op, other) {
            (ValueType::Number, _, ValueType::Number) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Value {
    pub value: ValueKind,
    pub syntax: Leaf<SyntaxKind>,
    pub type_: ValueType,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.value)
    }
}
