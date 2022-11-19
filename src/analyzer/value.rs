use std::fmt::Display;

use crate::{parser::syntax::Syntax, tree::TreeElement};

#[derive(Debug, Clone)]
pub enum Leaf {
    Operator(Syntax),
    Value(Value),
    Other(Syntax),
}

impl crate::tree::Leaf for Leaf {}
impl Display for Leaf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Leaf::Operator(a) | Leaf::Other(a) => write!(f, "{}", a),
            Leaf::Value(v) => write!(f, "{}", v),
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
    pub fn can_operate_binary(&self, op: &Syntax, other: &ValueType) -> bool {
        match (self, op.kind, other) {
            (ValueType::Number, _, ValueType::Number) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Value {
    pub value: ValueKind,
    pub element: TreeElement<Syntax, Syntax>,
    pub type_: ValueType,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.element)
    }
}
