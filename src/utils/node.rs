use crate::utils::{Position, Token};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Number,
    Boolean,
    None,
    Char,
    Array(Box<Type>, usize),
    Ref(Box<Type>),
    Function(Vec<Type>, Box<Type>),
}

/// A Node in the AST.
#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    /// Condition, Body
    While(Box<Node>, Box<Node>, Position),
    /// Number
    Number(Token),
    /// Boolean
    Boolean(Token),
    /// Operation, left, right
    BinaryOp(Token, Box<Node>, Box<Node>),
    /// Operation, expression
    UnaryOp(Token, Box<Node>),
    /// Variable, Expression, Type
    VarAssign(Token, Box<Node>, Option<Type>),
    /// Variable
    VarAccess(Token),
    /// Variable, Expression
    VarReassign(Token, Box<Node>),
    /// Statements
    Statements(Vec<Node>, Position),
    /// Function, args
    Call(Token, Vec<Node>, Position),
    /// Function, args, body, return type
    FuncDef(Token, Vec<(Token, Type)>, Box<Node>, Type, Position),
    /// Expression
    Return(Option<Box<Node>>, Position),
    /// Expressions
    Print(Vec<Node>, Position),
    /// Expressions
    Ascii(Vec<Node>, Position),
    /// Input
    Input(Position),
    /// Expression
    Ref(Box<Node>, Position),
    /// Expression
    Deref(Box<Node>, Position),
    /// Condition, then, else
    Ternary(Box<Node>, Box<Node>, Box<Node>, Position),
    /// Condition, then, else
    If(Box<Node>, Box<Node>, Option<Box<Node>>, Position),
    /// None
    None(Position),
    /// args, body, return type
    Lambda(Vec<(Token, Type)>, Box<Node>, Type, Position),
    /// Char
    Char(Token, Position),
    /// Elements
    Array(Vec<Node>, Position),
    /// Array, index
    Index(Token, Box<Node>, Position),
    /// Array, index, expression
    IndexAssign(Token, Box<Node>, Box<Node>),
    // Pointer, expression
    DerefAssign(Box<Node>, Box<Node>, Position),
}

impl Node {
    pub fn position(&self) -> Position {
        match self {
            Node::Number(token) | Node::Boolean(token) | Node::VarAccess(token) => {
                token.position.clone()
            }
            Node::Ref(.., pos)
            | Node::Deref(.., pos)
            | Node::While(.., pos)
            | Node::Statements(.., pos)
            | Node::Lambda(.., pos)
            | Node::Call(.., pos)
            | Node::FuncDef(.., pos)
            | Node::Print(.., pos)
            | Node::Ascii(.., pos)
            | Node::If(.., pos)
            | Node::Ternary(.., pos)
            | Node::None(pos)
            | Node::Char(.., pos)
            | Node::DerefAssign(.., pos)
            | Node::Array(.., pos)
            | Node::Index(.., pos)
            | Node::Input(pos) => pos.clone(),
            Node::BinaryOp(_, left, right) => {
                let mut pos = left.position();
                let end_pos = right.position();
                pos.end = end_pos.end;
                pos.line_end = end_pos.line_end;
                pos
            }
            Node::VarReassign(token, expr)
            | Node::VarAssign(token, expr, _)
            | Node::IndexAssign(token, _, expr)
            | Node::UnaryOp(token, expr) => {
                let mut pos = token.position.clone();
                let end_pos = expr.position();
                pos.end = end_pos.end;
                pos.line_end = end_pos.line_end;
                pos
            }
            Node::Return(val, pos) => {
                let mut pos = pos.clone();
                if let Some(val) = val {
                    let end_pos = val.position();
                    pos.end = end_pos.end;
                    pos.line_end = end_pos.line_end;
                }
                pos
            }
        }
    }
}
