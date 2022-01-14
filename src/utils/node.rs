use super::{Position, Token};

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
    Number(Token),
    Boolean(Token),
    BinaryOp(Token, Box<Node>, Box<Node>),
    UnaryOp(Token, Box<Node>),
    VarAssign(Token, Box<Node>, Option<Type>),
    VarAccess(Token),
    VarReassign(Token, Box<Node>),
    Statements(Vec<Node>, Position),
    Call(Token, Vec<Node>, Position),
    FuncDef(Token, Vec<(Token, Type)>, Box<Node>, Type, Position),
    Return(Option<Box<Node>>, Position),
    Print(Vec<Node>, Position),
    Ascii(Vec<Node>, Position),
    Input(Position),
    Ref(Box<Node>, Position),
    Deref(Box<Node>, Position),
    Ternary(Box<Node>, Box<Node>, Box<Node>, Position),
    If(Box<Node>, Box<Node>, Option<Box<Node>>, Position),
    None(Position),
    Char(Token, Position),
    Tuple(Position),
    Array(Vec<Node>, Position),
    Index(Token, Box<Node>, Position),
    IndexAssign(Token, Box<Node>, Box<Node>),
}

impl Node {
    pub fn position(&self) -> Position {
        match self {
            Node::Number(token) | Node::Boolean(token) | Node::VarAccess(token) => {
                token.position.clone()
            }
            Node::Ref(.., pos)
            | Node::Deref(.., pos)
            | Node::Statements(.., pos)
            | Node::Call(.., pos)
            | Node::FuncDef(.., pos)
            | Node::Print(.., pos)
            | Node::Ascii(.., pos)
            | Node::If(.., pos)
            | Node::Ternary(.., pos)
            | Node::Return(.., pos)
            | Node::Tuple(pos)
            | Node::None(pos)
            | Node::Char(.., pos)
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
        }
    }
}
