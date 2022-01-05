use super::token::Token;

/// A Node in the AST.
#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Number(Token),
    BinaryOp(Token, Box<Node>, Box<Node>),
    UnaryOp(Token, Box<Node>),
    VarAssign(Token, Box<Node>),
    VarAccess(Token),
    VarReassign(Token, Box<Node>),
    Statements(Vec<Node>),
    Call(Box<Node>, Vec<Node>),
    FuncDef(Token, Vec<Token>, Box<Node>),
    Return(Token, Option<Box<Node>>),
    Print(Box<Node>),
    Ascii(Box<Node>),
    Input,
    If(Box<Node>, Box<Node>, Option<Box<Node>>),
}
