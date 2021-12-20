use super::token::Token;

#[derive(Debug)]
pub enum Node {
    Number(Token),
    BinaryOp(Token, Box<Node>, Box<Node>),
    UnaryOp(Token, Box<Node>),
    VarAssign(Token, Box<Node>),
    VarAccess(Token),
    VarReassign(Token, Box<Node>),
    Statements(Vec<Node>),
    Call(Box<Node>, Vec<Node>),
}
