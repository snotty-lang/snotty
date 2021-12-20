use super::token::Token;

#[derive(Debug)]
pub enum Node {
    Number(Token),
    BinOp(Token, Box<Node>, Box<Node>),
    VarAssign(Token, Box<Node>),
    UnaryOp(Token, Box<Node>),
    VarAccess(Token),
    Statements(Vec<Node>),
    Call(Box<Node>, Vec<Node>),
    VarReassign(Token, Box<Node>),
}
