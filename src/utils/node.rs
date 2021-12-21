use super::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Number(Token),
    BinaryOp(Token, Box<Node>, Box<Node>),
    UnaryOp(Token, Box<Node>),
    // VarAssign(Variable),
    VarAssign(Token, Box<Node>),
    VarAccess(Token),
    // VarReassign(Variable),
    VarReassign(Token, Box<Node>),
    Statements(Vec<Node>),
    Call(Box<Node>, Vec<Node>),
    FuncDef(Token, Vec<Token>, Box<Node>),
    Return(Token, Option<Box<Node>>),
}
