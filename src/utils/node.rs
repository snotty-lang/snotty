use super::token::Token;

#[derive(Debug, Clone)]
pub enum Node {
    Number(Token),
    BinaryOp(Token, Box<Node>, Box<Node>),
    UnaryOp(Token, Box<Node>),
    VarAssign(Token, Box<Node>),
    VarAccess(Token),
    VarReassign(Token, Box<Node>),
    Statements(Vec<Node>),
    Call(Box<Node>, Vec<Node>),
    FuncDef(Token, Vec<Token>, Option<Token>, Box<Node>),
}
