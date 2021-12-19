use super::error::Position;
use std::cmp;

pub const KEYWORDS: [&str; 1] = ["let"];

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    ShlAssign,
    ShrAssign,
    BXorAssign,
    BAndAssign,
    BOrAssign,
    BNotAssign,
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
    LAnd,
    LOr,
    LNot,
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Mod,
    Shl,
    Shr,
    BAnd,
    BNot,
    BOr,
    BXor,
    Identifier(String),
    Number(u32),
    LParen,
    RParen,
    LCurly,
    RCurly,
    Assign,
    Comma,
    Keyword(String),
    Eol,
    Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub position: Position,
}

impl Token {
    pub fn new(token_type: TokenType, line: usize, start: usize, end: usize) -> Self {
        Self {
            token_type,
            position: Position { line, start, end },
        }
    }
}

impl cmp::PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.token_type == other.token_type
    }
}
