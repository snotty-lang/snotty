use super::{LexNumber, Position};
use std::{cmp, fmt};

/// List of all the keywords identified by the lexer
pub const KEYWORDS: [&str; 14] = [
    "let",
    "ez",
    "ezreturn",
    "ezout",
    "ezin",
    "ezascii",
    "true",
    "false",
    "if",
    "else",
    "ezblank",
    "ezbool",
    "ezpointer",
    "eznumber",
];

pub const BOOLEAN_OPERATORS: [TokenType; 6] = [
    TokenType::Neq,
    TokenType::Gt,
    TokenType::Le,
    TokenType::Lt,
    TokenType::Ge,
    TokenType::Eq,
];

pub const BOOLEAN_EXCLUSIVE: [TokenType; 4] = [
    TokenType::LAnd,
    TokenType::LOr,
    TokenType::LNot,
    TokenType::LXor,
];

pub const ASSIGNMENT_OPERATORS: [TokenType; 15] = [
    TokenType::Assign,
    TokenType::SubAssign,
    TokenType::AddAssign,
    TokenType::MulAssign,
    TokenType::DivAssign,
    TokenType::ModAssign,
    TokenType::ShlAssign,
    TokenType::ShrAssign,
    TokenType::BAndAssign,
    TokenType::BOrAssign,
    TokenType::BXorAssign,
    TokenType::PowAssign,
    TokenType::LXorAssign,
    TokenType::LAndAssign,
    TokenType::LOrAssign,
];

/// Different types of Tokens converted by the lexer
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
    PowAssign,
    LXorAssign,
    LAndAssign,
    LOrAssign,
    LAnd,
    LOr,
    LNot,
    LXor,
    Inc,
    Dec,
    Arrow,
    Colon,
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
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
    Number(LexNumber),
    LParen,
    RParen,
    LCurly,
    RCurly,
    Assign,
    Comma,
    Keyword(String),
    Eol,
    Eof,
    TernaryIf,
}

/// The token struct
#[derive(Clone, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub position: Position,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenType::TernaryIf => "?".to_owned(),
                TokenType::AddAssign => "+=".to_owned(),
                TokenType::SubAssign => "-=".to_owned(),
                TokenType::MulAssign => "*=".to_owned(),
                TokenType::DivAssign => "/=".to_owned(),
                TokenType::ModAssign => "%=".to_owned(),
                TokenType::ShlAssign => "<<=".to_owned(),
                TokenType::ShrAssign => ">>=".to_owned(),
                TokenType::BXorAssign => "^=".to_owned(),
                TokenType::BAndAssign => "&=".to_owned(),
                TokenType::BOrAssign => "|=".to_owned(),
                TokenType::PowAssign => "**=".to_owned(),
                TokenType::Inc => "++".to_owned(),
                TokenType::Dec => "--".to_owned(),
                TokenType::Arrow => "->".to_owned(),
                TokenType::Colon => ":".to_owned(),
                TokenType::Eq => "==".to_owned(),
                TokenType::Neq => "!=".to_owned(),
                TokenType::Lt => "<".to_owned(),
                TokenType::Gt => ">".to_owned(),
                TokenType::Le => "<=".to_owned(),
                TokenType::Ge => ">=".to_owned(),
                TokenType::LAnd => "&&".to_owned(),
                TokenType::LOr => "||".to_owned(),
                TokenType::LNot => "!".to_owned(),
                TokenType::Add => "+".to_owned(),
                TokenType::Sub => "-".to_owned(),
                TokenType::Mul => "*".to_owned(),
                TokenType::Div => "/".to_owned(),
                TokenType::Pow => "**".to_owned(),
                TokenType::Mod => "%".to_owned(),
                TokenType::Shl => "<<".to_owned(),
                TokenType::Shr => ">>".to_owned(),
                TokenType::BAnd => "&".to_owned(),
                TokenType::BNot => "~".to_owned(),
                TokenType::BOr => "|".to_owned(),
                TokenType::BXor => "^".to_owned(),
                TokenType::Identifier(ref s) => s.to_owned(),
                TokenType::Number(ref n) => n.to_string(),
                TokenType::LParen => "(".to_owned(),
                TokenType::RParen => ")".to_owned(),
                TokenType::LCurly => "{".to_owned(),
                TokenType::RCurly => "}".to_owned(),
                TokenType::Assign => "=".to_owned(),
                TokenType::Comma => ",".to_owned(),
                TokenType::Keyword(ref keyword) => keyword.to_owned(),
                TokenType::Eol => ";".to_owned(),
                TokenType::Eof => "End of file".to_owned(),
                TokenType::LXorAssign => "!&|=".to_owned(),
                TokenType::LAndAssign => "&&=".to_owned(),
                TokenType::LOrAssign => "||=".to_owned(),
                TokenType::LXor => "!&|".to_owned(),
            }
        )
    }
}

impl TokenType {
    pub fn get_operation_name(&self) -> String {
        match self {
            TokenType::Inc => "increment".to_owned(),
            TokenType::Dec => "decrement".to_owned(),
            TokenType::Eq => "equal to".to_owned(),
            TokenType::Neq => "not equal to".to_owned(),
            TokenType::Lt => "lesser than".to_owned(),
            TokenType::Gt => "greater than".to_owned(),
            TokenType::Le => "lesser than or equal to".to_owned(),
            TokenType::Ge => "greater than or equal to".to_owned(),
            TokenType::LAnd => "logical and".to_owned(),
            TokenType::LOr => "logical or".to_owned(),
            TokenType::LNot => "logical not".to_owned(),
            TokenType::Add => "add".to_owned(),
            TokenType::Sub => "subtract".to_owned(),
            TokenType::Mul => "multiply".to_owned(),
            TokenType::Div => "divide".to_owned(),
            TokenType::Pow => "exponent".to_owned(),
            TokenType::Mod => "mod".to_owned(),
            TokenType::Shl => "left shift".to_owned(),
            TokenType::Shr => "right shift".to_owned(),
            TokenType::BAnd => "binary and".to_owned(),
            TokenType::BNot => "binary not".to_owned(),
            TokenType::BOr => "binary or".to_owned(),
            TokenType::BXor => "binary xor".to_owned(),
            TokenType::LXor => "logical xor".to_owned(),
            TokenType::Identifier(_) => todo!(),
            TokenType::Number(_) => todo!(),
            TokenType::Assign => todo!(),
            TokenType::Keyword(_) => todo!(),
            _ => unreachable!(),
        }
    }
}

impl Token {
    pub fn new(
        token_type: TokenType,
        line: usize,
        start: usize,
        end: usize,
        filename: &'static str,
    ) -> Self {
        Self {
            token_type,
            position: Position::new(line, start, end, filename),
        }
    }

    pub fn un_augmented(self) -> Self {
        let token_type = match self.token_type {
            TokenType::AddAssign => TokenType::Add,
            TokenType::SubAssign => TokenType::Sub,
            TokenType::MulAssign => TokenType::Mul,
            TokenType::DivAssign => TokenType::Div,
            TokenType::ModAssign => TokenType::Mod,
            TokenType::ShlAssign => TokenType::Shl,
            TokenType::ShrAssign => TokenType::Shr,
            TokenType::BXorAssign => TokenType::BXor,
            TokenType::BAndAssign => TokenType::BAnd,
            TokenType::BOrAssign => TokenType::BOr,
            TokenType::PowAssign => TokenType::Pow,
            TokenType::LXorAssign => TokenType::LXor,
            TokenType::LAndAssign => TokenType::LAnd,
            TokenType::LOrAssign => TokenType::LOr,
            _ => self.token_type,
        };
        Self {
            token_type,
            position: self.position,
        }
    }
}

impl cmp::PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.token_type == other.token_type
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'{}'", self.token_type)
    }
}
