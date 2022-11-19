use std::fmt::Display;

use logos::Logos;

#[rustfmt::skip]
#[derive(Logos, Debug, PartialEq, Clone, Eq, Hash, Copy, PartialOrd, Ord)]
#[repr(u16)]
pub enum SyntaxKind {
    #[token("fx")] FxKw,
    #[token("return")] ReturnKw,
    #[token("box")] BoxKw,
    #[token("loop")] LoopKw,
    #[token("else")] ElseKw,
    #[token("if")] IfKw,
    #[token("out")] OutKw,
    #[token("let")] LetKw,
    #[token("file")] FileKw,
    #[token("byte")] ByteKw,
    #[token("const")] ConstKw,
    #[token("in")] InKw,

    #[token("{")] OpenBrace,
    #[token("}")] CloseBrace,
    #[token("[")] OpenBracket,
    #[token("]")] CloseBracket,
    #[token("(")] OpenParen,
    #[token(")")] CloseParen,

    #[token(",")] Comma,
    #[token(":")] Colon,
    #[token(";")] SemiColon,
    #[token(".")] Dot,
    #[token("'")] Quote,
    #[token("?")] Question,
    #[token("=>")] FatArrow,
    #[token("->")] Arrow,
    #[token("=")] Assign,

    #[token("<")] LessThan,
    #[token(">")] GreaterThan,
    #[token("<=")] LessEqual,
    #[token(">=")] GreaterEqual,
    #[token("&")] And,
    #[token("|")] Or,
    #[token("!")] Not,
    #[token("^")] Xor,
    #[token("%")] Mod,
    #[token("==")] Equal,
    #[token("!=")] NotEqual,
    #[token("*")] Mul,
    #[token("/")] Div,
    #[token("+")] Add,
    #[token("-")] Sub,
    #[token("++")] Inc,
    #[token("--")] Dec,
    #[token("<<")] Shl,
    #[token(">>")] Shr,

    #[regex(r#"'[^']|\\(["ntrf\\b/']|x[0-9A-Fa-f]+|[0-7][0-7]?[0-7]?)'"#)] Char,
    #[regex(r#""([^"]|\\(["ntrf\\b/']|x[0-9A-Fa-f]+|[0-7][0-7]?[0-7]?))*""#)] String,
    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*")] Identifier,
    #[regex(r"\d+")] Number,

    #[regex(r"[ \t\n\f]+", logos::skip)] Whitespace,

    #[regex(r"\\\\.*\n", logos::skip)]
    #[regex(r"\\\*.*\*\\", logos::skip)]
    Comment,

    Eof,

    Root,
    Kind,

    Cast,
    Ternary,
    Pointer,
    Scope,
    Let,
    Loop,
    BinaryOp,
    UnaryOp,
    Call,
    If,
    Statement,
    Value,

    #[error]
    Error,
}

impl Display for SyntaxKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                SyntaxKind::FxKw => "'fx'",
                SyntaxKind::ReturnKw => "'return'",
                SyntaxKind::BoxKw => "'box'",
                SyntaxKind::LoopKw => "'loop'",
                SyntaxKind::ElseKw => "'else'",
                SyntaxKind::IfKw => "'if'",
                SyntaxKind::OutKw => "'out'",
                SyntaxKind::LetKw => "'let'",
                SyntaxKind::FileKw => "'file'",
                SyntaxKind::ByteKw => "'byte'",
                SyntaxKind::ConstKw => "'const",
                SyntaxKind::InKw => "'in'",
                SyntaxKind::OpenBrace => "{",
                SyntaxKind::CloseBrace => "}",
                SyntaxKind::OpenBracket => "[",
                SyntaxKind::CloseBracket => "]",
                SyntaxKind::OpenParen => "(",
                SyntaxKind::CloseParen => ")",
                SyntaxKind::Comma => ",",
                SyntaxKind::Colon => ":",
                SyntaxKind::SemiColon => ";",
                SyntaxKind::Dot => ".",
                SyntaxKind::Quote => "'",
                SyntaxKind::Question => "?",
                SyntaxKind::FatArrow => "=>",
                SyntaxKind::Arrow => "->",
                SyntaxKind::LessThan => "<",
                SyntaxKind::GreaterThan => ">",
                SyntaxKind::Assign => "=",
                SyntaxKind::LessEqual => "<=",
                SyntaxKind::GreaterEqual => ">=",
                SyntaxKind::And => "&",
                SyntaxKind::Or => "|",
                SyntaxKind::Not => "!",
                SyntaxKind::Xor => "^",
                SyntaxKind::Mod => "%",
                SyntaxKind::Equal => "==",
                SyntaxKind::NotEqual => "!=",
                SyntaxKind::Mul => "*",
                SyntaxKind::Div => "/",
                SyntaxKind::Add => "+",
                SyntaxKind::Sub => "-",
                SyntaxKind::Inc => "++",
                SyntaxKind::Dec => "--",
                SyntaxKind::Shl => "<<",
                SyntaxKind::Shr => ">>",
                SyntaxKind::Char => "character",
                SyntaxKind::String => "string",
                SyntaxKind::Identifier => "identifier",
                SyntaxKind::Number => "number",
                SyntaxKind::Comment => "comment",
                SyntaxKind::Whitespace => "space",
                SyntaxKind::Kind => "kind",
                SyntaxKind::Pointer => "pointer",
                SyntaxKind::Value => "value",
                SyntaxKind::Statement => "statement",
                SyntaxKind::Call => "CALL",
                SyntaxKind::BinaryOp => "BINARY OPERATION",
                SyntaxKind::UnaryOp => "UNARY OPERATION",
                SyntaxKind::Let => "LET",
                SyntaxKind::If => "IF",
                SyntaxKind::Loop => "LOOP",
                SyntaxKind::Scope => "SCOPE",
                SyntaxKind::Eof => "EOF",
                SyntaxKind::Root => "ROOT",
                SyntaxKind::Cast => "CAST",
                SyntaxKind::Ternary => "Ternary",
                SyntaxKind::Error => "ERROR",
            }
        )
    }
}

#[derive(Debug, Clone)]
pub struct Syntax {
    pub span: crate::Span,
    pub kind: SyntaxKind,
}

impl crate::tree::Leaf for Syntax {}

impl Display for Syntax {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}@{}..{}", self.kind, self.span.start, self.span.end)
    }
}
