use logos::Logos;

#[rustfmt::skip]
#[derive(Logos, Debug, PartialEq, Clone, Eq, Hash)]
pub enum Token {
    #[token("fx")] Fx,
    #[token("return")] Return,
    #[token("box")] Box,
    #[token("for")] For,
    #[token("while")] While,
    #[token("otherwise")] Otherwise,
    #[token("if")] If,
    #[token("out")] Out,
    #[token("let")] Let,
    #[token("file")] File,
    #[token("byte")] ByteKw,
    #[token("const")] Const,
    #[token("in")] In,

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

    #[token("<")] LessThan,
    #[token(">")] GreaterThan,
    #[token("=")] Assign,
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
    #[regex(r"\d+")] Byte,

    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    #[regex(r"\\\\.*\n", logos::skip)]
    #[regex(r"\\\*.*\*\\", logos::skip)]
    LexError,
}
