use crate::grammar;
use crate::parser::grammar::Grammar;
use lazy_static::lazy_static;

#[rustfmt::skip]
lazy_static! {
    pub static ref SNOTTY_GRAMMAR: Grammar = grammar! {
        Token(Byte)             => Value,
        Token(Char)             => Value,
        Token(SemiColon)        => Value,
        Token(String)           => Value,
        Rule(Pointer)           => Value,
        Token(Identifier)       => Value,
        Token(In)               => Value,
        Token(OpenParen), Rule(Expr), Token(CloseParen) => Value,
        Token(OpenBrace), Rule(Expr), Token(ClosedBrace) => Pointer,

        Token(ByteKw)           => Kind,
        Token(SemiColon)        => Kind,
        Token(And), Rule(Kind)  => Kind,
        Token(Mul), Rule(Kind)  => Kind,
        Token(Identifier)       => Kind,

        Token(Fx), Token(OpenBrace), Rule(CSK), Token(ClosedBrace) => Kind,
        Token(Fx), Token(OpenBrace), Rule(Kind), Token(ClosedBrace) => Kind,
        Token(Fx), Token(OpenBrace), Rule(CSK), Token(ClosedBrace), Token(Arrow), Rule(Kind) => Kind,
        Token(Fx), Token(OpenBrace), Rule(Kind), Token(ClosedBrace), Token(Arrow), Rule(Kind) => Kind,
        Rule(CSK), Token(Comma), Rule(Kind) => CSK,
        Rule(Kind), Token(Comma), Rule(Kind) => CSK,

        Token(LessThan), Rule(Kind), Token(GreaterThan), Rule(Expr) => TypeCast,
        Rule(Expr), Token(Question), Rule(Expr), Token(Colon), Rule(Expr) => Ternary,
        Token(Mul), Rule(Expr)                         => UnOpExpr,
        Token(And), Rule(Expr)                         => UnOpExpr,
        Token(Sub), Rule(Expr)                         => UnOpExpr,
        Token(Not), Rule(Expr)                         => UnOpExpr,
        Rule(Expr), Token(Add),             Rule(Expr) => BinOpExpr,
        Rule(Expr), Token(Sub),             Rule(Expr) => BinOpExpr,
        Rule(Expr), Token(Mul),             Rule(Expr) => BinOpExpr,
        Rule(Expr), Token(Div),             Rule(Expr) => BinOpExpr,
        Rule(Expr), Token(Mod),             Rule(Expr) => BinOpExpr,
        Rule(Expr), Token(And),             Rule(Expr) => BinOpExpr,
        Rule(Expr), Token(Or),              Rule(Expr) => BinOpExpr,
        Rule(Expr), Token(Xor),             Rule(Expr) => BinOpExpr,
        Rule(Expr), Token(Equal),           Rule(Expr) => BinOpExpr,
        Rule(Expr), Token(NotEqual),        Rule(Expr) => BinOpExpr,
        Rule(Expr), Token(GreaterThan),     Rule(Expr) => BinOpExpr,
        Rule(Expr), Token(GreaterEqual),    Rule(Expr) => BinOpExpr,
        Rule(Expr), Token(LessEqual),       Rule(Expr) => BinOpExpr,
        Rule(Expr), Token(LessThan),        Rule(Expr) => BinOpExpr,
        Rule(Expr), Token(Shl),             Rule(Expr) => BinOpExpr,
        Rule(Expr), Token(Shr),             Rule(Expr) => BinOpExpr,

        Rule(Value), Token(OpenParen), Rule(Expr), Token(CloseParen) => Call,
        Rule(Value), Token(OpenParen), Rule(CSA), Token(CloseParen) => Call,
        Rule(CSA), Token(Comma), Rule(Expr) => CSA,
        Rule(Expr), Token(Comma), Rule(Expr) => CSA,

        Rule(TypeCast)  => Expr,
        Rule(Ternary)   => Expr,
        Rule(Value)     => Expr,
        Rule(UnOpExpr)  => Expr,
        Rule(Call)      => Expr,
        Rule(BinOpExpr) => Expr,

        Token(LexError) => ParseError,
    };
}
