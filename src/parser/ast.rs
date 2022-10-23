use crate::Spanned;

pub struct AST {
    pub rule: Spanned<Rule>,
    pub children: Vec<AST>,
}

impl IntoIterator for AST {
    type Item = AST;
    type IntoIter = <Vec<AST> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.children.into_iter()
    }
}

impl From<Spanned<Rule>> for AST {
    fn from(rule: Spanned<Rule>) -> Self {
        Self {
            rule,
            children: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Rule {
    Pointer,
    Byte,
    Identifier,
    None,
    Char,
    String,
    Input,
    Value,
    Expr,
    TypeCast,
    Kind,
    Ternary,
    CompExpr,
    ArithExpr,
    TermExpr,
    NegExpr,
    CallExpr,
    Increment,
    Decrement,
    If,
    While,
    For,
    FxDef,
    Statement,
    Scope,
    ParseError,
}
