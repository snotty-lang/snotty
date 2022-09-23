use std::fmt;

use crate::parser::Rule;
use pest::error::Error as PestError;
use pest::iterators::Pair;

#[derive(Debug, Clone)]
pub enum Error<'a> {
    SyntaxError(PestError<Rule>),
    NumberOutOfRange(Pair<'a, Rule>),
    UndefinedReference(Pair<'a, Rule>),
    InvalidReturn(Pair<'a, Rule>),
    TypeError(Pair<'a, Rule>),
    Redefinition(Pair<'a, Rule>),
    RecursionError(Pair<'a, Rule>),
}

impl<'a> fmt::Display for Error<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::SyntaxError(err) => write!(f, "{}", err),
            Error::NumberOutOfRange(pair) => {
                let span = pair.as_span();
                let (sl, sc) = span.start_pos().line_col();
                let (el, ec) = span.end_pos().line_col();
                write!(f, "NumberOutOfRange at {}:{} to {}:{}", sl, sc, el, ec,)
            }
            Error::UndefinedReference(pair) => {
                let span = pair.as_span();
                let (sl, sc) = span.start_pos().line_col();
                let (el, ec) = span.end_pos().line_col();
                write!(f, "UndefinedReference at {}:{} to {}:{}", sl, sc, el, ec)
            }
            Error::InvalidReturn(pair) => {
                let span = pair.as_span();
                let (sl, sc) = span.start_pos().line_col();
                let (el, ec) = span.end_pos().line_col();
                write!(f, "InvalidReturn at {}:{} to {}:{}", sl, sc, el, ec,)
            }
            Error::TypeError(pair) => {
                let span = pair.as_span();
                let (sl, sc) = span.start_pos().line_col();
                let (el, ec) = span.end_pos().line_col();
                write!(f, "TypeError at {}:{} to {}:{}", sl, sc, el, ec,)
            }
            Error::Redefinition(pair) => {
                let span = pair.as_span();
                let (sl, sc) = span.start_pos().line_col();
                let (el, ec) = span.end_pos().line_col();
                write!(f, "Redefinition at {}:{} to {}:{}", sl, sc, el, ec,)
            }
            Error::RecursionError(pair) => {
                let span = pair.as_span();
                let (sl, sc) = span.start_pos().line_col();
                let (el, ec) = span.end_pos().line_col();
                write!(f, "RecursionError at {}:{} to {}:{}", sl, sc, el, ec,)
            }
        }
    }
}

impl<'a> std::error::Error for Error<'a> {}

impl<'a> From<PestError<Rule>> for Error<'a> {
    fn from(err: PestError<Rule>) -> Self {
        Error::SyntaxError(err)
    }
}
