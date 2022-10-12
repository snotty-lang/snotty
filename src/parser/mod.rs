pub mod grammar;
pub mod snotty;
pub mod token;

pub type Span = core::ops::Range<usize>;
use self::grammar::{Grammar, Item, Rule, SpannedItem, SpannedRule};
use self::snotty::SNOTTY_GRAMMAR;
use crate::error::{Error, ErrorKind};
use token::{SpannedToken, Token};

use logos::Logos;

#[derive(Debug, Clone)]
pub struct ParseResult<'a> {
    pub rules: Vec<SpannedRule>,
    pub errors: Vec<Error<'a>>,
    pub tokens: Vec<SpannedToken>,
}

pub struct Parser<'source> {
    source: &'source str,
    tokens: Vec<SpannedToken>,
    buffer: Vec<SpannedItem>,
    errors: Vec<Error<'source>>,
    grammar: Grammar,
}

impl<'source> Parser<'source> {
    pub fn new(source: &'source str, grammar: Grammar) -> Parser {
        Self {
            source,
            tokens: Token::lexer(source).spanned().collect(),
            buffer: Vec::new(),
            errors: Vec::new(),
            grammar,
        }
    }

    pub fn new_snotty(source: &'source str) -> Parser {
        Self {
            source,
            tokens: Token::lexer(source).spanned().collect(),
            buffer: Vec::new(),
            errors: Vec::new(),
            grammar: SNOTTY_GRAMMAR.clone(),
        }
    }

    pub fn shift_reduce(mut self) -> ParseResult<'source> {
        for (token, span) in self.tokens.iter() {
            self.buffer.push((Item::Token(token.clone()), span.clone()));
            while let Some((rule, span)) = self.grammar.reduce(&mut self.buffer) {
                self.buffer.push((Item::Rule(rule), span));
            }
        }

        let rules = match self.buffer.as_slice() {
            [] => {
                self.errors.push(
                    Error::new(ErrorKind::Error, 0..self.source.len(), self.source)
                        .with_description(String::from("Empty file")),
                );
                vec![(Rule::ParseError, 0..self.source.len())]
            }
            slice if slice.iter().all(|(item, _)| matches!(item, Item::Rule(_))) => self
                .buffer
                .into_iter()
                .map(|(item, span)| (item.into_rule().unwrap(), span))
                .collect(),
            s => todo!("{s:?}"),
        };

        ParseResult {
            rules,
            errors: self.errors,
            tokens: self.tokens,
        }
    }
}
