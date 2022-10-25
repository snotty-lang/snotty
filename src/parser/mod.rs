pub mod syntax;

use crate::{
    error::{Error, ErrorKind},
    Spanned,
};
use syntax::{Parse, SyntaxKind};
use SyntaxKind::*;

use logos::Logos;
use rowan::GreenNodeBuilder;

#[derive(Debug, PartialEq, Eq, Clone)]
enum ParseResult {
    Recovered(usize),
    SemiColon,
    Eof,
    Ok,
}

impl ParseResult {
    fn is_end(&self) -> bool {
        matches!(self, ParseResult::Eof | ParseResult::SemiColon)
    }
}

pub struct Parser<'a> {
    source: &'a str,
    tokens: Vec<Spanned<SyntaxKind>>,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<Error<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Parser {
        Self {
            source,
            tokens: SyntaxKind::lexer(source)
                .spanned()
                .map(Spanned::from)
                .collect::<Vec<_>>()
                .into_iter()
                .rev()
                .collect(),
            builder: GreenNodeBuilder::new(),
            errors: Vec::new(),
        }
    }

    pub fn parse(mut self) -> Parse<'a> {
        self.builder.start_node(Root.into());

        while {
            self.skip_space();
            !self.tokens.is_empty()
        } {
            self.statement();
        }

        assert_eq!(self.current_syntax(), Eof);
        self.builder.finish_node();

        Parse {
            green_node: self.builder.finish(),
            errors: self.errors,
        }
    }

    fn statement(&mut self) {
        match self.current_syntax() {
            LetKw => {
                let start = self.current_span_location();
                self.builder.start_node(LetKw.into());
                let ident = self.bump_space();

                if ident != Identifier {
                    match self.unexpected_syntax(
                        self.current_span_location(),
                        Identifier,
                        &[Identifier, Assign],
                    ) {
                        ParseResult::SemiColon | ParseResult::Eof => {
                            self.builder.finish_node();
                            self.bump_space();
                            return;
                        }
                        ParseResult::Recovered(0) => {
                            self.bump_space();
                        }
                        _ => (),
                    }
                } else {
                    self.bump_space();
                }

                if self.current_syntax() != Assign
                    && self.unexpected_syntax(start, Assign, &[Assign]).is_end()
                {
                    self.builder.finish_node();
                    self.bump_space();
                    return;
                }

                self.bump_space();
                self.expression();

                self.builder.finish_node();
            }
            _ => self.expression(),
        }

        if self.current_syntax() != SemiColon {
            self.errors.push(Error::error(
                ErrorKind::MissingSemicolon,
                self.current_span_location() + 1..0,
                self.source,
            ))
        } else {
            self.bump_space();
        }
    }

    fn expression(&mut self) {
        match self.current_syntax() {
            LessThan => {
                self.builder.start_node(Cast.into());
                self.bump_space();

                if self.kind(&[GreaterThan]).is_end() {
                    self.builder.finish_node();
                    self.bump_space();
                    return;
                }

                if self.current_syntax() != GreaterThan
                    && self
                        .unexpected_syntax(self.current_span_location(), Assign, &[GreaterThan])
                        .is_end()
                {
                    self.builder.finish_node();
                    self.bump_space();
                    return;
                }

                self.bump_space();
                self.expression();

                self.builder.finish_node();
            }
            _ => {
                self.value(&[]);
            }
        }
    }

    fn value(&mut self, recovery: &[SyntaxKind]) -> ParseResult {
        match self.current_syntax() {
            Number | Char | String | SemiColon | InKw | Identifier => {
                self.bump_space();
            }
            Error => {
                let start = self.current_span_location();
                self.bump();
                self.errors.push(Error::error(
                    ErrorKind::UnknownSyntax,
                    start..self.current_span_location(),
                    self.source,
                ));
                self.skip_space();
            }
            OpenParen => {
                self.bump_space();
                self.expression();

                let mut s = ParseResult::Ok;
                if self.current_syntax() != CloseParen {
                    s = self.unexpected_syntax(self.current_span_location(), CloseParen, recovery);
                }

                self.bump_space();
                return s;
            }
            _ => todo!(),
        }
        ParseResult::Ok
    }

    fn kind(&mut self, recovery: &[SyntaxKind]) -> ParseResult {
        match self.current_syntax() {
            ByteKw | Identifier | SemiColon => {
                self.builder.start_node(Kind.into());
                self.bump_space();
                self.builder.finish_node();
                ParseResult::Ok
            }
            And | Mul => {
                self.builder.start_node(Kind.into());
                self.bump_space();
                let s = self.kind(recovery);
                self.builder.finish_node();
                s
            }
            _ => {
                let s = self.unexpected_syntax(self.current_span_location(), Kind, &[]);
                self.bump_space();
                s
            }
        }
    }

    /// Creates an UnexpectedSyntax error
    fn unexpected_syntax(
        &mut self,
        start: usize,
        expected: SyntaxKind,
        skip_until: &[SyntaxKind],
    ) -> ParseResult {
        self.builder.start_node(Error.into());
        self.bump();
        self.errors.push(Error::error(
            ErrorKind::UnexpectedSyntax { expected },
            start..self.current_span_location(),
            self.source,
        ));
        let s = self.skip_till_syntax(skip_until);
        self.builder.finish_node();
        s
    }

    /// Skip Whitespace and Comments
    fn skip_space(&mut self) {
        while matches!(self.current_syntax(), Whitespace | Comment) {
            self.bump();
        }
    }

    /// Skips until the specified syntax is encountered.
    /// Returns true if the syntax was found else false if eof was found
    fn skip_till_syntax(&mut self, syntaxs: &[SyntaxKind]) -> ParseResult {
        loop {
            match self.current_syntax() {
                Eof => return ParseResult::Eof,
                SemiColon => return ParseResult::SemiColon,
                s => {
                    if let Some((i, _)) =
                        syntaxs.iter().enumerate().find(|(_, &syntax)| syntax == s)
                    {
                        return ParseResult::Recovered(i);
                    }
                }
            }
            self.bump();
        }
    }

    /// Consume current syntax and push to builder.
    fn bump(&mut self) {
        if let Some(Spanned { span, value }) = self.tokens.pop() {
            self.builder.token(value.into(), &self.source[span]);
        }
    }

    fn bump_space(&mut self) -> SyntaxKind {
        self.bump();
        self.skip_space();
        self.current_syntax()
    }

    /// Peeks the current syntax
    fn current_syntax(&self) -> SyntaxKind {
        self.tokens
            .last()
            .map(|spanned| spanned.value)
            .unwrap_or(Eof)
    }

    fn current_span_location(&self) -> usize {
        self.tokens
            .last()
            .map(|spanned| spanned.span.start)
            .unwrap_or(self.source.len() - 1)
    }
}
