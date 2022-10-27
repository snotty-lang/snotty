mod peekable;
pub mod syntax;

use crate::error::{Error, ErrorKind};
use peekable::Peekable;
use syntax::{Parse, SyntaxKind};
use SyntaxKind::*;

use logos::{Logos, SpannedIter};
use rowan::GreenNodeBuilder;

#[derive(Debug, PartialEq, Eq, Clone)]
enum ParseRecovery {
    Recovered(usize),
    SemiColon,
    Eof,
    Ok,
}

impl ParseRecovery {
    fn is_end(&self) -> bool {
        matches!(self, ParseRecovery::Eof | ParseRecovery::SemiColon)
    }
}

pub struct Parser<'a> {
    source: &'a str,
    tokens: Peekable<SpannedIter<'a, SyntaxKind>>,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<Error<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Parser {
        Self {
            source,
            tokens: Peekable::new(SyntaxKind::lexer(source).spanned()),
            builder: GreenNodeBuilder::new(),
            errors: Vec::new(),
        }
    }

    pub fn parse(mut self) -> Parse<'a> {
        self.builder.start_node(Root.into());

        while {
            self.skip_space();
            self.tokens.peek(1).is_some()
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
                    let current = self.current_span_location();
                    if self
                        .unexpected_syntax(current, Identifier, &[Assign])
                        .is_end()
                    {
                        self.builder.finish_node();
                        self.bump_space();
                        return;
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
                self.expression(&[]);
                self.builder.finish_node();
            }
            a @ (OutKw | ReturnKw) => {
                self.builder.start_node(a.into());
                self.bump_space();
                self.expression(&[]);
                self.builder.finish_node();
            }
            FileKw => {
                let start = self.current_span_location();
                self.builder.start_node(FileKw.into());
                self.bump_space();
                if self.current_syntax() != String {
                    self.unexpected_syntax(start, String, &[]);
                }
                self.bump();
                self.builder.finish_node();
            }
            OpenBrace => {
                self.builder.start_node(Scope.into());
                self.bump_space();
                loop {
                    match self.current_syntax() {
                        Eof => {
                            self.builder.start_node(Error.into());
                            let current = self.current_span_location();
                            self.errors.push(Error::error(
                                ErrorKind::UnexpectedSyntax {
                                    expected: CloseBrace,
                                },
                                current + 1..0,
                                self.source,
                            ));
                            self.builder.finish_node();
                            break;
                        }
                        CloseBrace => {
                            self.bump();
                            break;
                        }
                        _ => self.statement(),
                    }
                }
                self.builder.finish_node();
                return;
            }
            _ => {
                self.expression(&[]);
            }
        }

        let end = self.current_span_location();
        self.skip_space();
        if self.current_syntax() != SemiColon {
            self.errors.push(Error::error(
                ErrorKind::MissingSemicolon,
                end..end + 1,
                self.source,
            ))
        }

        self.skip_syntax(&[Whitespace, Comment, SemiColon]);
    }

    fn expression(&mut self, recovery: &[SyntaxKind]) -> ParseRecovery {
        let start = self.builder.checkpoint();
        match self.current_syntax() {
            LessThan => {
                self.builder.start_node(Cast.into());
                self.bump_space();

                let mut r = Vec::from(recovery);
                r.push(GreaterThan);
                let s = self.kind(&r);
                if s.is_end() {
                    self.builder.finish_node();
                    return s;
                }

                self.skip_space();

                let current = self.current_span_location();
                if self.current_syntax() != GreaterThan {
                    let mut r = Vec::from(recovery);
                    r.push(GreaterThan);
                    let s = self.unexpected_syntax(current, GreaterThan, &r);

                    if s.is_end() {
                        self.builder.finish_node();
                        return s;
                    }
                }

                self.bump_space();
                self.expression(recovery);

                self.builder.finish_node();
            }
            _ => {
                let s = self.value(recovery);
                if s.is_end() {
                    return s;
                }
            }
        }

        self.current_syntax();
        if self.peek_space_skipped() == Question {
            self.skip_space();
            self.builder.start_node_at(start, Ternary.into());
            self.bump_space();
            let mut r = Vec::from(recovery);
            r.push(Colon);
            self.expression(&r);
            self.skip_space();

            let current = self.current_span_location();
            if self.current_syntax() != Colon {
                let s = self.unexpected_syntax(current, Colon, &r);
                if s.is_end() {
                    self.builder.finish_node();
                    return s;
                }
            }
            self.bump_space();
            self.expression(recovery);
            self.builder.finish_node();
        }

        ParseRecovery::Ok
    }

    fn value(&mut self, recovery: &[SyntaxKind]) -> ParseRecovery {
        match self.current_syntax() {
            Number | Char | String | SemiColon | InKw | Identifier => self.bump(),
            Error => {
                let start = self.current_span_location();
                self.bump();
                let current = self.current_span_location();
                self.errors.push(Error::error(
                    ErrorKind::UnknownSyntax,
                    start..current,
                    self.source,
                ));
            }
            OpenParen => {
                self.bump_space();
                self.expression(recovery);

                let mut s = ParseRecovery::Ok;
                if self.current_syntax() != CloseParen {
                    let current = self.current_span_location();
                    s = self.unexpected_syntax(current, CloseParen, recovery);
                }

                self.bump();
                return s;
            }
            OpenBrace => {
                self.builder.start_node(Pointer.into());
                self.bump_space();
                self.expression(recovery);

                let mut s = ParseRecovery::Ok;
                if self.current_syntax() != CloseBrace {
                    let current = self.current_span_location();
                    s = self.unexpected_syntax(current, CloseParen, recovery);
                }
                self.bump();
                self.builder.finish_node();
                return s;
            }
            _ => {
                let current = self.current_span_location();
                self.unexpected_syntax(current, Value, &[]);
            }
        }
        ParseRecovery::Ok
    }

    fn kind(&mut self, recovery: &[SyntaxKind]) -> ParseRecovery {
        match self.current_syntax() {
            ByteKw | Identifier | SemiColon => {
                self.builder.start_node(Kind.into());
                self.bump();
                self.builder.finish_node();
                ParseRecovery::Ok
            }
            And | Mul => {
                self.builder.start_node(Kind.into());
                self.bump_space();
                let s = self.kind(recovery);
                self.builder.finish_node();
                s
            }
            _ => {
                let current = self.current_span_location();
                self.unexpected_syntax(current, Kind, recovery)
            }
        }
    }

    /// Creates an UnexpectedSyntax error
    fn unexpected_syntax(
        &mut self,
        start: usize,
        expected: SyntaxKind,
        skip_until: &[SyntaxKind],
    ) -> ParseRecovery {
        self.builder.start_node(Error.into());
        self.bump();
        let current = self.current_span_location();
        self.errors.push(Error::error(
            ErrorKind::UnexpectedSyntax { expected },
            start..current,
            self.source,
        ));
        let s = self.skip_till_syntax(skip_until);
        self.builder.finish_node();
        s
    }

    /// Skip Whitespace and Comments
    fn skip_space(&mut self) {
        self.skip_syntax(&[Whitespace, Comment])
    }

    fn peek_space_skipped(&mut self) -> SyntaxKind {
        self.tokens
            .peek_till_cloned(|(kind, _)| ![Comment, Whitespace].contains(kind))
            .map(|(kind, _)| kind)
            .unwrap_or(Eof)
    }

    /// Skips until the specified syntax is encountered.
    /// Returns true if the syntax was found else false if eof was found
    fn skip_till_syntax(&mut self, syntaxs: &[SyntaxKind]) -> ParseRecovery {
        loop {
            match self.current_syntax() {
                Eof => return ParseRecovery::Eof,
                SemiColon => return ParseRecovery::SemiColon,
                s => {
                    if let Some((i, _)) =
                        syntaxs.iter().enumerate().find(|(_, &syntax)| syntax == s)
                    {
                        return ParseRecovery::Recovered(i);
                    }
                }
            }
            self.bump();
        }
    }

    /// Skips until the current syntax is none of the specified syntaxs
    fn skip_syntax(&mut self, syntaxs: &[SyntaxKind]) {
        while syntaxs.contains(&self.current_syntax()) {
            self.bump();
        }
    }

    /// Consume current syntax and push to builder.
    fn bump(&mut self) {
        if let Some((kind, span)) = self.tokens.next() {
            self.builder.token(kind.into(), &self.source[span]);
        }
    }

    fn bump_space(&mut self) -> SyntaxKind {
        self.bump();
        self.skip_space();
        self.current_syntax()
    }

    /// Peeks the current syntax
    fn current_syntax(&mut self) -> SyntaxKind {
        self.tokens.peek(1).map(|&(kind, _)| kind).unwrap_or(Eof)
    }

    fn current_span_location(&mut self) -> usize {
        self.tokens
            .peek(1)
            .map(|(_, span)| span.start)
            .unwrap_or(self.source.len())
    }
}
