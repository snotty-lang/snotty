mod peekable;
pub mod syntax;

use crate::{
    error::{Error, ErrorKind},
    Span,
};
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

    fn is_not_ok(&self) -> bool {
        *self != ParseRecovery::Ok
    }

    fn has_not_recovered_by(&self, n: usize) -> bool {
        match self {
            ParseRecovery::Ok => false,
            &ParseRecovery::Recovered(r) => r > n,
            _ => true,
        }
    }
}

pub struct Parser<'a> {
    source: &'a str,
    tokens: Peekable<SpannedIter<'a, SyntaxKind>>,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<Error<'a>>,
    recovery: Vec<SyntaxKind>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Parser {
        Self {
            source,
            tokens: Peekable::new(SyntaxKind::lexer(source).spanned()),
            builder: GreenNodeBuilder::new(),
            errors: Vec::new(),
            recovery: Vec::new(),
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

    fn statement(&mut self) -> ParseRecovery {
        match self.current_syntax() {
            LetKw => {
                self.builder.start_node(Let.into());
                self.recovery.push(Assign);
                if self.bump_space() != Identifier {
                    let s = self.unexpected_syntax(Identifier);
                    if s.has_not_recovered_by(0) {
                        self.recovery.pop();
                        self.builder.finish_node();
                        return s;
                    }
                } else {
                    self.bump_space();
                }
                if self.current_syntax() != Assign {
                    let s = self.unexpected_syntax(Assign);
                    if s.has_not_recovered_by(0) {
                        self.recovery.pop();
                        self.builder.finish_node();
                        return s;
                    }
                }
                self.recovery.pop();

                self.bump_space();
                self.expression();
                self.builder.finish_node();
            }
            a @ (OutKw | ReturnKw) => {
                self.builder.start_node(a.into());
                self.bump_space();
                self.expression();
                self.builder.finish_node();
            }
            FileKw => {
                self.builder.start_node(FileKw.into());
                self.bump_space();
                if self.current_syntax() != String {
                    self.unexpected_syntax(String);
                }
                self.bump();
                self.builder.finish_node();
            }
            OpenBrace => {
                self.builder.start_node(Scope.into());
                self.recovery.push(CloseBrace);
                self.bump_space();
                loop {
                    match self.current_syntax() {
                        Eof => {
                            self.builder.start_node(Error.into());
                            let current = self.current_syntax_location();
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
                        _ => {
                            self.statement();
                        }
                    }
                }
                self.recovery.pop();
                self.builder.finish_node();
                return ParseRecovery::Ok;
            }
            LoopKw => {
                self.builder.start_node(Loop.into());
                self.recovery
                    .extend([CloseBrace, OpenBrace, CloseParen, OpenParen]);

                if self.bump_space() != OpenParen {
                    let s = self.unexpected_syntax(OpenParen);
                    if s.has_not_recovered_by(4) {
                        self.recovery.drain(self.recovery.len() - 4..);
                        self.builder.finish_node();
                        self.bump_space();
                        return s;
                    }
                }
                self.bump_space();
                self.recovery.pop();

                if self.current_syntax() != SemiColon {
                    let s = self.statement();
                    if matches!(s, ParseRecovery::Recovered(4..) | ParseRecovery::Eof) {
                        self.recovery.drain(self.recovery.len() - 3..);
                        self.builder.finish_node();
                        self.bump_space();
                        return s;
                    }
                } else {
                    self.bump();
                }
                self.skip_space();

                if self.current_syntax() != SemiColon {
                    let s = self.expression();
                    if matches!(s, ParseRecovery::Recovered(4..) | ParseRecovery::Eof) {
                        self.recovery.drain(self.recovery.len() - 3..);
                        self.builder.finish_node();
                        self.bump_space();
                        return s;
                    }
                } else {
                    self.bump();
                }
                self.skip_space();

                if self.current_syntax() != SemiColon {
                    let s = self.statement();
                    if matches!(s, ParseRecovery::Recovered(4..) | ParseRecovery::Eof) {
                        self.recovery.drain(self.recovery.len() - 3..);
                        self.builder.finish_node();
                        self.bump_space();
                        return s;
                    }
                } else {
                    self.bump();
                }
                self.skip_space();

                if self.current_syntax() != CloseParen {
                    let s = self.unexpected_syntax(CloseParen);
                    if s.is_end() {
                        self.recovery.drain(self.recovery.len() - 3..);
                        self.builder.finish_node();
                        self.bump_space();
                        return s;
                    }
                }
                self.bump_space();
                self.recovery.pop();

                self.builder.start_node(Scope.into());
                if self.current_syntax() != OpenBrace {
                    let s = self.unexpected_syntax(OpenBrace);
                    if s.is_end() || matches!(s, ParseRecovery::Recovered(1)) {
                        self.bump();
                        self.recovery.drain(self.recovery.len() - 2..);
                        self.builder.finish_node();
                        self.builder.finish_node();
                        return s;
                    }
                }
                self.bump_space();
                self.recovery.pop();

                loop {
                    match self.current_syntax() {
                        Eof => {
                            self.builder.start_node(Error.into());
                            let current = self.current_syntax_location();
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
                        _ => {
                            let s = self.statement();
                            if s.has_not_recovered_by(0) {
                                self.recovery.pop();
                                self.builder.finish_node();
                                self.builder.finish_node();
                                return s;
                            }
                        }
                    }
                }

                self.recovery.pop();
                self.builder.finish_node();
                self.builder.finish_node();
                return ParseRecovery::Ok;
            }
            _ => {
                let s = self.expression();
                if s.is_not_ok() {
                    return s;
                }
            }
        }

        let end = self.current_syntax_location();
        self.skip_space();
        if self.current_syntax() != SemiColon {
            self.errors.push(Error::error(
                ErrorKind::MissingSemicolon,
                end..end + 1,
                self.source,
            ))
        }

        self.skip_syntax(&[Whitespace, Comment, SemiColon]);
        ParseRecovery::Ok
    }

    fn expression(&mut self) -> ParseRecovery {
        let start = self.builder.checkpoint();
        match self.current_syntax() {
            LessThan => {
                self.builder.start_node(Cast.into());
                self.recovery.push(GreaterThan);
                self.bump_space();

                let s = self.kind();
                if s.has_not_recovered_by(0) {
                    self.builder.finish_node();
                    return s;
                }

                self.skip_space();

                if self.current_syntax() != GreaterThan {
                    let s = self.unexpected_syntax(GreaterThan);
                    if s.has_not_recovered_by(0) {
                        self.recovery.pop();
                        self.builder.finish_node();
                        return s;
                    }
                }
                self.recovery.pop();

                self.bump_space();
                self.expression();

                self.builder.finish_node();
            }
            _ => {
                let s = self.value();
                if s.is_not_ok() {
                    return s;
                }
            }
        }

        if self.peek_space_skipped() == Question {
            self.skip_space();
            self.builder.start_node_at(start, Ternary.into());
            self.recovery.push(Colon);
            self.bump_space();
            self.expression();
            self.skip_space();

            if self.current_syntax() != Colon {
                let s = self.unexpected_syntax(Colon);
                if s.has_not_recovered_by(0) {
                    self.recovery.pop();
                    self.builder.finish_node();
                    return s;
                }
            }
            self.recovery.pop();
            self.bump_space();
            self.expression();
            self.builder.finish_node();
        }

        ParseRecovery::Ok
    }

    fn value(&mut self) -> ParseRecovery {
        match self.current_syntax() {
            Number | Char | String | SemiColon | InKw | Identifier => self.bump(),
            Error => {
                let span = self.current_syntax_span();
                self.bump();
                self.errors
                    .push(Error::error(ErrorKind::UnknownSyntax, span, self.source));
            }
            OpenParen => {
                self.bump_space();
                self.recovery.push(CloseParen);
                self.expression();

                let mut s = ParseRecovery::Ok;
                if self.current_syntax() != CloseParen {
                    s = self.unexpected_syntax(CloseParen);
                }

                self.recovery.pop();
                self.bump();
                return s;
            }
            OpenBrace => {
                self.builder.start_node(Pointer.into());
                self.recovery.push(CloseBrace);
                self.bump_space();
                self.expression();

                let mut s = ParseRecovery::Ok;
                if self.current_syntax() != CloseBrace {
                    s = self.unexpected_syntax(CloseBrace);
                }
                self.bump();
                self.recovery.pop();
                self.builder.finish_node();
                return s;
            }
            _ => {
                self.unexpected_syntax(Value);
            }
        }
        ParseRecovery::Ok
    }

    fn kind(&mut self) -> ParseRecovery {
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
                let s = self.kind();
                self.builder.finish_node();
                s
            }
            _ => self.unexpected_syntax(Kind),
        }
    }

    /// Creates an UnexpectedSyntax error
    fn unexpected_syntax(&mut self, expected: SyntaxKind) -> ParseRecovery {
        self.builder.start_node(Error.into());
        let span = self.current_syntax_span();
        self.bump();
        self.errors.push(Error::error(
            ErrorKind::UnexpectedSyntax { expected },
            span,
            self.source,
        ));
        let s = self.recover();
        self.builder.finish_node();
        s
    }

    /// Skip Whitespace and Comments
    fn skip_space(&mut self) {
        self.skip_syntax(&[Whitespace, Comment])
    }

    /// Peek the syntax after whitespace
    fn peek_space_skipped(&mut self) -> SyntaxKind {
        self.tokens
            .peek_till_cloned(|(kind, _)| ![Comment, Whitespace].contains(kind))
            .map(|(kind, _)| kind)
            .unwrap_or(Eof)
    }

    /// Bumps till a recoverable syntax is found
    fn recover(&mut self) -> ParseRecovery {
        loop {
            match self.current_syntax() {
                Eof => return ParseRecovery::Eof,
                SemiColon => return ParseRecovery::SemiColon,
                s => {
                    if let Some((i, _)) = self
                        .recovery
                        .iter()
                        .rev()
                        .enumerate()
                        .find(|(_, &syntax)| syntax == s)
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

    /// Bump and then skip space. Returns the current syntax
    fn bump_space(&mut self) -> SyntaxKind {
        self.bump();
        self.skip_space();
        self.current_syntax()
    }

    /// Peeks the current syntax
    fn current_syntax(&mut self) -> SyntaxKind {
        self.tokens.peek(1).map(|&(kind, _)| kind).unwrap_or(Eof)
    }

    /// Current location of the parser
    fn current_syntax_location(&mut self) -> usize {
        self.tokens
            .peek(1)
            .map(|(_, span)| span.start)
            .unwrap_or(self.source.len())
    }

    /// Current location of the parser
    fn current_syntax_span(&mut self) -> Span {
        self.tokens
            .peek(1)
            .map(|(_, span)| span.clone())
            .unwrap_or(self.source.len() - 1..self.source.len())
    }
}
