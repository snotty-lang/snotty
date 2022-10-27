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
    fn has_not_recovered_by(&self, n: usize) -> bool {
        match self {
            ParseRecovery::Ok => false,
            &ParseRecovery::Recovered(r) => r > n,
            _ => true,
        }
    }
}

enum Action {
    Return(ParseRecovery),
    Found,
    NotFound(ParseRecovery),
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
        assert!(self.recovery.is_empty(), "{:?}", self.recovery);
        self.builder.finish_node();

        Parse {
            green_node: self.builder.finish(),
            errors: self.errors,
        }
    }

    fn statement(&mut self) -> ParseRecovery {
        match self.current_syntax() {
            LetKw | ConstKw => {
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
                self.recovery.extend([CloseParen, OpenParen]);

                if self.bump_space() != OpenParen {
                    let s = self.unexpected_syntax(OpenParen);
                    if s.has_not_recovered_by(1) {
                        self.recovery.drain(self.recovery.len() - 2..);
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
                        self.recovery.pop();
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
                        self.recovery.pop();
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
                        self.recovery.pop();
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
                    if s.has_not_recovered_by(0) {
                        self.recovery.pop();
                        self.builder.finish_node();
                        self.bump_space();
                        return s;
                    }
                }
                self.bump_space();
                self.recovery.pop();

                let s = self.statement();
                self.builder.finish_node();
                return s;
            }
            IfKw => {
                self.builder.start_node(If.into());
                self.bump_space();
                self.recovery.push(ElseKw);
                let s = self.expression();
                if s.has_not_recovered_by(0) {
                    self.recovery.pop();
                    self.builder.finish_node();
                    return s;
                } else if s != ParseRecovery::Recovered(0) {
                    self.skip_space();
                    let s = self.statement();
                    if s.has_not_recovered_by(0) {
                        self.recovery.pop();
                        self.builder.finish_node();
                        return s;
                    }
                }

                self.skip_space();
                self.recovery.pop();
                let mut s = ParseRecovery::Ok;

                if self.current_syntax() == ElseKw {
                    self.bump_space();
                    s = self.statement();
                }
                self.builder.finish_node();
                return s;
            }
            _ => {
                if let Action::Return(s) = self.expect_func(Self::expression, 0, false) {
                    return s;
                }
            }
        }
        self.skip_syntax(&[Whitespace, Comment, SemiColon]);
        ParseRecovery::Ok
    }

    fn expression(&mut self) -> ParseRecovery {
        let start = self.builder.checkpoint();
        if let Action::Return(s) =
            self.expect_func(|p| p.binary_op(Self::comparison, &[And, Or, Xor]), 0, false)
        {
            return s;
        }

        if self.peek_space_skipped() == Question {
            self.skip_space();
            self.builder.start_node_at(start, Ternary.into());
            self.recovery.push(Colon);
            self.bump_space();
            self.expression();
            self.skip_space();
            if let Action::Return(s) = self.expect(Colon, 1, true) {
                return s;
            }
            self.recovery.pop();
            self.skip_space();
            self.expression();
            self.builder.finish_node();
        }

        ParseRecovery::Ok
    }

    fn comparison(&mut self) -> ParseRecovery {
        match self.current_syntax() {
            Not => {
                self.builder.start_node(UnaryOp.into());
                self.bump_space();
                let s = self.expression();
                self.builder.finish_node();
                s
            }
            _ => self.binary_op(
                Self::bitwise,
                &[
                    Equal,
                    NotEqual,
                    GreaterThan,
                    GreaterEqual,
                    LessThan,
                    LessEqual,
                ],
            ),
        }
    }

    fn bitwise(&mut self) -> ParseRecovery {
        self.binary_op(Self::arithmetic, &[Shr, Shl])
    }

    fn arithmetic(&mut self) -> ParseRecovery {
        self.binary_op(Self::term, &[Add, Sub])
    }

    fn term(&mut self) -> ParseRecovery {
        self.binary_op(Self::factor, &[Mul, Div, Mod])
    }

    fn factor(&mut self) -> ParseRecovery {
        match self.current_syntax() {
            Sub | Add | Inc | Dec => {
                self.builder.start_node(UnaryOp.into());
                self.bump_space();
                let s = self.factor();
                self.builder.finish_node();
                s
            }
            _ => {
                self.recovery.extend([Inc, Dec]);
                let start = self.builder.checkpoint();
                if let Action::Return(s) = self.expect_func(Self::cast, 2, false) {
                    return s;
                }
                if matches!(self.peek_space_skipped(), Inc | Dec) {
                    self.builder.start_node_at(start, UnaryOp.into());
                    self.skip_space();
                    self.bump();
                    self.builder.finish_node();
                }
                self.recovery.pop();
                self.recovery.pop();
                ParseRecovery::Ok
            }
        }
    }

    fn cast(&mut self) -> ParseRecovery {
        match self.current_syntax() {
            LessThan => {
                self.builder.start_node(Cast.into());
                self.recovery.push(GreaterThan);
                self.bump_space();
                if let Action::Return(s) = self.expect_func(Self::kind, 1, true) {
                    return s;
                }
                self.skip_space();
                if let Action::Return(s) = self.expect(GreaterThan, 1, true) {
                    return s;
                }
                self.recovery.pop();
                self.skip_space();
                let s = self.call();
                self.builder.finish_node();
                s
            }
            _ => self.call(),
        }
    }

    fn call(&mut self) -> ParseRecovery {
        let start = self.builder.checkpoint();
        self.recovery.push(OpenParen);
        if let Action::Return(s) = self.expect_func(Self::value, 1, false) {
            return s;
        }
        self.recovery.pop();
        if self.peek_space_skipped() == OpenParen {
            self.builder.start_node_at(start, Call.into());
            self.skip_space();
            self.bump_space();
            self.recovery.extend([CloseParen, Comma]);
            while self.current_syntax() != CloseParen {
                match self.expression() {
                    ParseRecovery::Ok | ParseRecovery::Recovered(1) => (),
                    ParseRecovery::Recovered(0) => break,
                    s => return s,
                }
                self.skip_space();
                if self.current_syntax() != Comma {
                    break;
                }
                self.bump_space();
            }
            self.recovery.pop();
            if self.current_syntax() != CloseParen {
                match self.unexpected_syntax(CloseParen) {
                    ParseRecovery::Recovered(0) => (),
                    s => return s,
                }
            }
            self.recovery.pop();
            self.bump();
            self.builder.finish_node();
        }
        ParseRecovery::Ok
    }

    fn value(&mut self) -> ParseRecovery {
        match self.current_syntax() {
            Number | Char | String | SemiColon | InKw | Identifier => {
                self.bump();
                ParseRecovery::Ok
            }
            Error => {
                let span = self.current_syntax_span();
                self.bump();
                self.errors
                    .push(Error::error(ErrorKind::UnknownSyntax, span, self.source));
                ParseRecovery::Ok
            }
            OpenParen => {
                self.bump_space();
                self.recovery.push(CloseParen);
                if let Action::Return(s) = self.expect_func(Self::expression, 1, false) {
                    return s;
                }
                if let Action::Return(s) = self.expect(CloseParen, 1, false) {
                    return s;
                }
                self.recovery.pop();
                self.bump();
                ParseRecovery::Ok
            }
            OpenBrace => {
                self.builder.start_node(Pointer.into());
                self.recovery.push(CloseBrace);
                self.bump_space();
                if let Action::Return(s) = self.expect_func(Self::expression, 1, true) {
                    return s;
                }
                if let Action::Return(s) = self.expect(CloseBrace, 1, true) {
                    return s;
                }
                self.bump();
                self.recovery.pop();
                self.builder.finish_node();
                ParseRecovery::Ok
            }
            And | Mul => {
                self.builder.start_node(UnaryOp.into());
                self.bump_space();
                let s = self.value();
                self.builder.finish_node();
                s
            }
            _ => self.unexpected_syntax(Value),
        }
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

    /// Peforms binary operations over a set of operators for the fiven function
    fn binary_op(
        &mut self,
        func: fn(&mut Self) -> ParseRecovery,
        ops: &[SyntaxKind],
    ) -> ParseRecovery {
        let start = self.builder.checkpoint();
        self.recovery.extend(ops);
        if let Action::Return(s) = self.expect_func(func, ops.len(), false) {
            return s;
        }
        while ops.contains(&self.peek_space_skipped()) {
            self.builder.start_node_at(start, BinaryOp.into());
            self.skip_space();
            self.bump_space();
            if let Action::Return(s) = self.expect_func(func, ops.len(), true) {
                return s;
            }
            self.builder.finish_node();
        }
        self.recovery.drain(self.recovery.len() - ops.len()..);
        ParseRecovery::Ok
    }

    /// Checks for a token.
    /// If the token was found, bumps to the next one, else finishes node and cleans recovery
    /// Returns the Action to be performed
    fn expect(&mut self, expect: SyntaxKind, n: usize, node: bool) -> Action {
        if self.current_syntax() != expect {
            let s = self.unexpected_syntax(expect);
            if s.has_not_recovered_by(n) {
                self.recovery.drain(self.recovery.len() - n..);
                if node {
                    self.builder.finish_node();
                }
                return Action::Return(s);
            }
            Action::NotFound(s)
        } else {
            self.bump();
            Action::Found
        }
    }

    /// Checks if the function parsed or not
    /// Returns the Action to be performed
    fn expect_func(
        &mut self,
        func: fn(&mut Self) -> ParseRecovery,
        n: usize,
        node: bool,
    ) -> Action {
        let s = func(self);
        match s {
            ParseRecovery::Eof | ParseRecovery::SemiColon => {
                if node {
                    self.builder.finish_node()
                }
                self.recovery.drain(self.recovery.len() - n..);
                Action::Return(s)
            }
            ParseRecovery::Recovered(r) if r > n => {
                if node {
                    self.builder.finish_node()
                }
                self.recovery.drain(self.recovery.len() - n..);
                Action::Return(s)
            }
            ParseRecovery::Recovered(_) => Action::NotFound(s),
            ParseRecovery::Ok => Action::Found,
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
