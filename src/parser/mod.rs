mod peekable;
pub mod syntax;

use crate::{
    error::{Error, ErrorKind},
    Span,
};
use peekable::Peekable;
use syntax::{Parse, SyntaxKind};
use SyntaxKind::*;

use cstree::GreenNodeBuilder;
use logos::{Logos, SpannedIter};

#[derive(Debug, PartialEq, Eq, Clone)]
enum ParseRecovery {
    Recovered(usize),
    SemiColon,
    Eof,
    Ok,
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum ParseAction {
    Return(ParseRecovery),
    Found,
    Recovered(usize),
}

pub struct Parser<'a> {
    source: &'a str,
    tokens: Peekable<SpannedIter<'a, SyntaxKind>>,
    builder: GreenNodeBuilder<'static, 'static>,
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
            self.current_syntax() != Eof
        } {
            let s = self.statement();
            assert!(matches!(s, ParseRecovery::Ok | ParseRecovery::Eof), "{s:?}");
        }

        assert_eq!(self.current_syntax(), Eof);
        assert!(self.recovery.is_empty(), "{:?}", self.recovery);
        self.builder.finish_node();

        Parse {
            green_node: self.builder.finish().0,
            errors: self.errors,
        }
    }

    fn statement(&mut self) -> ParseRecovery {
        self.builder.start_node(Statement.into());
        let s = match self.current_syntax() {
            LetKw | ConstKw => {
                self.builder.start_node(Let.into());
                self.recovery.push(Assign);
                self.bump_space();
                if let ParseAction::Return(s) = self.expect(Identifier, 1, 2) {
                    return s;
                }
                self.skip_space();
                if let ParseAction::Return(s) = self.expect(Assign, 1, 2) {
                    return s;
                }
                self.recovery.pop();
                self.skip_space();
                self.expression();
                self.builder.finish_node();
                ParseRecovery::Ok
            }
            a @ (OutKw | ReturnKw) => {
                self.builder.start_node(a.into());
                self.bump_space();
                self.expression();
                self.builder.finish_node();
                ParseRecovery::Ok
            }
            FileKw => {
                self.builder.start_node(FileKw.into());
                self.bump_space();
                self.recovery.push(String);
                if let ParseAction::Return(s) = self.expect(String, 1, 2) {
                    return s;
                }
                self.recovery.pop();
                self.builder.finish_node();
                ParseRecovery::Ok
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
                            if let ParseAction::Return(s) = self.expect_func(Self::statement, 1, 2)
                            {
                                return s;
                            }
                        }
                    }
                }
                self.recovery.pop();
                self.builder.finish_node();
                ParseRecovery::Ok
            }
            LoopKw => {
                self.builder.start_node(Loop.into());
                self.recovery.extend([CloseParen, OpenParen]);
                self.bump_space();
                if let ParseAction::Return(s) = self.expect(OpenParen, 2, 2) {
                    return s;
                }
                self.skip_space();
                self.recovery.pop();

                if self.current_syntax() != SemiColon {
                    let s = self.statement();
                    if matches!(s, ParseRecovery::Recovered(2..) | ParseRecovery::Eof) {
                        self.recovery.pop();
                        self.builder.finish_node();
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
                    if matches!(s, ParseRecovery::Recovered(2..) | ParseRecovery::Eof) {
                        self.recovery.pop();
                        self.builder.finish_node();
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
                    if matches!(s, ParseRecovery::Recovered(2..) | ParseRecovery::Eof) {
                        self.recovery.pop();
                        self.builder.finish_node();
                        self.builder.finish_node();
                        self.bump_space();
                        return s;
                    }
                } else {
                    self.bump();
                }
                self.skip_space();

                if let ParseAction::Return(s) = self.expect(CloseParen, 1, 2) {
                    return s;
                }
                self.skip_space();
                self.recovery.pop();

                let s = self.statement();
                self.builder.finish_node();
                s
            }
            IfKw => {
                self.builder.start_node(If.into());
                self.bump_space();
                self.recovery.push(ElseKw);
                match self.expect_func(Self::expression, 1, 2) {
                    ParseAction::Return(s) => return s,
                    ParseAction::Found => {
                        self.skip_space();
                        if let ParseAction::Return(s) = self.expect_func(Self::statement, 1, 2) {
                            return s;
                        }
                    }
                    ParseAction::Recovered(_) => (),
                }
                self.skip_space();
                self.recovery.pop();
                let mut s = ParseRecovery::Ok;
                if self.current_syntax() == ElseKw {
                    self.bump_space();
                    s = self.statement();
                }
                self.builder.finish_node();
                s
            }
            _ => {
                if let ParseAction::Return(s) = self.expect_func(Self::expression, 0, 1) {
                    s
                } else {
                    ParseRecovery::Ok
                }
            }
        };
        self.builder.finish_node();
        self.skip_syntax(&[Whitespace, Comment, SemiColon]);
        s
    }

    fn expression(&mut self) -> ParseRecovery {
        let start = self.builder.checkpoint();
        if let ParseAction::Return(s) =
            self.expect_func(|p| p.binary_op(Self::comparison, &[And, Or, Xor]), 0, 0)
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
            if let ParseAction::Return(s) = self.expect(Colon, 1, 1) {
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
                if let ParseAction::Return(s) = self.expect_func(Self::cast, 2, 0) {
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
                if let ParseAction::Return(s) = self.expect_func(Self::kind, 1, 1) {
                    return s;
                }
                self.skip_space();
                if let ParseAction::Return(s) = self.expect(GreaterThan, 1, 1) {
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
        if let ParseAction::Return(s) = self.expect_func(Self::value, 1, 0) {
            return s;
        }
        self.recovery.pop();
        if self.peek_space_skipped() == OpenParen {
            self.builder.start_node_at(start, Call.into());
            self.skip_space();
            self.bump_space();
            self.recovery.extend([CloseParen, Comma]);
            while self.current_syntax() != CloseParen {
                match self.expect_func(Self::expression, 2, 1) {
                    ParseAction::Found | ParseAction::Recovered(1) => (),
                    ParseAction::Recovered(_) => break,
                    ParseAction::Return(s) => return s,
                }
                self.skip_space();
                if self.current_syntax() != Comma {
                    break;
                }
                self.bump_space();
            }
            self.recovery.pop();
            if let ParseAction::Return(s) = self.expect(CloseParen, 1, 1) {
                return s;
            }
            self.recovery.pop();
            self.builder.finish_node();
        }
        ParseRecovery::Ok
    }

    fn value(&mut self) -> ParseRecovery {
        self.builder.start_node(Value.into());
        let s = match self.current_syntax() {
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
                if let ParseAction::Return(s) = self.expect_func(Self::expression, 1, 0) {
                    return s;
                }
                if let ParseAction::Return(s) = self.expect(CloseParen, 1, 0) {
                    return s;
                }
                self.recovery.pop();
                ParseRecovery::Ok
            }
            OpenBrace => {
                self.builder.start_node(Pointer.into());
                self.recovery.push(CloseBrace);
                self.bump_space();
                if let ParseAction::Return(s) = self.expect_func(Self::expression, 1, 1) {
                    return s;
                }
                if let ParseAction::Return(s) = self.expect(CloseBrace, 1, 1) {
                    return s;
                }
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
        };
        self.builder.finish_node();
        s
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
        if let ParseAction::Return(s) = self.expect_func(func, ops.len(), 0) {
            return s;
        }
        while ops.contains(&self.peek_space_skipped()) {
            self.builder.start_node_at(start, BinaryOp.into());
            self.skip_space();
            self.bump_space();
            if let ParseAction::Return(s) = self.expect_func(func, ops.len(), 1) {
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
    fn expect(&mut self, expect: SyntaxKind, n: usize, node: usize) -> ParseAction {
        if self.current_syntax() != expect {
            let s = self.unexpected_syntax(expect);
            match s {
                ParseRecovery::Eof | ParseRecovery::SemiColon => {
                    for _ in 0..node {
                        self.builder.finish_node();
                    }
                    self.recovery.drain(self.recovery.len() - n..);
                    ParseAction::Return(s)
                }
                ParseRecovery::Recovered(r) if r >= n => {
                    for _ in 0..node {
                        self.builder.finish_node();
                    }
                    self.recovery.drain(self.recovery.len() - n..);
                    ParseAction::Return(s)
                }
                ParseRecovery::Recovered(s) => {
                    self.bump();
                    ParseAction::Recovered(s)
                }
                ParseRecovery::Ok => unreachable!(),
            }
        } else {
            self.bump();
            ParseAction::Found
        }
    }

    /// Checks if the function parsed or not
    /// Returns the Action to be performed
    fn expect_func(
        &mut self,
        func: fn(&mut Self) -> ParseRecovery,
        n: usize,
        node: usize,
    ) -> ParseAction {
        let s = func(self);
        match s {
            ParseRecovery::Eof | ParseRecovery::SemiColon => {
                for _ in 0..node {
                    self.builder.finish_node()
                }
                self.recovery.drain(self.recovery.len() - n..);
                ParseAction::Return(s)
            }
            ParseRecovery::Recovered(r) if r >= n => {
                for _ in 0..node {
                    self.builder.finish_node()
                }
                self.recovery.drain(self.recovery.len() - n..);
                ParseAction::Return(s)
            }
            ParseRecovery::Recovered(s) => ParseAction::Recovered(s),
            ParseRecovery::Ok => ParseAction::Found,
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
