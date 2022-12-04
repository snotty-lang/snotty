pub mod syntax;

use std::iter::Peekable;

use crate::{
    error::{Error, ErrorKind},
    Span,
};

use syntax::{ParseResult, ParseTreeBuilder, SyntaxKind};
use SyntaxKind::*;

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
    p_loc: usize,
    s_loc: usize,
    e_loc: usize,
    tokens: Peekable<SpannedIter<'a, SyntaxKind>>,
    builder: ParseTreeBuilder,
    errors: Vec<Error<'a>>,
    recovery: Vec<SyntaxKind>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Parser {
        let mut tokens = SyntaxKind::lexer(source).spanned().peekable();
        Self {
            source,
            p_loc: 0,
            s_loc: tokens
                .peek()
                .map(|&(_, Span { start, .. })| start)
                .unwrap_or(0),
            e_loc: tokens
                .peek()
                .map(|&(_, Span { end, .. })| end)
                .unwrap_or(source.len()),
            tokens,
            builder: ParseTreeBuilder::new(),
            errors: Vec::new(),
            recovery: Vec::new(),
        }
    }

    pub fn parse(mut self) -> ParseResult<'a> {
        self.builder.start_node(Root, 0);

        while self.current_syntax() != Eof {
            let s = self.statement();
            assert!(matches!(s, ParseRecovery::Ok | ParseRecovery::Eof), "{s:?}");
        }

        assert_eq!(self.current_syntax(), Eof);
        assert!(self.recovery.is_empty(), "{:?}", self.recovery);
        self.builder.finish_node(self.e_loc, |_| None);

        ParseResult {
            parse: self.builder.finish(),
            errors: self.errors,
        }
    }

    fn statement(&mut self) -> ParseRecovery {
        self.builder.start_node(Statement, self.s_loc);
        let s = match self.current_syntax() {
            LetKw => {
                self.builder.start_node(Let, self.s_loc);
                self.recovery.push(Assign);
                self.pass();
                if let ParseAction::Return(s) = self.expect(&[Identifier], 1, 2, true) {
                    return s;
                }

                if let ParseAction::Return(s) = self.expect(&[Assign], 1, 2, false) {
                    return s;
                }
                self.recovery.pop();

                let s = self.expression();
                self.builder.finish_node(self.e_loc, |_| None);
                s
            }
            ReturnKw => {
                self.builder.start_node(ReturnKw, self.s_loc);
                self.pass();
                let s = self.expression();
                self.builder.finish_node(self.e_loc, |_| None);
                s
            }
            FileKw => {
                self.builder.start_node(FileKw, self.s_loc);
                self.pass();
                self.recovery.push(String);
                if let ParseAction::Return(s) = self.expect(&[String], 1, 2, true) {
                    return s;
                }
                self.recovery.pop();
                self.builder.finish_node(self.e_loc, |_| None);
                ParseRecovery::Ok
            }
            OpenBrace => {
                self.builder.start_node(Scope, self.s_loc);
                self.recovery.push(CloseBrace);
                self.pass();
                loop {
                    match self.current_syntax() {
                        Eof => {
                            self.builder.start_node(Error, self.s_loc);
                            let current = self.s_loc;
                            self.errors.push(Error::error(
                                ErrorKind::UnexpectedSyntax {
                                    expected: vec![CloseBrace],
                                },
                                current + 1..0,
                                self.source,
                            ));
                            self.builder.finish_node(self.e_loc, |_| None);
                            break;
                        }
                        CloseBrace => {
                            self.pass();
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
                self.builder.finish_node(self.e_loc, |_| None);
                ParseRecovery::Ok
            }
            LoopKw => {
                self.builder.start_node(Loop, self.s_loc);
                self.recovery.extend([CloseParen, OpenParen]);
                self.pass();
                if let ParseAction::Return(s) = self.expect(&[OpenParen], 2, 2, false) {
                    return s;
                }
                self.recovery.pop();

                for syntax in [Self::statement, Self::expression] {
                    if self.current_syntax() != SemiColon {
                        let s = syntax(self);
                        if matches!(s, ParseRecovery::Recovered(2..) | ParseRecovery::Eof) {
                            self.recovery.pop();
                            self.builder.finish_node(self.e_loc, |_| None);
                            self.builder.finish_node(self.e_loc, |_| None);
                            self.bump();
                            return s;
                        }
                    }

                    if let ParseAction::Return(s) = self.expect(&[SemiColon], 1, 2, true) {
                        return s;
                    }
                }

                if self.current_syntax() != CloseParen {
                    let s = self.statement();
                    if matches!(s, ParseRecovery::Recovered(2..) | ParseRecovery::Eof) {
                        self.recovery.pop();
                        self.builder.finish_node(self.e_loc, |_| None);
                        self.builder.finish_node(self.e_loc, |_| None);
                        self.bump();
                        return s;
                    }
                }

                if let ParseAction::Return(s) = self.expect(&[CloseParen], 1, 2, false) {
                    return s;
                }

                self.recovery.pop();

                let s = self.statement();
                self.builder.finish_node(self.e_loc, |_| None);
                s
            }
            IfKw => {
                self.builder.start_node(If, self.s_loc);
                self.pass();
                self.recovery.push(ElseKw);
                match self.expect_func(Self::expression, 1, 2) {
                    ParseAction::Return(s) => return s,
                    ParseAction::Found => {
                        if let ParseAction::Return(s) = self.expect_func(Self::statement, 1, 2) {
                            return s;
                        }
                    }
                    ParseAction::Recovered(_) => (),
                }

                self.recovery.pop();
                let mut s = ParseRecovery::Ok;
                if self.current_syntax() == ElseKw {
                    self.pass();
                    s = self.statement();
                }
                self.builder.finish_node(self.e_loc, |_| None);
                s
            }
            FxKw => {
                self.builder.start_node(Fx, self.s_loc);
                self.recovery.extend([CloseParen, OpenParen, Identifier]);
                self.pass();
                if let ParseAction::Return(s) = self.expect(&[Identifier], 3, 2, true) {
                    return s;
                }
                self.recovery.pop();

                if let ParseAction::Return(s) = self.expect(&[OpenParen], 2, 2, false) {
                    return s;
                }
                self.recovery.pop();

                while self.current_syntax() != CloseParen {
                    if let ParseAction::Return(s) = self.expect(&[Identifier], 1, 2, true) {
                        return s;
                    }
                }

                if let ParseAction::Return(s) = self.expect(&[CloseParen], 1, 2, false) {
                    return s;
                }
                self.recovery.pop();

                let s = self.statement();
                self.builder.finish_node(self.e_loc, |_| None);
                s
            }
            _ => {
                let checkpoint = self.builder.checkpoint(self.s_loc);
                self.recovery.extend(SyntaxKind::ASSIGNMENT);
                if let ParseAction::Return(s) = self.expect_func(Self::expression, 11, 1) {
                    return s;
                }
                if SyntaxKind::ASSIGNMENT.contains(&self.current_syntax()) {
                    self.builder.start_node_at(checkpoint, ReLet);
                    self.bump();

                    self.expression();
                    self.builder.finish_node(self.e_loc, |_| None);
                }
                self.recovery.drain(self.recovery.len() - 11..);
                ParseRecovery::Ok
            }
        };
        self.builder.finish_node(self.e_loc, |_| None);
        s
    }

    fn expression(&mut self) -> ParseRecovery {
        let start = self.builder.checkpoint(self.s_loc);
        if let ParseAction::Return(s) =
            self.expect_func(|p| p.binary_op(Self::comparison, &[And, Or, Xor]), 0, 0)
        {
            return s;
        }

        if self.current_syntax() == Question {
            self.builder.start_node_at(start, Ternary);
            self.recovery.push(Colon);
            self.pass();
            self.expression();

            if let ParseAction::Return(s) = self.expect(&[Colon], 1, 1, false) {
                return s;
            }
            self.recovery.pop();
            self.expression();
            self.builder.finish_node(self.e_loc, |_| None);
        }

        ParseRecovery::Ok
    }

    fn comparison(&mut self) -> ParseRecovery {
        match self.current_syntax() {
            Not => {
                self.builder.start_node(UnaryOp, self.s_loc);
                self.bump();
                let s = self.expression();
                self.builder.finish_node(self.e_loc, |_| None);
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
            Sub | Inc | Dec => {
                self.builder.start_node(UnaryOp, self.s_loc);
                self.bump();
                let s = self.factor();
                self.builder.finish_node(self.e_loc, |_| None);
                s
            }
            _ => {
                self.recovery.extend([Inc, Dec]);
                let start = self.builder.checkpoint(self.s_loc);
                if let ParseAction::Return(s) = self.expect_func(Self::cast, 2, 0) {
                    return s;
                }
                if matches!(self.current_syntax(), Inc | Dec) {
                    self.builder.start_node_at(start, UnaryOp);
                    self.bump();
                    self.builder.finish_node(self.e_loc, |_| None);
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
                self.builder.start_node(Cast, self.s_loc);
                self.recovery.push(GreaterThan);
                self.pass();
                if let ParseAction::Return(s) = self.expect_func(Self::kind, 1, 1) {
                    return s;
                }
                if let ParseAction::Return(s) = self.expect(&[GreaterThan], 1, 1, false) {
                    return s;
                }
                self.recovery.pop();
                let s = self.call();
                self.builder.finish_node(self.e_loc, |_| None);
                s
            }
            _ => self.call(),
        }
    }

    fn call(&mut self) -> ParseRecovery {
        let start = self.builder.checkpoint(self.s_loc);
        self.recovery.push(OpenParen);
        if let ParseAction::Return(s) = self.expect_func(Self::value, 1, 0) {
            return s;
        }
        self.recovery.pop();
        if self.current_syntax() == OpenParen {
            self.builder.start_node_at(start, Call);
            self.pass();
            self.recovery.extend([CloseParen, Comma]);
            while self.current_syntax() != CloseParen {
                match self.expect_func(Self::expression, 2, 1) {
                    ParseAction::Found | ParseAction::Recovered(1) => (),
                    ParseAction::Recovered(_) => break,
                    ParseAction::Return(s) => return s,
                }
                if self.current_syntax() != Comma {
                    break;
                }
                self.pass();
            }
            self.recovery.pop();
            if let ParseAction::Return(s) = self.expect(&[CloseParen], 1, 1, false) {
                return s;
            }
            self.recovery.pop();
            self.builder.finish_node(self.e_loc, |_| None);
        }
        ParseRecovery::Ok
    }

    fn value(&mut self) -> ParseRecovery {
        self.builder.start_node(Value, self.s_loc);
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
                self.pass();
                self.recovery.push(CloseParen);
                if let ParseAction::Return(s) = self.expect_func(Self::expression, 1, 0) {
                    return s;
                }
                if let ParseAction::Return(s) = self.expect(&[CloseParen], 1, 0, false) {
                    return s;
                }
                self.recovery.pop();
                ParseRecovery::Ok
            }
            OpenBrace => {
                self.builder.start_node(Pointer, self.s_loc);
                self.recovery.push(CloseBrace);
                self.pass();
                if let ParseAction::Return(s) = self.expect_func(Self::expression, 1, 1) {
                    return s;
                }
                if let ParseAction::Return(s) = self.expect(&[CloseBrace], 1, 1, false) {
                    return s;
                }
                self.recovery.pop();
                self.builder.finish_node(self.e_loc, |_| None);
                ParseRecovery::Ok
            }
            Mul => {
                self.builder.start_node(UnaryOp, self.s_loc);
                self.bump();
                let s = self.value();
                self.builder.finish_node(self.p_loc, |_| None);
                s
            }
            _ => self.unexpected_syntax(vec![Value]),
        };
        self.builder.finish_node(self.e_loc, |_| None);
        s
    }

    fn kind(&mut self) -> ParseRecovery {
        match self.current_syntax() {
            ByteKw | Identifier | SemiColon => {
                self.builder.start_node(Kind, self.s_loc);
                self.bump();
                self.builder.finish_node(self.e_loc, |_| None);
                ParseRecovery::Ok
            }
            Mul => {
                self.builder.start_node(UnaryOp, self.s_loc);
                self.bump();
                let s = self.kind();
                self.builder.finish_node(self.p_loc, |_| None);
                s
            }
            _ => self.unexpected_syntax(vec![Kind]),
        }
    }

    /// Peforms binary operations over a set of operators for the fiven function
    fn binary_op(
        &mut self,
        func: fn(&mut Self) -> ParseRecovery,
        ops: &[SyntaxKind],
    ) -> ParseRecovery {
        let start = self.builder.checkpoint(self.s_loc);
        self.recovery.extend(ops);
        if let ParseAction::Return(s) = self.expect_func(func, ops.len(), 0) {
            return s;
        }
        while ops.contains(&self.current_syntax()) {
            self.builder.start_node_at(start, BinaryOp);
            self.bump();
            if let ParseAction::Return(s) = self.expect_func(func, ops.len(), 1) {
                return s;
            }
            self.builder.finish_node(self.e_loc, |_| None);
        }
        self.recovery.drain(self.recovery.len() - ops.len()..);
        ParseRecovery::Ok
    }

    /// Checks for a token.
    /// If the token was found, bumps or passes to the next one, else finishes node and cleans recovery
    /// Returns the Action to be performed
    fn expect(
        &mut self,
        expect: &[SyntaxKind],
        current_recovery: usize,
        node: usize,
        bump: bool,
    ) -> ParseAction {
        if !expect.contains(&self.current_syntax()) {
            let s = self.unexpected_syntax(expect.to_vec());
            match s {
                ParseRecovery::Recovered(r) if r >= current_recovery => {
                    for _ in 0..node {
                        self.builder.finish_node(self.e_loc, |_| None);
                    }
                    self.recovery
                        .drain(self.recovery.len() - current_recovery..);
                    ParseAction::Return(s)
                }
                ParseRecovery::Eof | ParseRecovery::SemiColon => {
                    for _ in 0..node {
                        self.builder.finish_node(self.e_loc, |_| None);
                    }
                    self.recovery
                        .drain(self.recovery.len() - current_recovery..);
                    ParseAction::Return(s)
                }
                ParseRecovery::Recovered(s) => {
                    if bump {
                        self.bump()
                    } else {
                        self.pass()
                    }
                    ParseAction::Recovered(s)
                }
                ParseRecovery::Ok => unreachable!(),
            }
        } else {
            if bump {
                self.bump()
            } else {
                self.pass()
            }
            ParseAction::Found
        }
    }

    /// Checks if the function parsed or not
    /// Returns the Action to be performed
    fn expect_func(
        &mut self,
        func: fn(&mut Self) -> ParseRecovery,
        current_recovery: usize,
        node: usize,
    ) -> ParseAction {
        let s = func(self);
        match s {
            ParseRecovery::Recovered(r) if r >= current_recovery => {
                for _ in 0..node {
                    self.builder.finish_node(self.e_loc, |_| None);
                }
                self.recovery
                    .drain(self.recovery.len() - current_recovery..);
                ParseAction::Return(s)
            }
            ParseRecovery::Eof | ParseRecovery::SemiColon => {
                for _ in 0..node {
                    self.builder.finish_node(self.e_loc, |_| None);
                }
                self.recovery
                    .drain(self.recovery.len() - current_recovery..);
                ParseAction::Return(s)
            }
            ParseRecovery::Recovered(s) => ParseAction::Recovered(s),
            ParseRecovery::Ok => ParseAction::Found,
        }
    }

    /// Creates an UnexpectedSyntax error
    fn unexpected_syntax(&mut self, expected: Vec<SyntaxKind>) -> ParseRecovery {
        self.builder.start_node(Error, self.s_loc);
        let span = self.current_syntax_span();
        self.bump();
        self.errors.push(Error::error(
            ErrorKind::UnexpectedSyntax { expected },
            span,
            self.source,
        ));
        let s = self.recover();
        self.builder.finish_node(self.e_loc, |_| None);
        s
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

    /// Consumes current syntax and push to builder.
    fn bump(&mut self) {
        if let Some((kind, span)) = self.tokens.next() {
            self.builder.push(kind, span, |_| None);
        }
        self.p_loc = self.e_loc;
        self.s_loc = self.s_loc();
        self.e_loc = self.e_loc();
    }

    /// Consumes the current syntax
    fn pass(&mut self) {
        self.tokens.next();
        self.p_loc = self.e_loc;
        self.s_loc = self.s_loc();
        self.e_loc = self.e_loc();
    }

    /// Peeks the current syntax
    fn current_syntax(&mut self) -> SyntaxKind {
        self.tokens.peek().map(|&(kind, _)| kind).unwrap_or(Eof)
    }

    /// Current span of the syntax
    fn current_syntax_span(&mut self) -> Span {
        self.tokens
            .peek()
            .map(|(_, span)| span.clone())
            .unwrap_or(self.source.len()..self.source.len() + 1)
    }

    fn s_loc(&mut self) -> usize {
        self.tokens
            .peek()
            .map(|(_, span)| span.start)
            .unwrap_or(self.source.len())
    }

    fn e_loc(&mut self) -> usize {
        self.tokens
            .peek()
            .map(|(_, span)| span.end)
            .unwrap_or(self.source.len())
    }
}
