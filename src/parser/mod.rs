pub mod syntax;

use crate::{
    error::{Error, ErrorKind},
    Spanned,
};
use syntax::{Parse, SyntaxKind};
use SyntaxKind::*;

use logos::Logos;
use rowan::GreenNodeBuilder;

pub type ParseResult = Result<(), ErrorKind>;

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
            if let ParseResult::Err(kind) = self.statement() {
                let Spanned { span, .. } = self.current().unwrap();
                self.errors
                    .push(Error::error(kind, span.clone(), self.source));
                self.builder.start_node(Error.into());
                self.bump();
                self.builder.finish_node();
            }
        }

        self.skip_space();
        self.builder.finish_node();

        Parse {
            green_node: self.builder.finish(),
            errors: self.errors,
        }
    }

    fn statement(&mut self) -> ParseResult {
        self.skip_space();
        match self.current_syntax() {
            IfKw => {
                self.builder.start_node(IfKw.into());
                self.bump();
                self.builder.finish_node();
            }
            LetKw => {
                self.builder.start_node(LetKw.into());
                let ident = self.bump_space();
                if ident != Identifier {
                    let start = self.current_span_location();
                    self.builder.start_node(Error.into());
                    self.skip_till_syntax(Assign);
                    self.builder.finish_node();
                    self.errors.push(Error::error(
                        ErrorKind::UnexpectedSyntax {
                            expected: Identifier,
                        },
                        start..self.current_span_location(),
                        self.source,
                    ));
                }
                self.bump();
                self.builder.finish_node();
            }
            _ => return self.expression(),
        }

        Ok(())
    }

    fn expression(&self) -> ParseResult {
        todo!()
    }

    /// Skip Whitespace and Comments
    fn skip_space(&mut self) {
        while matches!(self.current_syntax(), Whitespace | Comment) {
            self.bump();
        }
    }

    /// Skips until the specified syntax is encountered.
    /// Returns true if the syntax was found else false if eof was found
    fn skip_till_syntax(&mut self, syntax: SyntaxKind) -> bool {
        while match self.current_syntax() {
            Eof => return false,
            s => s != syntax,
        } {
            self.bump();
        }
        true
    }

    /// Consume current syntax and push to builder.
    /// Returns false if the token is Eof
    fn bump(&mut self) -> bool {
        if let Some(Spanned { span, value }) = self.tokens.pop() {
            self.builder.token(value.into(), &self.source[span]);
            return true;
        }
        false
    }

    fn bump_space(&mut self) -> SyntaxKind {
        self.bump();
        self.skip_space();
        self.current_syntax()
    }

    fn current(&self) -> Option<&Spanned<SyntaxKind>> {
        self.tokens.last()
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
