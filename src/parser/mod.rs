pub mod ast;
pub mod token;

use chumsky::prelude::*;

use crate::Spanned;

use ast::{Rule, AST};
use token::Token::{self, *};

pub fn parser() -> impl Parser<Token, AST, Error = Simple<Token>> {
    let expr = recursive(|expr| {
        let pointer = expr
            .clone()
            .delimited_by(just(OpenBrace), just(CloseBrace))
            .map_with_span(|_, span| {
                AST::from(Spanned {
                    span,
                    value: Rule::Pointer,
                })
            });

        let byte = just(Byte).map_with_span(|_, span| {
            AST::from(Spanned {
                span,
                value: Rule::Byte,
            })
        });

        let identifier = just(Identifier).map_with_span(|_, span| {
            AST::from(Spanned {
                span,
                value: Rule::Identifier,
            })
        });

        let none = just(SemiColon).map_with_span(|_, span| {
            AST::from(Spanned {
                span,
                value: Rule::None,
            })
        });

        let char = just(Char).map_with_span(|_, span| {
            AST::from(Spanned {
                span,
                value: Rule::Byte,
            })
        });

        let string = just(String).map_with_span(|_, span| {
            AST::from(Spanned {
                span,
                value: Rule::String,
            })
        });

        let input = just(In).map_with_span(|_, span| {
            AST::from(Spanned {
                span,
                value: Rule::Input,
            })
        });

        let value = byte
            .or(pointer)
            .or(char)
            .or(string)
            .or(none)
            .or(identifier)
            .or(input)
            .or(expr
                .clone()
                .delimited_by(just(OpenParen), just(CloseParen))
                .map_with_span(|_, span| {
                    AST::from(Spanned {
                        span,
                        value: Rule::Value,
                    })
                }));

        let kind = recursive(|kind: Recursive<Token, AST, _>| {
            just(ByteKw)
                .or(just(SemiColon))
                .or(just(Identifier))
                .map_with_span(|_, span| {
                    AST::from(Spanned {
                        span,
                        value: Rule::Kind,
                    })
                })
                .or(just(Fx)
                    .ignore_then(
                        kind.clone()
                            .separated_by(just(Comma))
                            .delimited_by(just(OpenParen), just(CloseParen)),
                    )
                    .then(just(Arrow).ignore_then(kind.clone()).or_not())
                    .map_with_span(|(mut params, ret), span| {
                        params.extend(ret);
                        AST {
                            rule: Spanned {
                                span,
                                value: Rule::Kind,
                            },
                            children: params,
                        }
                    }))
                .or(just(And).ignore_then(kind.clone()))
                .or(just(Mul).ignore_then(kind))
        });

        let cast = just(LessThan)
            .ignore_then(kind)
            .then_ignore(just(GreaterThan))
            .then(expr.clone())
            .map_with_span(|_, span| {
                AST::from(Spanned {
                    span,
                    value: Rule::TypeCast,
                })
            });

        let ternary = expr
            .clone()
            .then_ignore(just(Question))
            .then(expr.clone())
            .then_ignore(just(Colon))
            .then(expr)
            .map_with_span(|_, span| {
                AST::from(Spanned {
                    span,
                    value: Rule::Ternary,
                })
            });

        cast.or(ternary).or(value)
    });

    expr

    //     let op = |c| just(c).padded();

    //     let unary = op('-')
    //         .repeated()
    //         .then(atom)
    //         .foldr(|_op, rhs| Expr::Neg(Box::new(rhs)));

    //     let product = unary
    //         .clone()
    //         .then(
    //             op('*')
    //                 .to(Expr::Mul as fn(_, _) -> _)
    //                 .or(op('/').to(Expr::Div as fn(_, _) -> _))
    //                 .then(unary)
    //                 .repeated(),
    //         )
    //         .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

    //     let sum = product
    //         .clone()
    //         .then(
    //             op('+')
    //                 .to(Expr::Add as fn(_, _) -> _)
    //                 .or(op('-').to(Expr::Sub as fn(_, _) -> _))
    //                 .then(product)
    //                 .repeated(),
    //         )
    //         .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

    //     sum
    // });

    // let decl = recursive(|decl| {
    //     let r#let = text::keyword("let")
    //         .ignore_then(ident)
    //         .then_ignore(just('='))
    //         .then(expr.clone())
    //         .then_ignore(just(';'))
    //         .then(decl.clone())
    //         .map(|((name, rhs), then)| Expr::Let {
    //             name,
    //             rhs: Box::new(rhs),
    //             then: Box::new(then),
    //         });

    //     let r#fn = text::keyword("fn")
    //         .ignore_then(ident)
    //         .then(ident.repeated())
    //         .then_ignore(just('='))
    //         .then(expr.clone())
    //         .then_ignore(just(';'))
    //         .then(decl)
    //         .map(|(((name, args), body), then)| Expr::Fn {
    //             name,
    //             args,
    //             body: Box::new(body),
    //             then: Box::new(then),
    //         });

    //     r#let.or(r#fn).or(expr).padded()
    // });

    // decl.then_ignore(end())
}
