use std::{fmt, iter::Peekable};

use super::token::Token;
use logos::{SpannedIter, Logos};

pub enum S {
    Atom(u8),
    Cons(Token, Vec<S>),
}

impl fmt::Display for S {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            S::Atom(i) => write!(f, "{}", i),
            S::Cons(head, rest) => {
                write!(f, "({:?}", head)?;
                for s in rest {
                    write!(f, " {}", s)?
                }
                write!(f, ")")
            }
        }
    }
}

pub struct Parser<'source> {
    source: &'source str,
    lexer: Peekable<SpannedIter<'source, Token>>,
}

impl<'source> Parser<'source> {
    pub fn parse(source: &'source str) -> S {
        Self {
            source,
            lexer: Token::lexer(source).spanned().peekable(),
        }.expr_bp(0)
    }

    fn expr_bp(&mut self, min_bp: u8) -> S {
        let mut lhs = match self.lexer.next() {
            Some((Token::Byte(it), _)) => S::Atom(it),
            Some((Token::OpenParen, _)) => {
                let lhs = self.expr_bp(0);
                assert_eq!(self.lexer.next().unwrap().0, Token::CloseParen);
                lhs
            }
            Some((op, _)) => {
                let ((), r_bp) = Self::prefix_binding_power(&op);
                let rhs = self.expr_bp(r_bp);
                S::Cons(op, vec![rhs])
            }
            t => panic!("bad token: {:?}", t),
        };
    
        loop {
            let op = match self.lexer.peek() {
                None => break,
                Some((op, _)) => op.clone(),
                // t => panic!("bad token: {:?}", t),
            };
    
            if let Some((l_bp, ())) = Self::postfix_binding_power(&op) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next();
    
                lhs = if op == Token::OpenBracket {
                    let rhs = self.expr_bp(0);
                    assert_eq!(self.lexer.next().unwrap().0, Token::ClosedBracket);
                    S::Cons(op, vec![lhs, rhs])
                } else {
                    S::Cons(op, vec![lhs])
                };
                continue;
            }
    
            if let Some((l_bp, r_bp)) = Self::infix_binding_power(&op) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next();
    
                lhs = if op == Token::Question {
                    let mhs = self.expr_bp(0);
                    assert_eq!(self.lexer.next().unwrap().0, Token::Colon);
                    let rhs = self.expr_bp(r_bp);
                    S::Cons(op, vec![lhs, mhs, rhs])
                } else {
                    let rhs = self.expr_bp(r_bp);
                    S::Cons(op, vec![lhs, rhs])
                };
                continue;
            }
    
            break;
        }
    
        lhs
    }
    
    fn prefix_binding_power(op: &Token) -> ((), u8) {
        match op {
            Token::Add | Token::Sub => ((), 9),
            _ => panic!("bad op: {:?}", op),
        }
    }
    
    fn postfix_binding_power(op: &Token) -> Option<(u8, ())> {
        let res = match op {
            Token::Not => (11, ()),
            Token::OpenBracket => (11, ()),
            _ => return None,
        };
        Some(res)
    }
    
    fn infix_binding_power(op: &Token) -> Option<(u8, u8)> {
        let res = match op {
            Token::Assign => (2, 1),
            Token::Question => (4, 3),
            Token::Add | Token::Sub => (5, 6),
            Token::Mul | Token::Div => (7, 8),
            Token::Dot => (14, 13),
            _ => return None,
        };
        Some(res)
    }
}
