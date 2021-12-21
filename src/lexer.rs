#![allow(clippy::while_let_on_iterator)]

use super::utils::{Error, ErrorType, Position, Token, TokenType, KEYWORDS};

type LexResult = Result<Vec<Token>, Error>;

pub struct Lexer;

impl Lexer {
    pub fn lex(input: &str) -> LexResult {
        let mut tokens = Vec::new();
        let mut chars = input.chars().enumerate().peekable();
        let mut line = 1;
        let mut last_line = 0;

        while let Some((j, c)) = chars.next() {
            let i = j - last_line;
            match c {
                ' ' | '\t' | '\n' | '\r' => {
                    if c == '\n' {
                        line += 1;
                        last_line = j + 1;
                    }
                }
                '+' => {
                    if let Some((_, '=')) = chars.peek() {
                        tokens.push(Token::new(TokenType::AddAssign, line, i, i + 1));
                        chars.next();
                    } else if let Some((_, '+')) = chars.peek() {
                        tokens.push(Token::new(TokenType::Inc, line, i, i + 1));
                        chars.next();
                    } else {
                        tokens.push(Token::new(TokenType::Add, line, i, i));
                    }
                }
                '-' => {
                    if let Some((_, '=')) = chars.peek() {
                        tokens.push(Token::new(TokenType::SubAssign, line, i, i + 1));
                        chars.next();
                    } else if let Some((_, '-')) = chars.peek() {
                        tokens.push(Token::new(TokenType::Dec, line, i, i + 1));
                        chars.next();
                    } else if let Some((_, '>')) = chars.peek() {
                        tokens.push(Token::new(TokenType::Arrow, line, i, i + 1));
                        chars.next();
                    } else {
                        tokens.push(Token::new(TokenType::Sub, line, i, i));
                    }
                }
                '*' => {
                    if let Some((_, '=')) = chars.peek() {
                        tokens.push(Token::new(TokenType::MulAssign, line, i, i + 1));
                        chars.next();
                    } else if let Some((_, '*')) = chars.peek() {
                        chars.next();
                        if let Some((_, '=')) = chars.peek() {
                            tokens.push(Token::new(TokenType::PowAssign, line, i, i + 2));
                            chars.next();
                        } else {
                            tokens.push(Token::new(TokenType::Pow, line, i, i + 1));
                        }
                    } else {
                        tokens.push(Token::new(TokenType::Mul, line, i, i));
                    }
                }
                '/' => match chars.peek() {
                    Some((_, '=')) => {
                        tokens.push(Token::new(TokenType::DivAssign, line, i, i + 1));
                        chars.next();
                    }
                    Some((_, '/')) => {
                        while let Some((i, c)) = chars.next() {
                            if c == '\n' {
                                line += 1;
                                last_line = i + 1;
                                break;
                            }
                        }
                    }
                    Some((_, '*')) => {
                        chars.next();
                        let mut end = false;
                        while let Some((i, c)) = chars.next() {
                            if c == '*' {
                                if let Some((_, '/')) = chars.next() {
                                    end = true;
                                    break;
                                }
                            } else if c == '\n' {
                                line += 1;
                                last_line = i + 1;
                            }
                        }
                        if !end {
                            return Err(Error::new(
                                ErrorType::Lex,
                                Position::new(line, i, i),
                                "Unterminated comment".to_string(),
                            ));
                        }
                    }
                    _ => {
                        tokens.push(Token::new(TokenType::Div, line, i, i + 1));
                    }
                },
                ':' => {
                    tokens.push(Token::new(TokenType::Colon, line, i, i));
                }
                '%' => {
                    if let Some((_, '=')) = chars.peek() {
                        tokens.push(Token::new(TokenType::ModAssign, line, i, i + 1));
                        chars.next();
                    } else {
                        tokens.push(Token::new(TokenType::Mod, line, i, i));
                    }
                }
                '(' => {
                    tokens.push(Token::new(TokenType::LParen, line, i, i));
                }
                ')' => {
                    tokens.push(Token::new(TokenType::RParen, line, i, i));
                }
                '{' => {
                    tokens.push(Token::new(TokenType::LCurly, line, i, i));
                }
                '}' => {
                    tokens.push(Token::new(TokenType::RCurly, line, i, i));
                }
                ',' => {
                    tokens.push(Token::new(TokenType::Comma, line, i, i));
                }
                ';' => {
                    tokens.push(Token::new(TokenType::Eol, line, i, i));
                }
                '>' => {
                    if let Some((_, '=')) = chars.peek() {
                        chars.next();
                        tokens.push(Token::new(TokenType::Ge, line, i, i + 1));
                    } else if let Some((_, '>')) = chars.peek() {
                        chars.next();
                        if let Some((_, '=')) = chars.peek() {
                            chars.next();
                            tokens.push(Token::new(TokenType::ShrAssign, line, i, i + 2));
                        } else {
                            tokens.push(Token::new(TokenType::Shr, line, i, i + 1));
                        }
                    } else {
                        tokens.push(Token::new(TokenType::Gt, line, i, i));
                    }
                }
                '<' => {
                    if let Some((_, '=')) = chars.peek() {
                        chars.next();
                        tokens.push(Token::new(TokenType::Le, line, i, i + 1));
                    } else if let Some((_, '<')) = chars.peek() {
                        chars.next();
                        if let Some((_, '=')) = chars.peek() {
                            chars.next();
                            tokens.push(Token::new(TokenType::ShlAssign, line, i, i + 2));
                        } else {
                            tokens.push(Token::new(TokenType::Shl, line, i, i + 1));
                        }
                    } else {
                        tokens.push(Token::new(TokenType::Lt, line, i, i));
                    }
                }
                '!' => {
                    if let Some((_, '=')) = chars.peek() {
                        chars.next();
                        tokens.push(Token::new(TokenType::Neq, line, i, i + 1));
                    } else {
                        tokens.push(Token::new(TokenType::LNot, line, i, i));
                    }
                }
                '=' => {
                    if let Some((_, '=')) = chars.peek() {
                        chars.next();
                        tokens.push(Token::new(TokenType::Eq, line, i, i + 1));
                    } else {
                        tokens.push(Token::new(TokenType::Assign, line, i, i));
                    }
                }
                '&' => {
                    if let Some((_, '&')) = chars.peek() {
                        chars.next();
                        tokens.push(Token::new(TokenType::LAnd, line, i, i + 1));
                    } else if let Some((_, '=')) = chars.peek() {
                        chars.next();
                        tokens.push(Token::new(TokenType::BAndAssign, line, i, i + 1));
                    } else {
                        tokens.push(Token::new(TokenType::BAnd, line, i, i));
                    }
                }
                '~' => {
                    tokens.push(Token::new(TokenType::BNot, line, i, i));
                }
                '|' => {
                    if let Some((_, '|')) = chars.peek() {
                        chars.next();
                        tokens.push(Token::new(TokenType::LOr, line, i, i + 1));
                    } else if let Some((_, '=')) = chars.peek() {
                        chars.next();
                        tokens.push(Token::new(TokenType::BOrAssign, line, i, i + 1));
                    } else {
                        tokens.push(Token::new(TokenType::BOr, line, i, i));
                    }
                }
                '^' => {
                    if let Some((_, '=')) = chars.peek() {
                        chars.next();
                        tokens.push(Token::new(TokenType::BXorAssign, line, i, i + 1));
                    } else {
                        tokens.push(Token::new(TokenType::BXor, line, i, i));
                    }
                }
                _ if c.is_digit(10) => {
                    let mut num = c.to_string();
                    let start = i;
                    let mut end = i;
                    while let Some((i, c)) = chars.peek() {
                        if !c.is_digit(10) {
                            break;
                        }
                        end = *i;
                        num.push(*c);
                        chars.next();
                    }
                    tokens.push(Token::new(
                        TokenType::Number(match num.parse() {
                            Ok(num) => num,
                            Err(err) => {
                                return Err(Error::new(
                                    ErrorType::Lex,
                                    Position::new(line, start, end),
                                    err.to_string(),
                                ));
                            }
                        }),
                        line,
                        start,
                        end,
                    ));
                }
                _ if c.is_alphabetic() => {
                    let mut word = c.to_string();
                    let start = i;
                    let mut end = j;
                    while let Some((i, c)) = chars.peek() {
                        if !c.is_alphanumeric() {
                            break;
                        }
                        end = *i;
                        word.push(*c);
                        chars.next();
                    }
                    end -= last_line;
                    if KEYWORDS.contains(&word.as_ref()) {
                        tokens.push(Token::new(TokenType::Keyword(word), line, start, end));
                    } else {
                        tokens.push(Token::new(TokenType::Identifier(word), line, start, end));
                    }
                }
                _ => {
                    return Err(Error::new(
                        ErrorType::Lex,
                        Position::new(line, i, i + 1),
                        format!("Invalid token: {}", c),
                    ));
                }
            }
        }
        tokens.push(Token::new(TokenType::Eof, line, input.len(), input.len()));
        Ok(tokens)
    }
}
