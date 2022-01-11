use super::utils::{Error, ErrorType, Position, Token, TokenType, KEYWORDS};

/// A Result type for Lexing
type LexResult = Result<Vec<Token>, Error>;

/// Lexes the given input string into a vector of tokens
/// # Arguments
/// * `input` - The input string to be lexed
/// # Returns
/// * `LexResult` - A Result containing a vector of tokens or an error, if any
/// # Errors
/// Returns an error if a number is very big or if an invalid token was found
/// # Examples
/// ```
/// use ezlang;
///
/// let tokens = ezlang::lexer::lex("ezout 5 + 7");
/// assert!(tokens.is_ok());
///
/// let tokens = ezlang::lexer::lex("$? ez");
/// assert!(tokens.is_err());
/// ```
pub fn lex(input: &str, filename: &'static str) -> LexResult {
    let mut parentheses = Vec::new();
    let mut tokens = Vec::new();
    let mut chars = input.chars().enumerate().peekable();
    let mut line = 1;
    let mut last_line = 0;
    let mut last = 0;

    while let Some((j, c)) = chars.next() {
        let i = j - last_line + 1;
        last = i + 1;
        match c {
            ' ' | '\t' | '\n' | '\r' => {
                if c == '\n' {
                    line += 1;
                    last_line = j + 1;
                }
            }
            '+' => {
                if let Some((_, '=')) = chars.peek() {
                    tokens.push(Token::new(TokenType::AddAssign, line, i, i + 2, filename));
                    chars.next();
                } else if let Some((_, '+')) = chars.peek() {
                    tokens.push(Token::new(TokenType::Inc, line, i, i + 2, filename));
                    chars.next();
                } else {
                    tokens.push(Token::new(TokenType::Add, line, i, i + 1, filename));
                }
            }
            '-' => {
                if let Some((_, '=')) = chars.peek() {
                    tokens.push(Token::new(TokenType::SubAssign, line, i, i + 2, filename));
                    chars.next();
                } else if let Some((_, '-')) = chars.peek() {
                    tokens.push(Token::new(TokenType::Dec, line, i, i + 2, filename));
                    chars.next();
                } else if let Some((_, '>')) = chars.peek() {
                    tokens.push(Token::new(TokenType::Arrow, line, i, i + 2, filename));
                    chars.next();
                } else {
                    tokens.push(Token::new(TokenType::Sub, line, i, i + 1, filename));
                }
            }
            '*' => {
                if let Some((_, '=')) = chars.peek() {
                    tokens.push(Token::new(TokenType::MulAssign, line, i, i + 2, filename));
                    chars.next();
                } else if let Some((_, '*')) = chars.peek() {
                    chars.next();
                    if let Some((_, '=')) = chars.peek() {
                        tokens.push(Token::new(TokenType::PowAssign, line, i, i + 3, filename));
                        chars.next();
                    } else {
                        tokens.push(Token::new(TokenType::Pow, line, i, i + 2, filename));
                    }
                } else {
                    tokens.push(Token::new(TokenType::Mul, line, i, i + 1, filename));
                }
            }
            '/' => match chars.peek() {
                Some((_, '=')) => {
                    tokens.push(Token::new(TokenType::DivAssign, line, i, i + 2, filename));
                    chars.next();
                }
                Some((_, '/')) => {
                    for (i, c) in chars.by_ref() {
                        if c == '\n' {
                            line += 1;
                            last_line = i + 2;
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
                            last_line = i + 2;
                        }
                    }
                    if !end {
                        return Err(Error::new(
                            ErrorType::SyntaxError,
                            Position::new(line, i, i + 1, filename),
                            "Unterminated comment".to_string(),
                        ));
                    }
                }
                _ => {
                    tokens.push(Token::new(TokenType::Div, line, i, i + 2, filename));
                }
            },
            ':' => {
                tokens.push(Token::new(TokenType::Colon, line, i, i + 1, filename));
            }
            '%' => {
                if let Some((_, '=')) = chars.peek() {
                    tokens.push(Token::new(TokenType::ModAssign, line, i, i + 2, filename));
                    chars.next();
                } else {
                    tokens.push(Token::new(TokenType::Mod, line, i, i + 1, filename));
                }
            }
            '(' => {
                tokens.push(Token::new(TokenType::LParen, line, i, i + 1, filename));
                parentheses.push((Position::new(line, i, i + 1, filename), 0));
            }
            ')' => {
                tokens.push(Token::new(TokenType::RParen, line, i, i + 1, filename));
                let paren = parentheses.pop();
                if paren.is_none() {
                    return Err(Error::new(
                        ErrorType::SyntaxError,
                        Position::new(line, i, i + 1, filename),
                        "Missing opening '(' pair".to_string(),
                    ));
                } else if paren.unwrap().1 != 0 {
                    return Err(Error::new(
                        ErrorType::SyntaxError,
                        Position::new(line, i, i + 1, filename),
                        "Mismatched brackets: '{'".to_string(),
                    ));
                }
            }
            '{' => {
                tokens.push(Token::new(TokenType::LCurly, line, i, i + 1, filename));
                parentheses.push((Position::new(line, i, i + 1, filename), 1));
            }
            '}' => {
                tokens.push(Token::new(TokenType::RCurly, line, i, i + 1, filename));
                let paren = parentheses.pop();
                if paren.is_none() {
                    return Err(Error::new(
                        ErrorType::SyntaxError,
                        Position::new(line, i, i + 1, filename),
                        "Missing opening '{' pair".to_string(),
                    ));
                } else if paren.unwrap().1 != 1 {
                    return Err(Error::new(
                        ErrorType::SyntaxError,
                        Position::new(line, i, i + 1, filename),
                        "Mismatched brackets: '('".to_string(),
                    ));
                }
            }
            ',' => {
                tokens.push(Token::new(TokenType::Comma, line, i, i + 1, filename));
            }
            ';' => {
                tokens.push(Token::new(TokenType::Eol, line, i, i + 1, filename));
            }
            '>' => {
                if let Some((_, '=')) = chars.peek() {
                    chars.next();
                    tokens.push(Token::new(TokenType::Ge, line, i, i + 2, filename));
                } else if let Some((_, '>')) = chars.peek() {
                    chars.next();
                    if let Some((_, '=')) = chars.peek() {
                        chars.next();
                        tokens.push(Token::new(TokenType::ShrAssign, line, i, i + 3, filename));
                    } else {
                        tokens.push(Token::new(TokenType::Shr, line, i, i + 2, filename));
                    }
                } else {
                    tokens.push(Token::new(TokenType::Gt, line, i, i + 1, filename));
                }
            }
            '<' => {
                if let Some((_, '=')) = chars.peek() {
                    chars.next();
                    tokens.push(Token::new(TokenType::Le, line, i, i + 2, filename));
                } else if let Some((_, '<')) = chars.peek() {
                    chars.next();
                    if let Some((_, '=')) = chars.peek() {
                        chars.next();
                        tokens.push(Token::new(TokenType::ShlAssign, line, i, i + 3, filename));
                    } else {
                        tokens.push(Token::new(TokenType::Shl, line, i, i + 2, filename));
                    }
                } else {
                    tokens.push(Token::new(TokenType::Lt, line, i, i + 1, filename));
                }
            }
            '!' => match chars.peek() {
                Some((_, '=')) => {
                    chars.next();
                    tokens.push(Token::new(TokenType::Neq, line, i, i + 2, filename));
                }
                Some((_, '&')) => {
                    let mut new = chars.clone();
                    new.next();
                    if let Some((_, '|')) = new.peek() {
                        chars.next();
                        chars.next();
                        if let Some((_, '=')) = chars.peek() {
                            chars.next();
                            tokens.push(Token::new(
                                TokenType::LXorAssign,
                                line,
                                i,
                                i + 4,
                                filename,
                            ));
                        } else {
                            tokens.push(Token::new(TokenType::LXor, line, i, i + 3, filename));
                        }
                    } else {
                        tokens.push(Token::new(TokenType::LNot, line, i, i + 1, filename));
                    }
                }
                _ => {
                    tokens.push(Token::new(TokenType::LNot, line, i, i + 1, filename));
                }
            },
            '=' => {
                if let Some((_, '=')) = chars.peek() {
                    chars.next();
                    tokens.push(Token::new(TokenType::Eq, line, i, i + 2, filename));
                } else {
                    tokens.push(Token::new(TokenType::Assign, line, i, i + 1, filename));
                }
            }
            '&' => {
                if let Some((_, '&')) = chars.peek() {
                    chars.next();
                    if let Some((_, '=')) = chars.peek() {
                        chars.next();
                        tokens.push(Token::new(TokenType::LAndAssign, line, i, i + 3, filename));
                    } else {
                        tokens.push(Token::new(TokenType::LAnd, line, i, i + 2, filename));
                    }
                } else if let Some((_, '=')) = chars.peek() {
                    chars.next();
                    tokens.push(Token::new(TokenType::BAndAssign, line, i, i + 2, filename));
                } else {
                    tokens.push(Token::new(TokenType::BAnd, line, i, i + 1, filename));
                }
            }
            '~' => {
                tokens.push(Token::new(TokenType::BNot, line, i, i + 1, filename));
            }
            '|' => {
                if let Some((_, '|')) = chars.peek() {
                    chars.next();
                    if let Some((_, '=')) = chars.peek() {
                        chars.next();
                        tokens.push(Token::new(TokenType::LOrAssign, line, i, i + 3, filename));
                    } else {
                        tokens.push(Token::new(TokenType::LOr, line, i, i + 2, filename));
                    }
                } else if let Some((_, '=')) = chars.peek() {
                    chars.next();
                    tokens.push(Token::new(TokenType::BOrAssign, line, i, i + 2, filename));
                } else {
                    tokens.push(Token::new(TokenType::BOr, line, i, i + 1, filename));
                }
            }
            '^' => {
                if let Some((_, '=')) = chars.peek() {
                    chars.next();
                    tokens.push(Token::new(TokenType::BXorAssign, line, i, i + 2, filename));
                } else {
                    tokens.push(Token::new(TokenType::BXor, line, i, i + 1, filename));
                }
            }
            '?' => {
                tokens.push(Token::new(TokenType::TernaryIf, line, i, i + 1, filename));
            }
            _ if c.is_digit(10) => {
                let mut num = c.to_string();
                let start = i;
                let mut end = j + 2;
                while let Some((i, c)) = chars.peek() {
                    if !c.is_digit(10) {
                        break;
                    }
                    end = *i + 2;
                    num.push(*c);
                    chars.next();
                }
                tokens.push(Token::new(
                    TokenType::Number(match num.parse() {
                        Ok(num) => num,
                        Err(err) => {
                            return Err(Error::new(
                                ErrorType::NumberTooLarge,
                                Position::new(line, start, end, filename),
                                err.to_string(),
                            ));
                        }
                    }),
                    line,
                    start,
                    end,
                    filename,
                ));
            }
            _ if c.is_alphabetic() => {
                let mut word = c.to_string();
                let start = i;
                let mut end = j + 2;
                while let Some((i, c)) = chars.peek() {
                    if !c.is_alphanumeric() {
                        break;
                    }
                    end = *i + 2;
                    word.push(*c);
                    chars.next();
                }
                end -= last_line;
                if KEYWORDS.contains(&word.as_ref()) {
                    tokens.push(Token::new(
                        TokenType::Keyword(word),
                        line,
                        start,
                        end,
                        filename,
                    ));
                } else {
                    tokens.push(Token::new(
                        TokenType::Identifier(word),
                        line,
                        start,
                        end,
                        filename,
                    ));
                }
            }
            _ => {
                return Err(Error::new(
                    ErrorType::InvalidLiteral,
                    Position::new(line, i, i + 1, filename),
                    format!("Invalid token: {}", c),
                ));
            }
        }
    }

    if !parentheses.is_empty() {
        return Err(Error::new(
            ErrorType::SyntaxError,
            parentheses.pop().unwrap().0,
            "Unclosed Parentheses".to_string(),
        ));
    }

    tokens.push(Token::new(TokenType::Eof, line, last, last, filename));
    Ok(tokens)
}
