use std::rc::Rc;

use crate::utils::{
    Error, ErrorType, Position, Token, TokenType, KEYWORDS, PREPROCESSOR_STATEMENTS,
};

/// A Result type for Lexing
type LexResult = Result<Vec<Token>, Error>;

const LITERALS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";

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
pub fn lex(input: &str, filename: Rc<String>) -> LexResult {
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
            '\'' => {
                let c = match chars.next() {
                    Some((_, c)) => match c {
                        '\'' => {
                            return Err(Error::new(
                                ErrorType::SyntaxError,
                                Position::new(line, i, i + 2, Rc::clone(&filename)),
                                "Expected char literal, found \'".to_string(),
                            ))
                        }
                        '\\' => {
                            (match chars.next() {
                                Some((_, c)) => match c {
                                    'n' => '\n',
                                    'r' => '\r',
                                    't' => '\t',
                                    '\\' => '\\',
                                    '\'' => '\'',
                                    '0' => '\0',
                                    _ => {
                                        return Err(Error::new(
                                            ErrorType::SyntaxError,
                                            Position::new(line, i, i + 3, Rc::clone(&filename)),
                                            "Invalid escape sequence".to_string(),
                                        ))
                                    }
                                },
                                None => {
                                    return Err(Error::new(
                                        ErrorType::SyntaxError,
                                        Position::new(line, i, i + 3, Rc::clone(&filename)),
                                        "Expected char literal, found \\".to_string(),
                                    ))
                                }
                            } as u8)
                        }
                        _ => c as u8,
                    },
                    None => {
                        return Err(Error::new(
                            ErrorType::SyntaxError,
                            Position::new(line, i, i + 2, Rc::clone(&filename)),
                            "Unclosed char literal".to_owned(),
                        ));
                    }
                };
                match chars.next() {
                    Some((_, '\'')) => {
                        tokens.push(Token::new(
                            TokenType::Char(c),
                            line,
                            i,
                            i + 3,
                            Rc::clone(&filename),
                        ));
                    }
                    Some((_, c)) => {
                        return Err(Error::new(
                            ErrorType::SyntaxError,
                            Position::new(line, i, i + 2, Rc::clone(&filename)),
                            format!("Expected \', found {:?}", c),
                        ));
                    }
                    None => {
                        return Err(Error::new(
                            ErrorType::SyntaxError,
                            Position::new(line, i, i + 3, Rc::clone(&filename)),
                            "Unclosed char literal".to_owned(),
                        ));
                    }
                }
            }
            '+' => {
                if let Some((_, '=')) = chars.peek() {
                    tokens.push(Token::new(
                        TokenType::AddAssign,
                        line,
                        i,
                        i + 2,
                        Rc::clone(&filename),
                    ));
                    chars.next();
                } else if let Some((_, '+')) = chars.peek() {
                    tokens.push(Token::new(
                        TokenType::Inc,
                        line,
                        i,
                        i + 2,
                        Rc::clone(&filename),
                    ));
                    chars.next();
                } else {
                    tokens.push(Token::new(
                        TokenType::Add,
                        line,
                        i,
                        i + 1,
                        Rc::clone(&filename),
                    ));
                }
            }
            '-' => {
                if let Some((_, '=')) = chars.peek() {
                    tokens.push(Token::new(
                        TokenType::SubAssign,
                        line,
                        i,
                        i + 2,
                        Rc::clone(&filename),
                    ));
                    chars.next();
                } else if let Some((_, '-')) = chars.peek() {
                    tokens.push(Token::new(
                        TokenType::Dec,
                        line,
                        i,
                        i + 2,
                        Rc::clone(&filename),
                    ));
                    chars.next();
                } else if let Some((_, '>')) = chars.peek() {
                    tokens.push(Token::new(
                        TokenType::Arrow,
                        line,
                        i,
                        i + 2,
                        Rc::clone(&filename),
                    ));
                    chars.next();
                } else {
                    tokens.push(Token::new(
                        TokenType::Sub,
                        line,
                        i,
                        i + 1,
                        Rc::clone(&filename),
                    ));
                }
            }
            '*' => {
                if let Some((_, '=')) = chars.peek() {
                    tokens.push(Token::new(
                        TokenType::MulAssign,
                        line,
                        i,
                        i + 2,
                        Rc::clone(&filename),
                    ));
                    chars.next();
                } else if let Some((_, '*')) = chars.peek() {
                    chars.next();
                    if let Some((_, '=')) = chars.peek() {
                        tokens.push(Token::new(
                            TokenType::PowAssign,
                            line,
                            i,
                            i + 3,
                            Rc::clone(&filename),
                        ));
                        chars.next();
                    } else {
                        tokens.push(Token::new(
                            TokenType::Pow,
                            line,
                            i,
                            i + 2,
                            Rc::clone(&filename),
                        ));
                    }
                } else {
                    tokens.push(Token::new(
                        TokenType::Mul,
                        line,
                        i,
                        i + 1,
                        Rc::clone(&filename),
                    ));
                }
            }
            '/' => match chars.peek() {
                Some((_, '=')) => {
                    tokens.push(Token::new(
                        TokenType::DivAssign,
                        line,
                        i,
                        i + 2,
                        Rc::clone(&filename),
                    ));
                    chars.next();
                }
                Some((_, '/')) => {
                    for (i, c) in chars.by_ref() {
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
                            ErrorType::SyntaxError,
                            Position::new(line, i, i + 1, Rc::clone(&filename)),
                            "Unterminated comment".to_string(),
                        ));
                    }
                }
                _ => {
                    tokens.push(Token::new(
                        TokenType::Div,
                        line,
                        i,
                        i + 2,
                        Rc::clone(&filename),
                    ));
                }
            },
            ':' => {
                tokens.push(Token::new(
                    TokenType::Colon,
                    line,
                    i,
                    i + 1,
                    Rc::clone(&filename),
                ));
            }
            '%' => {
                if let Some((_, '=')) = chars.peek() {
                    tokens.push(Token::new(
                        TokenType::ModAssign,
                        line,
                        i,
                        i + 2,
                        Rc::clone(&filename),
                    ));
                    chars.next();
                } else {
                    tokens.push(Token::new(
                        TokenType::Mod,
                        line,
                        i,
                        i + 1,
                        Rc::clone(&filename),
                    ));
                }
            }
            '(' => {
                tokens.push(Token::new(
                    TokenType::LParen,
                    line,
                    i,
                    i + 1,
                    Rc::clone(&filename),
                ));
                parentheses.push((Position::new(line, i, i + 1, Rc::clone(&filename)), 0));
            }
            ')' => {
                tokens.push(Token::new(
                    TokenType::RParen,
                    line,
                    i,
                    i + 1,
                    Rc::clone(&filename),
                ));
                let paren = parentheses.pop();
                if paren.is_none() {
                    return Err(Error::new(
                        ErrorType::SyntaxError,
                        Position::new(line, i, i + 1, Rc::clone(&filename)),
                        "Missing opening '(' pair".to_string(),
                    ));
                } else if paren.unwrap().1 != 0 {
                    return Err(Error::new(
                        ErrorType::SyntaxError,
                        Position::new(line, i, i + 1, Rc::clone(&filename)),
                        "Mismatched brackets".to_string(),
                    ));
                }
            }
            '[' => {
                tokens.push(Token::new(
                    TokenType::LSquare,
                    line,
                    i,
                    i + 1,
                    Rc::clone(&filename),
                ));
                parentheses.push((Position::new(line, i, i + 1, Rc::clone(&filename)), 2));
            }
            ']' => {
                tokens.push(Token::new(
                    TokenType::RSquare,
                    line,
                    i,
                    i + 1,
                    Rc::clone(&filename),
                ));
                let paren = parentheses.pop();
                if paren.is_none() {
                    return Err(Error::new(
                        ErrorType::SyntaxError,
                        Position::new(line, i, i + 1, Rc::clone(&filename)),
                        "Missing opening '[' pair".to_string(),
                    ));
                } else if paren.unwrap().1 != 2 {
                    return Err(Error::new(
                        ErrorType::SyntaxError,
                        Position::new(line, i, i + 1, Rc::clone(&filename)),
                        "Mismatched brackets".to_string(),
                    ));
                }
            }
            '{' => {
                tokens.push(Token::new(
                    TokenType::LCurly,
                    line,
                    i,
                    i + 1,
                    Rc::clone(&filename),
                ));
                parentheses.push((Position::new(line, i, i + 1, Rc::clone(&filename)), 1));
            }
            '}' => {
                tokens.push(Token::new(
                    TokenType::RCurly,
                    line,
                    i,
                    i + 1,
                    Rc::clone(&filename),
                ));
                let paren = parentheses.pop();
                if paren.is_none() {
                    return Err(Error::new(
                        ErrorType::SyntaxError,
                        Position::new(line, i, i + 1, Rc::clone(&filename)),
                        "Missing opening '{' pair".to_string(),
                    ));
                } else if paren.unwrap().1 != 1 {
                    return Err(Error::new(
                        ErrorType::SyntaxError,
                        Position::new(line, i, i + 1, Rc::clone(&filename)),
                        "Mismatched brackets".to_string(),
                    ));
                }
            }
            ',' => {
                tokens.push(Token::new(
                    TokenType::Comma,
                    line,
                    i,
                    i + 1,
                    Rc::clone(&filename),
                ));
            }
            ';' => {
                tokens.push(Token::new(
                    TokenType::Eol,
                    line,
                    i,
                    i + 1,
                    Rc::clone(&filename),
                ));
            }
            '>' => {
                if let Some((_, '=')) = chars.peek() {
                    chars.next();
                    tokens.push(Token::new(
                        TokenType::Ge,
                        line,
                        i,
                        i + 2,
                        Rc::clone(&filename),
                    ));
                } else if let Some((_, '>')) = chars.peek() {
                    chars.next();
                    if let Some((_, '=')) = chars.peek() {
                        chars.next();
                        tokens.push(Token::new(
                            TokenType::ShrAssign,
                            line,
                            i,
                            i + 3,
                            Rc::clone(&filename),
                        ));
                    } else {
                        tokens.push(Token::new(
                            TokenType::Shr,
                            line,
                            i,
                            i + 2,
                            Rc::clone(&filename),
                        ));
                    }
                } else {
                    tokens.push(Token::new(
                        TokenType::Gt,
                        line,
                        i,
                        i + 1,
                        Rc::clone(&filename),
                    ));
                }
            }
            '<' => {
                if let Some((_, '=')) = chars.peek() {
                    chars.next();
                    tokens.push(Token::new(
                        TokenType::Le,
                        line,
                        i,
                        i + 2,
                        Rc::clone(&filename),
                    ));
                } else if let Some((_, '<')) = chars.peek() {
                    chars.next();
                    if let Some((_, '=')) = chars.peek() {
                        chars.next();
                        tokens.push(Token::new(
                            TokenType::ShlAssign,
                            line,
                            i,
                            i + 3,
                            Rc::clone(&filename),
                        ));
                    } else {
                        tokens.push(Token::new(
                            TokenType::Shl,
                            line,
                            i,
                            i + 2,
                            Rc::clone(&filename),
                        ));
                    }
                } else {
                    tokens.push(Token::new(
                        TokenType::Lt,
                        line,
                        i,
                        i + 1,
                        Rc::clone(&filename),
                    ));
                }
            }
            '!' => match chars.peek() {
                Some((_, '=')) => {
                    chars.next();
                    tokens.push(Token::new(
                        TokenType::Neq,
                        line,
                        i,
                        i + 2,
                        Rc::clone(&filename),
                    ));
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
                                Rc::clone(&filename),
                            ));
                        } else {
                            tokens.push(Token::new(
                                TokenType::LXor,
                                line,
                                i,
                                i + 3,
                                Rc::clone(&filename),
                            ));
                        }
                    } else {
                        tokens.push(Token::new(
                            TokenType::LNot,
                            line,
                            i,
                            i + 1,
                            Rc::clone(&filename),
                        ));
                    }
                }
                Some((_, c)) => {
                    if LITERALS.contains(*c) {
                        let mut word = String::new();
                        let start = i;
                        let mut end = j + 2;
                        while let Some((i, c)) = chars.peek() {
                            if !LITERALS.contains(*c) && !c.is_numeric() {
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
                                Rc::clone(&filename),
                            ));
                        } else if PREPROCESSOR_STATEMENTS.contains(&word.as_ref()) {
                            tokens.push(Token::new(
                                TokenType::PreprocessorStatement(word),
                                line,
                                start,
                                end,
                                Rc::clone(&filename),
                            ));
                        } else {
                            tokens.push(Token::new(
                                TokenType::Identifier(word),
                                line,
                                start,
                                end,
                                Rc::clone(&filename),
                            ));
                        }
                    } else {
                        tokens.push(Token::new(
                            TokenType::LNot,
                            line,
                            i,
                            i + 1,
                            Rc::clone(&filename),
                        ));
                    }
                }
                None => {
                    tokens.push(Token::new(
                        TokenType::LNot,
                        line,
                        i,
                        i + 1,
                        Rc::clone(&filename),
                    ));
                }
            },
            '"' => {
                let mut word = String::new();
                let start = i;
                let mut end = j + 2;
                let mut escape = false;
                for (i, c) in chars.by_ref() {
                    if escape {
                        match c {
                            'n' => word.push('\n'),
                            't' => word.push('\t'),
                            'r' => word.push('\r'),
                            '\\' => word.push('\\'),
                            '"' => word.push('"'),
                            _ => {
                                return Err(Error::new(
                                    ErrorType::SyntaxError,
                                    Position::new(line, i, i + 3, Rc::clone(&filename)),
                                    "Invalid escape sequence".to_string(),
                                ))
                            }
                        }
                        escape = false;
                    } else if c == '"' {
                        end = i + 2;
                        break;
                    } else if c == '\\' {
                        escape = true;
                    } else {
                        word.push(c);
                    }
                }
                end -= last_line;
                tokens.push(Token::new(
                    TokenType::String(word),
                    line,
                    start,
                    end,
                    Rc::clone(&filename),
                ));
            }
            '=' => {
                if let Some((_, '=')) = chars.peek() {
                    chars.next();
                    tokens.push(Token::new(
                        TokenType::Eq,
                        line,
                        i,
                        i + 2,
                        Rc::clone(&filename),
                    ));
                } else {
                    tokens.push(Token::new(
                        TokenType::Assign,
                        line,
                        i,
                        i + 1,
                        Rc::clone(&filename),
                    ));
                }
            }
            '&' => {
                if let Some((_, '&')) = chars.peek() {
                    chars.next();
                    if let Some((_, '=')) = chars.peek() {
                        chars.next();
                        tokens.push(Token::new(
                            TokenType::LAndAssign,
                            line,
                            i,
                            i + 3,
                            Rc::clone(&filename),
                        ));
                    } else {
                        tokens.push(Token::new(
                            TokenType::LAnd,
                            line,
                            i,
                            i + 2,
                            Rc::clone(&filename),
                        ));
                    }
                } else if let Some((_, '=')) = chars.peek() {
                    chars.next();
                    tokens.push(Token::new(
                        TokenType::BAndAssign,
                        line,
                        i,
                        i + 2,
                        Rc::clone(&filename),
                    ));
                } else {
                    tokens.push(Token::new(
                        TokenType::BAnd,
                        line,
                        i,
                        i + 1,
                        Rc::clone(&filename),
                    ));
                }
            }
            '~' => {
                tokens.push(Token::new(
                    TokenType::BNot,
                    line,
                    i,
                    i + 1,
                    Rc::clone(&filename),
                ));
            }
            '|' => {
                if let Some((_, '|')) = chars.peek() {
                    chars.next();
                    if let Some((_, '=')) = chars.peek() {
                        chars.next();
                        tokens.push(Token::new(
                            TokenType::LOrAssign,
                            line,
                            i,
                            i + 3,
                            Rc::clone(&filename),
                        ));
                    } else {
                        tokens.push(Token::new(
                            TokenType::LOr,
                            line,
                            i,
                            i + 2,
                            Rc::clone(&filename),
                        ));
                    }
                } else if let Some((_, '=')) = chars.peek() {
                    chars.next();
                    tokens.push(Token::new(
                        TokenType::BOrAssign,
                        line,
                        i,
                        i + 2,
                        Rc::clone(&filename),
                    ));
                } else {
                    tokens.push(Token::new(
                        TokenType::BOr,
                        line,
                        i,
                        i + 1,
                        Rc::clone(&filename),
                    ));
                }
            }
            '^' => {
                if let Some((_, '=')) = chars.peek() {
                    chars.next();
                    tokens.push(Token::new(
                        TokenType::BXorAssign,
                        line,
                        i,
                        i + 2,
                        Rc::clone(&filename),
                    ));
                } else {
                    tokens.push(Token::new(
                        TokenType::BXor,
                        line,
                        i,
                        i + 1,
                        Rc::clone(&filename),
                    ));
                }
            }
            '?' => {
                tokens.push(Token::new(
                    TokenType::TernaryIf,
                    line,
                    i,
                    i + 1,
                    Rc::clone(&filename),
                ));
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
                                Position::new(line, start, end, Rc::clone(&filename)),
                                err.to_string(),
                            ));
                        }
                    }),
                    line,
                    start,
                    end,
                    Rc::clone(&filename),
                ));
            }
            _ if LITERALS.contains(c) => {
                let mut word = c.to_string();
                let start = i;
                let mut end = j + 2;
                while let Some((i, c)) = chars.peek() {
                    if !LITERALS.contains(*c) && !c.is_numeric() {
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
                        Rc::clone(&filename),
                    ));
                } else {
                    tokens.push(Token::new(
                        TokenType::Identifier(word),
                        line,
                        start,
                        end,
                        Rc::clone(&filename),
                    ));
                }
            }
            _ => {
                return Err(Error::new(
                    ErrorType::InvalidLiteral,
                    Position::new(line, i, i + 1, Rc::clone(&filename)),
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

    if let Some(s) = last.checked_sub(last_line) {
        last = s;
    }

    tokens.push(Token::new(
        TokenType::Eof,
        line,
        last,
        last,
        Rc::clone(&filename),
    ));
    Ok(tokens)
}
