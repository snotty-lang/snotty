use std::{fs, rc::Rc, collections::HashSet};

use crate::{
    lexer,
    utils::{Error, ErrorType, Token, TokenType},
};

pub fn preprocess(mut tokens: Vec<Token>) -> Result<Vec<Token>, Error> {
    let mut declared = HashSet::new();
    let mut i = 0;
    while i < tokens.len() {
        if let TokenType::PreprocessorStatement(ref stmt) = tokens[i].token_type {
            match stmt.as_ref() {
                "use" => match tokens.get(i + 1).cloned() {
                    None => {
                        return Err(Error::new(
                            ErrorType::SyntaxError,
                            tokens[i].position.clone(),
                            "Expected a filename after `use`".to_owned(),
                        ))
                    }
                    Some(t) => match t.token_type {
                        TokenType::Identifier(file) => match fs::read_to_string(&file) {
                            Ok(contents) => {
                                let mut new_tokens = lexer::lex(&contents, Rc::new(file))?;
                                new_tokens.pop().unwrap();
                                tokens.splice(i..=i + 1, new_tokens);
                            }
                            Err(e) => {
                                return Err(Error::new(
                                    ErrorType::FileNotFound,
                                    t.position.clone(),
                                    format!("Could not find file `{}` ({})", file, e),
                                ))
                            }
                        },
                        _ => {
                            return Err(Error::new(
                                ErrorType::SyntaxError,
                                t.position.clone(),
                                "Expected a filename after `use`".to_owned(),
                            ))
                        }
                    },
                },
                "replace" => {
                    let find = match tokens.get(i + 1).cloned() {
                        None => {
                            return Err(Error::new(
                                ErrorType::SyntaxError,
                                tokens[i].position.clone(),
                                "Expected find element `replace`".to_owned(),
                            ))
                        }
                        Some(t) => match t.token_type {
                            TokenType::Identifier(find) => find,
                            _ => {
                                return Err(Error::new(
                                    ErrorType::SyntaxError,
                                    t.position,
                                    "Expected find element `replace`".to_owned(),
                                ))
                            }
                        },
                    };
                    let replace = match tokens.get(i + 2).cloned() {
                        None => {
                            return Err(Error::new(
                                ErrorType::SyntaxError,
                                tokens[i].position.clone(),
                                "Expected replace element `replace`".to_owned(),
                            ))
                        }
                        Some(t) => match t.token_type {
                            TokenType::Identifier(replace) => replace,
                            _ => {
                                return Err(Error::new(
                                    ErrorType::SyntaxError,
                                    t.position,
                                    "Expected replace element `replace`".to_owned(),
                                ))
                            }
                        },
                    };
                    tokens.drain(i..=i + 2);
                    for token in tokens.iter_mut() {
                        if let TokenType::Identifier(ref mut id) = token.token_type {
                            if *id == find {
                                *id = replace.to_owned();
                            }
                        }
                    }
                }
                "declare" => match tokens.get(i + 1).cloned() {
                    None => {
                        return Err(Error::new(
                            ErrorType::SyntaxError,
                            tokens[i].position.clone(),
                            "Expected an identifier after `declare`".to_owned(),
                        ))
                    }
                    Some(t) => match t.token_type {
                        TokenType::Identifier(ident) => {
                            declared.insert(ident);
                            tokens.drain(i..=i + 1);
                        },
                        _ => {
                            return Err(Error::new(
                                ErrorType::SyntaxError,
                                t.position.clone(),
                                "Expected an identifier after `declare`".to_owned(),
                            ))
                        }
                    },
                },
                _ => todo!(),
            }
        }
        i += 1;
    }
    Ok(tokens)
}