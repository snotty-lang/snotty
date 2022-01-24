use std::{fs, rc::Rc};

use crate::{
    lexer,
    utils::{Error, ErrorType, Token, TokenType},
};

pub fn preprocess(mut tokens: Vec<Token>) -> Result<Vec<Token>, Error> {
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
                        TokenType::Identifier(file) | TokenType::Keyword(file) => {
                            match fs::read_to_string(&file) {
                                Ok(contents) => {
                                    let new_tokens = lexer::lex(&contents, Rc::new(file))?;
                                    tokens.splice(i..=i + 1, new_tokens);
                                }
                                Err(e) => {
                                    return Err(Error::new(
                                        ErrorType::FileNotFound,
                                        t.position.clone(),
                                        format!("Could not find file `{}` ({})", file, e),
                                    ))
                                }
                            }
                        }
                        _ => {
                            return Err(Error::new(
                                ErrorType::SyntaxError,
                                t.position.clone(),
                                "Expected a filename after `use`".to_owned(),
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
