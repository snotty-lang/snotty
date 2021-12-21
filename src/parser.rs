#![allow(clippy::fn_address_comparisons)]

use super::utils::{Error, ErrorType, Node, Token, TokenType, ASSIGNMENT_OPERATORS};

type ParseResult = Result<Node, Error>;

pub struct Parser {
    tokens: Vec<Token>,
    token_index: usize,
    current_token: Token,
    accessed_variables: Vec<Node>,
    accessed_functions: Vec<Node>,
    defined_functions: Vec<Node>,
    defined_variables: Vec<Node>,
}

impl Parser {
    fn advance(&mut self) {
        self.token_index += 1;
        if self.token_index < self.tokens.len() {
            self.current_token = self.tokens[self.token_index].clone();
        }
    }

    fn peek_type(&self) -> Option<TokenType> {
        if self.token_index + 1 < self.tokens.len() {
            return Some(self.tokens[self.token_index + 1].token_type.clone());
        }
        None
    }

    pub fn parse(tokens: Vec<Token>) -> ParseResult {
        let token = tokens[0].clone();
        let mut obj = Self {
            tokens,
            token_index: 0,
            current_token: token,
            accessed_variables: vec![],
            accessed_functions: vec![],
            defined_functions: vec![],
            defined_variables: vec![],
        };
        let ast = obj.statements(TokenType::Eof)?;
        // obj.analyze()?;
        Ok(ast)
    }

    // fn analyze(&mut self) -> Option<Error> {
    //     for variable in &self.accessed_variables {
    //         match variable {
    //             Node::VarAccess(token) | Node::VarReassign(token, _) => {
    //                 if !self.defined_variables.contains(&variable) {
    //                     return Err(Error::new(
    //                         ErrorType::Parse,
    //                         token.position.clone(),
    //                         format!("Variable {:?} is not defined", token),
    //                     ));
    //                 }
    //             }
    //             _ => unreachable!(),
    //         }
    //     }
    //     for function in self.accessed_functions {
    //         match function {
    //             Node::Call(node, args) => {
    //                 for function in &self.defined_functions {
    //                     if let Node::FuncDef(name, _, _, _) = function {
    //                         if name == *node {
    //                             return Ok(Node::Call(Box::new(function.clone()), args.clone()));
    //                         }
    //                     }
    //                 }
    //             }
    //             _ => unreachable!(),
    //         }
    //     }
    // }

    fn statements(&mut self, end_token: TokenType) -> ParseResult {
        let mut statements = vec![];
        while self.current_token.token_type != end_token {
            match self.current_token.token_type {
                TokenType::Eol => self.advance(),
                _ => statements.push(self.statement()?),
            }
        }
        self.advance();
        Ok(Node::Statements(statements))
    }

    fn statement(&mut self) -> ParseResult {
        match self.current_token.token_type {
            TokenType::Keyword(ref keyword) => match keyword {
                // break, continue, return
                _ => self.expression(),
            },
            _ => self.expression(),
        }
    }

    fn expression(&mut self) -> ParseResult {
        match self.current_token.token_type {
            TokenType::Keyword(ref keyword) if keyword == "let" => {
                self.advance();
                let node = self.let_statement(Node::VarAssign)?;
                self.defined_variables.push(node.clone());
                Ok(node)
            }
            TokenType::Identifier(_)
                if self.peek_type().is_some()
                    && ASSIGNMENT_OPERATORS.contains(&self.peek_type().unwrap()) =>
            {
                let node = self.let_statement(Node::VarReassign)?;
                self.accessed_variables.push(node.clone());
                Ok(node)
            }

            TokenType::LCurly => {
                self.advance();
                self.statements(TokenType::RCurly)
            }
            _ => self.binary_op(
                Self::comparison,
                vec![TokenType::LAnd, TokenType::LOr],
                Self::comparison,
            ),
        }
    }

    fn let_statement(&mut self, node_type: fn(Token, Box<Node>) -> Node) -> ParseResult {
        if let TokenType::Identifier(_) = self.current_token.token_type {
            let token = self.current_token.clone();
            self.advance();
            match self.current_token.token_type {
                TokenType::Assign => {
                    self.advance();
                    Ok(node_type(token, Box::new(self.expression()?)))
                }
                ref x if ASSIGNMENT_OPERATORS.contains(x) && node_type == Node::VarReassign => {
                    let op = self.current_token.clone();
                    self.advance();
                    Ok(node_type(
                        token.clone(),
                        Box::new(Node::BinaryOp(
                            op,
                            Box::new(Node::VarAccess(token)),
                            Box::new(self.expression()?),
                        )),
                    ))
                }
                _ => Err(Error::new(
                    ErrorType::Parse,
                    self.current_token.position,
                    format!("Expected '=', found {:?}", self.current_token),
                )),
            }
        } else {
            Err(Error::new(
                ErrorType::Parse,
                self.current_token.position,
                format!("Expected an identifier, found {:?}", self.current_token),
            ))
        }
    }

    fn comparison(&mut self) -> ParseResult {
        if let TokenType::LNot = self.current_token.token_type {
            let token = self.current_token.clone();
            self.advance();
            let node = self.comparison()?;
            Ok(Node::UnaryOp(token, Box::new(node)))
        } else {
            self.binary_op(
                Self::bitwise,
                vec![
                    TokenType::Eq,
                    TokenType::Neq,
                    TokenType::Lt,
                    TokenType::Gt,
                    TokenType::Le,
                    TokenType::Ge,
                ],
                Self::bitwise,
            )
        }
    }

    fn bitwise(&mut self) -> ParseResult {
        self.binary_op(
            Self::arithmetic,
            vec![
                TokenType::BAnd,
                TokenType::BOr,
                TokenType::BXor,
                TokenType::Shl,
                TokenType::Shr,
            ],
            Self::arithmetic,
        )
    }

    fn arithmetic(&mut self) -> ParseResult {
        self.binary_op(Self::term, vec![TokenType::Add, TokenType::Sub], Self::term)
    }

    fn term(&mut self) -> ParseResult {
        self.binary_op(
            Self::factor,
            vec![TokenType::Mul, TokenType::Div, TokenType::Mod],
            Self::factor,
        )
    }

    fn factor(&mut self) -> ParseResult {
        let token = self.current_token.clone();
        match token.token_type {
            TokenType::Sub | TokenType::BNot | TokenType::Inc | TokenType::Dec => {
                self.advance();
                let node = self.factor()?;
                Ok(Node::UnaryOp(token, Box::new(node)))
            }
            _ => self.power(),
        }
    }

    fn power(&mut self) -> ParseResult {
        self.binary_op(Self::call, vec![TokenType::Pow], Self::call)
    }

    fn call(&mut self) -> ParseResult {
        let atom = self.atom()?;
        if let TokenType::LParen = self.current_token.token_type {
            self.advance();
            let mut args = vec![];
            while self.current_token.token_type != TokenType::RParen {
                args.push(self.expression()?);
                if self.current_token.token_type != TokenType::Comma {
                    break;
                }
                self.advance();
            }
            if self.current_token.token_type != TokenType::RParen {
                return Err(Error::new(
                    ErrorType::Parse,
                    self.current_token.position,
                    format!("Expected ')', found {:?}", self.current_token),
                ));
            }
            self.advance();
            let node = Node::Call(Box::new(atom), args);
            self.accessed_functions.push(node.clone());
            Ok(node)
        } else {
            Ok(atom)
        }
    }

    fn atom(&mut self) -> ParseResult {
        let token = self.current_token.clone();
        match token.token_type {
            TokenType::Keyword(ref keyword) => match keyword.as_ref() {
                "ez" => {
                    self.advance();
                    let node = self.function_definition()?;
                    self.defined_functions.push(node.clone());
                    Ok(node)
                }
                _ => Err(Error::new(
                    ErrorType::Parse,
                    self.current_token.position,
                    format!(
                        "Unexpected token: {:?}. This might be because of unterminated brackets",
                        self.current_token
                    ),
                )),
            },
            TokenType::Identifier(_) => {
                self.advance();
                let node = Node::VarAccess(token);
                self.accessed_variables.push(node.clone());
                Ok(node)
            }
            TokenType::LParen => {
                self.advance();
                let node = self.expression()?;
                if self.current_token.token_type != TokenType::RParen {
                    return Err(Error::new(
                        ErrorType::Parse,
                        self.current_token.position,
                        format!("Expected ')', found {:?}", self.current_token),
                    ));
                }
                self.advance();
                Ok(node)
            }
            TokenType::Number(_) => {
                self.advance();
                Ok(Node::Number(token))
            }
            _ => Err(Error::new(
                ErrorType::Parse,
                self.current_token.position,
                format!(
                    "Unexpected token: {:?}. This might be because of unterminated brackets",
                    self.current_token
                ),
            )),
        }
    }

    fn function_definition(&mut self) -> ParseResult {
        let name = if let TokenType::Identifier(_) = self.current_token.token_type {
            self.current_token.clone()
        } else {
            return Err(Error::new(
                ErrorType::Parse,
                self.current_token.position,
                format!("Expected an identifier, found {:?}", self.current_token),
            ));
        };
        self.advance();
        if self.current_token.token_type != TokenType::LParen {
            return Err(Error::new(
                ErrorType::Parse,
                self.current_token.position,
                format!("Expected '(', found {:?}", self.current_token),
            ));
        }
        self.advance();
        let mut params = vec![];
        if let TokenType::Identifier(_) = self.current_token.token_type {
            params.push(self.current_token.clone());
            self.advance();
            if self.current_token.token_type == TokenType::Colon {
                self.advance();
                if let TokenType::Keyword(ref keyword) = self.current_token.token_type {
                    match keyword.as_ref() {
                        "u32" => (),
                        _ => {
                            return Err(Error::new(
                                ErrorType::Parse,
                                self.current_token.position,
                                format!("Expected type, found {:?}", self.current_token),
                            ))
                        }
                    }
                    params.push(self.current_token.clone());
                    self.advance();
                } else {
                    return Err(Error::new(
                        ErrorType::Parse,
                        self.current_token.position,
                        format!("Expected type, found {:?}", self.current_token),
                    ));
                }
            } else {
                return Err(Error::new(
                    ErrorType::Parse,
                    self.current_token.position,
                    format!("Expected ':', found {:?}", self.current_token),
                ));
            }
            while self.current_token.token_type == TokenType::Comma {
                self.advance();
                if let TokenType::Identifier(_) = self.current_token.token_type {
                    params.push(self.current_token.clone());
                    self.advance();
                } else {
                    return Err(Error::new(
                        ErrorType::Parse,
                        self.current_token.position,
                        format!("Expected identifier, found {:?}", self.current_token),
                    ));
                }
                if self.current_token.token_type == TokenType::Colon {
                    self.advance();
                    if let TokenType::Keyword(ref keyword) = self.current_token.token_type {
                        match keyword.as_ref() {
                            "u32" => (),
                            _ => {
                                return Err(Error::new(
                                    ErrorType::Parse,
                                    self.current_token.position,
                                    format!("Expected type, found {:?}", self.current_token),
                                ))
                            }
                        }
                        params.push(self.current_token.clone());
                        self.advance();
                    } else {
                        return Err(Error::new(
                            ErrorType::Parse,
                            self.current_token.position,
                            format!("Expected type, found {:?}", self.current_token),
                        ));
                    }
                } else {
                    return Err(Error::new(
                        ErrorType::Parse,
                        self.current_token.position,
                        format!("Expected ':', found {:?}", self.current_token),
                    ));
                }
            }
            if self.current_token.token_type != TokenType::RParen {
                return Err(Error::new(
                    ErrorType::Parse,
                    self.current_token.position,
                    format!("Expected ')' or ',', found {:?}", self.current_token),
                ));
            }
        } else if self.current_token.token_type != TokenType::RParen {
            return Err(Error::new(
                ErrorType::Parse,
                self.current_token.position,
                format!("Expected identifier or ')', found {:?}", self.current_token),
            ));
        }
        self.advance();
        if self.current_token.token_type == TokenType::Arrow {
            self.advance();
            let return_type = match self.current_token.token_type {
                TokenType::Keyword(ref keyword) => match keyword.as_ref() {
                    "u32" => self.current_token.clone(),
                    _ => {
                        return Err(Error::new(
                            ErrorType::Parse,
                            self.current_token.position,
                            format!("Expected return type, found {:?}", self.current_token),
                        ))
                    }
                },
                _ => {
                    return Err(Error::new(
                        ErrorType::Parse,
                        self.current_token.position,
                        format!("Expected return type, found {:?}", self.current_token),
                    ))
                }
            };
            self.advance();
            return Ok(Node::FuncDef(
                name,
                params,
                Some(return_type),
                Box::new(self.expression()?),
            ));
        }
        Ok(Node::FuncDef(
            name,
            params,
            None,
            Box::new(self.expression()?),
        ))
    }

    fn binary_op(
        &mut self,
        func1: fn(&mut Self) -> ParseResult,
        ops: Vec<TokenType>,
        func2: fn(&mut Self) -> ParseResult,
    ) -> ParseResult {
        let mut left = func1(self)?;
        let mut token_type = self.current_token.token_type.clone();
        while ops.contains(&token_type) {
            let op = self.current_token.clone();
            self.advance();
            let right = func2(self)?;
            left = Node::BinaryOp(op, Box::new(left), Box::new(right));
            token_type = self.current_token.token_type.clone();
        }
        Ok(left)
    }
}
