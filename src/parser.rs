use super::utils::{Error, ErrorType, Node, Token, TokenType};

type ParseResult = Result<Node, Error>;

pub struct Parser {
    tokens: Vec<Token>,
    token_index: usize,
    current_token: Token,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Option<Self> {
        if tokens.is_empty() {
            return None;
        }
        let token = tokens[0].clone();
        Some(Self {
            tokens,
            token_index: 0,
            current_token: token,
        })
    }

    fn advance(&mut self) {
        self.token_index += 1;
        if self.token_index < self.tokens.len() {
            self.current_token = self.tokens[self.token_index].clone();
        }
    }

    pub fn parse(&mut self) -> ParseResult {
        self.statements(TokenType::Eof)
    }

    fn statements(&mut self, end_token: TokenType) -> ParseResult {
        let mut statements = vec![];
        while self.current_token.token_type != end_token {
            println!("{:?} != {:?}", self.current_token.token_type, end_token);
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
                self.let_statement()
            }
            TokenType::LCurly => {
                self.advance();
                self.statements(TokenType::RCurly)
            }
            _ => self.bin_op(
                Self::comparison,
                vec![TokenType::LAnd, TokenType::LOr],
                Self::comparison,
            ),
        }
    }

    fn let_statement(&mut self) -> ParseResult {
        if let TokenType::Identifier(_) = self.current_token.token_type {
            let token = self.current_token.clone();
            self.advance();
            if self.current_token.token_type != TokenType::Assign {
                return Err(Error::new(
                    ErrorType::Parse,
                    self.current_token.position,
                    format!("let Unexpected token: {:?}", self.current_token),
                ));
            }
            self.advance();
            let value = self.expression()?;
            Ok(Node::VarAssign(token, Box::new(value)))
        } else {
            Err(Error::new(
                ErrorType::Parse,
                self.current_token.position,
                format!("let2 Unexpected token: {:?}", self.current_token),
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
            self.bin_op(
                Self::biwise,
                vec![
                    TokenType::Eq,
                    TokenType::Neq,
                    TokenType::Lt,
                    TokenType::Gt,
                    TokenType::Le,
                    TokenType::Ge,
                ],
                Self::biwise,
            )
        }
    }

    fn biwise(&mut self) -> ParseResult {
        self.bin_op(
            Self::arithmetic,
            vec![
                TokenType::Eq,
                TokenType::Neq,
                TokenType::Lt,
                TokenType::Gt,
                TokenType::Le,
                TokenType::Ge,
            ],
            Self::arithmetic,
        )
    }

    fn arithmetic(&mut self) -> ParseResult {
        self.bin_op(Self::term, vec![TokenType::Add, TokenType::Sub], Self::term)
    }

    fn term(&mut self) -> ParseResult {
        self.bin_op(
            Self::factor,
            vec![TokenType::Mul, TokenType::Div, TokenType::Mod],
            Self::factor,
        )
    }

    fn factor(&mut self) -> ParseResult {
        let token = self.current_token.clone();
        match token.token_type {
            TokenType::Add | TokenType::Sub => {
                self.advance();
                let node = self.factor()?;
                Ok(Node::UnaryOp(token, Box::new(node)))
            }
            _ => self.power(),
        }
    }

    fn power(&mut self) -> ParseResult {
        self.bin_op(Self::call, vec![TokenType::Pow], Self::call)
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
                    format!("call Unexpected token: {:?}", self.current_token),
                ));
            }
            self.advance();
            Ok(Node::Call(Box::new(atom), args))
        } else {
            Ok(atom)
        }
    }

    fn atom(&mut self) -> ParseResult {
        let token = self.current_token.clone();
        match token.token_type {
            TokenType::Identifier(_) => {
                self.advance();
                Ok(Node::VarAccess(token))
            }
            TokenType::LParen => {
                self.advance();
                let node = self.expression()?;
                if self.current_token.token_type != TokenType::RParen {
                    return Err(Error::new(
                        ErrorType::Parse,
                        self.current_token.position,
                        format!("atom Unexpected token: {:?}", self.current_token),
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
                format!("atom2 Unexpected token: {:?}", self.current_token),
            )),
        }
    }

    fn bin_op(
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
            left = Node::BinOp(op, Box::new(left), Box::new(right));
            token_type = self.current_token.token_type.clone();
        }
        Ok(left)
    }
}
