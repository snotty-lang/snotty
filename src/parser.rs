use super::utils::{Error, ErrorType, Node, Token, TokenType};

type ParseResult = Result<Node, Error>;

pub struct Parser {
    tokens: Vec<Token>,
    token_index: usize,
    current_token: Token,
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
            return Some(self.tokens[self.token_index + 1].clone().token_type);
        }
        None
    }

    /// Returns `None` if there were no tokens to parse
    ///
    /// Returns `Some(Err(Error))` if there was an error
    ///
    /// Returns `Some(Ok(Node))` if there was no error
    pub fn parse(tokens: Vec<Token>) -> Option<ParseResult> {
        if tokens.is_empty() {
            return None;
        }
        let token = tokens[0].clone();
        Some(
            Self {
                tokens,
                token_index: 0,
                current_token: token,
            }
            .statements(TokenType::Eof),
        )
    }

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
                self.let_statement(Node::VarAssign)
            }
            TokenType::Identifier(_) if self.peek_type() == Some(TokenType::Assign) => {
                self.let_statement(Node::VarReassign)
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
                TokenType::AddAssign
                | TokenType::SubAssign
                | TokenType::BAndAssign
                | TokenType::BOrAssign
                | TokenType::BXorAssign
                | TokenType::DivAssign
                | TokenType::ShlAssign
                | TokenType::ShrAssign
                | TokenType::MulAssign
                | TokenType::ModAssign
                | TokenType::PowAssign
                    if node_type == Node::VarReassign =>
                {
                    let op = self.current_token.clone();
                    self.advance();
                    Ok(node_type(
                        token.clone(),
                        Box::new(Node::BinaryOp(
                            op,
                            Box::new(Node::VarAccess(token.clone())),
                            Box::new(self.expression()?),
                        )),
                    ))
                }
                _ => Err(Error::new(
                    ErrorType::Parse,
                    self.current_token.position,
                    format!("let Unexpected token: {:?}", self.current_token),
                )),
            }
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
            TokenType::Add | TokenType::Sub | TokenType::BNot | TokenType::Inc | TokenType::Dec => {
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
