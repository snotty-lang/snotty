#![allow(clippy::fn_address_comparisons)]

use super::utils::{Error, ErrorType, Node, Scope, Token, TokenType, ASSIGNMENT_OPERATORS};

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
            return Some(self.tokens[self.token_index + 1].token_type.clone());
        }
        None
    }

    pub fn parse(tokens: Vec<Token>) -> ParseResult {
        let token = tokens[0].clone();
        let mut global = Scope::new(None);
        let mut obj = Self {
            tokens,
            token_index: 0,
            current_token: token,
        };
        let ast = obj.statements(TokenType::Eof, &mut global)?;
        let ast = match Self::check_undefined(&mut global) {
            Some(err) => return Err(err),
            None => ast,
        };
        match Self::keyword_checks(&ast) {
            Some(err) => Err(err),
            None => Ok(ast),
        }
    }

    fn check_undefined(global: &mut Scope) -> Option<Error> {
        fn check_error(scope: &Scope) -> Option<&Error> {
            if scope.error.is_some() {
                return scope.error.as_ref();
            }
            if scope.func_error.is_some() {
                return scope.func_error.as_ref();
            }
            for child in &scope.scopes {
                if let Some(err) = check_error(child) {
                    return Some(err);
                }
            }
            None
        }
        if let Some(err) = check_error(global) {
            return Some(err.clone());
        }

        fn refresh(scope: &mut Scope, parent: Scope) {
            scope.parent = Some(Box::new(parent));
            scope.refresh();
            let clone = scope.clone();
            for child in scope.scopes.iter_mut() {
                refresh(child, clone.clone());
            }
        }

        let clone = global.clone();
        for child in global.scopes.iter_mut() {
            refresh(child, clone.clone());
        }

        fn check_functions(scope: &mut Scope) -> Option<Node> {
            if !scope.unresolved_functions.is_empty() {
                return Some(scope.unresolved_functions.pop().unwrap());
            }
            for child in &mut scope.scopes {
                if let Some(err) = check_functions(child) {
                    return Some(err);
                }
            }
            None
        }

        if let Some(err) = check_error(global) {
            return Some(err.clone());
        }

        if let Some(node) = check_functions(global) {
            match &node {
                Node::Call(node, _) => match &**node {
                    Node::VarAccess(token) => {
                        return Some(Error::new(
                            ErrorType::Parse,
                            token.position,
                            format!("Function {} is not defined", token),
                        ));
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            }
        }

        None
    }

    fn keyword_checks(_ast: &Node) -> Option<Error> {
        None
    }

    fn statements(&mut self, end_token: TokenType, scope: &mut Scope) -> ParseResult {
        let mut statements = vec![];
        while self.current_token.token_type != end_token {
            match self.current_token.token_type {
                TokenType::Eol => self.advance(),
                _ => statements.push(self.statement(scope)?),
            }
        }
        self.advance();
        Ok(Node::Statements(statements))
    }

    fn statement(&mut self, scope: &mut Scope) -> ParseResult {
        match self.current_token.token_type {
            TokenType::Keyword(ref keyword) => match keyword.as_ref() {
                // break, continue
                "return" => {
                    let token = self.current_token.clone();
                    self.advance();
                    if self.current_token.token_type == TokenType::Eol {
                        Ok(Node::Return(token, None))
                    } else {
                        Ok(Node::Return(token, Some(Box::new(self.expression(scope)?))))
                    }
                }
                _ => self.expression(scope),
            },
            _ => self.expression(scope),
        }
    }

    fn expression(&mut self, scope: &mut Scope) -> ParseResult {
        match self.current_token.token_type {
            TokenType::Keyword(ref keyword) if keyword == "let" => {
                self.advance();
                let node = self.let_statement(Node::VarAssign, scope)?;
                scope.register_variable(node.clone());
                Ok(node)
            }
            TokenType::Identifier(_)
                if self.peek_type().is_some()
                    && ASSIGNMENT_OPERATORS.contains(&self.peek_type().unwrap()) =>
            {
                let node = self.let_statement(Node::VarReassign, scope)?;
                scope.access_variable(node.clone());
                Ok(node)
            }

            TokenType::LCurly => {
                self.advance();
                let mut new_scope = Scope::new(Some(scope));
                let node = self.statements(TokenType::RCurly, &mut new_scope)?;
                scope.scopes.push(new_scope);
                Ok(node)
            }
            _ => self.binary_op(
                Self::comparison,
                vec![TokenType::LAnd, TokenType::LOr],
                Self::comparison,
                scope,
            ),
        }
    }

    fn let_statement(
        &mut self,
        node_type: fn(Token, Box<Node>) -> Node,
        scope: &mut Scope,
    ) -> ParseResult {
        if let TokenType::Identifier(_) = self.current_token.token_type {
            let token = self.current_token.clone();
            self.advance();
            match self.current_token.token_type {
                TokenType::Assign => {
                    self.advance();
                    Ok(node_type(token, Box::new(self.expression(scope)?)))
                }
                ref x if ASSIGNMENT_OPERATORS.contains(x) && node_type == Node::VarReassign => {
                    let op = self.current_token.clone();
                    self.advance();
                    Ok(node_type(
                        token.clone(),
                        Box::new(Node::BinaryOp(
                            op,
                            Box::new(Node::VarAccess(token)),
                            Box::new(self.expression(scope)?),
                        )),
                    ))
                }
                _ => Err(Error::new(
                    ErrorType::Parse,
                    self.current_token.position,
                    format!("Expected '=', found {}", self.current_token),
                )),
            }
        } else {
            Err(Error::new(
                ErrorType::Parse,
                self.current_token.position,
                format!("Expected an identifier, found {}", self.current_token),
            ))
        }
    }

    fn comparison(&mut self, scope: &mut Scope) -> ParseResult {
        if let TokenType::LNot = self.current_token.token_type {
            let token = self.current_token.clone();
            self.advance();
            let node = self.comparison(scope)?;
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
                scope,
            )
        }
    }

    fn bitwise(&mut self, scope: &mut Scope) -> ParseResult {
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
            scope,
        )
    }

    fn arithmetic(&mut self, scope: &mut Scope) -> ParseResult {
        self.binary_op(
            Self::term,
            vec![TokenType::Add, TokenType::Sub],
            Self::term,
            scope,
        )
    }

    fn term(&mut self, scope: &mut Scope) -> ParseResult {
        self.binary_op(
            Self::factor,
            vec![TokenType::Mul, TokenType::Div, TokenType::Mod],
            Self::factor,
            scope,
        )
    }

    fn factor(&mut self, scope: &mut Scope) -> ParseResult {
        let token = self.current_token.clone();
        match token.token_type {
            TokenType::Sub | TokenType::BNot | TokenType::Inc | TokenType::Dec => {
                self.advance();
                let node = self.factor(scope)?;
                Ok(Node::UnaryOp(token, Box::new(node)))
            }
            _ => self.power(scope),
        }
    }

    fn power(&mut self, scope: &mut Scope) -> ParseResult {
        self.binary_op(Self::call, vec![TokenType::Pow], Self::call, scope)
    }

    fn call(&mut self, scope: &mut Scope) -> ParseResult {
        let atom = if let TokenType::Identifier(_) = self.current_token.token_type {
            Some(Node::VarAccess(self.current_token.clone()))
        } else {
            None
        };
        self.advance();
        if let TokenType::LParen = self.current_token.token_type {
            let atom = match atom {
                Some(x) => x,
                None => {
                    return Err(Error::new(
                        ErrorType::Parse,
                        self.current_token.position,
                        format!("Unexpected {}", self.current_token),
                    ))
                }
            };
            self.advance();
            let mut args = vec![];
            while self.current_token.token_type != TokenType::RParen {
                args.push(self.expression(scope)?);
                if self.current_token.token_type != TokenType::Comma {
                    break;
                }
                self.advance();
            }
            if self.current_token.token_type != TokenType::RParen {
                return Err(Error::new(
                    ErrorType::Parse,
                    self.current_token.position,
                    format!("Expected ')', found {}", self.current_token),
                ));
            }
            self.advance();
            let node = Node::Call(Box::new(atom), args);
            scope.access_function(node.clone());
            Ok(node)
        } else {
            self.token_index -= 1;
            self.current_token = self.tokens[self.token_index].clone();
            Ok(self.atom(scope)?)
        }
    }

    fn atom(&mut self, scope: &mut Scope) -> ParseResult {
        let token = self.current_token.clone();
        match token.token_type {
            TokenType::Keyword(ref keyword) => match keyword.as_ref() {
                "ez" => {
                    self.advance();
                    let node = self.function_definition(scope)?;
                    scope.register_function(node.clone());
                    Ok(node)
                }
                _ => Err(Error::new(
                    ErrorType::Parse,
                    self.current_token.position,
                    format!(
                        "Unexpected token: {}. This might be because of unterminated brackets",
                        self.current_token
                    ),
                )),
            },
            TokenType::Identifier(_) => {
                self.advance();
                let node = Node::VarAccess(token);
                scope.access_variable(node.clone());
                Ok(node)
            }
            TokenType::LParen => {
                self.advance();
                let node = self.expression(scope)?;
                if self.current_token.token_type != TokenType::RParen {
                    return Err(Error::new(
                        ErrorType::Parse,
                        self.current_token.position,
                        format!("Expected ')', found {}", self.current_token),
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
                    "Unexpected token: {}. This might be because of unterminated brackets",
                    self.current_token
                ),
            )),
        }
    }

    fn function_definition(&mut self, scope: &mut Scope) -> ParseResult {
        let name = if let TokenType::Identifier(_) = self.current_token.token_type {
            self.current_token.clone()
        } else {
            return Err(Error::new(
                ErrorType::Parse,
                self.current_token.position,
                format!("Expected an identifier, found {}", self.current_token),
            ));
        };
        self.advance();
        if self.current_token.token_type != TokenType::LParen {
            return Err(Error::new(
                ErrorType::Parse,
                self.current_token.position,
                format!("Expected '(', found {}", self.current_token),
            ));
        }
        self.advance();
        let mut params = vec![];
        if let TokenType::Identifier(_) = self.current_token.token_type {
            params.push(self.current_token.clone());
            self.advance();
            while self.current_token.token_type == TokenType::Comma {
                self.advance();
                if let TokenType::Identifier(_) = self.current_token.token_type {
                    params.push(self.current_token.clone());
                    self.advance();
                } else {
                    return Err(Error::new(
                        ErrorType::Parse,
                        self.current_token.position,
                        format!("Expected identifier, found {}", self.current_token),
                    ));
                }
            }
            if self.current_token.token_type != TokenType::RParen {
                return Err(Error::new(
                    ErrorType::Parse,
                    self.current_token.position,
                    format!("Expected ')' or ',', found {}", self.current_token),
                ));
            }
        } else if self.current_token.token_type != TokenType::RParen {
            return Err(Error::new(
                ErrorType::Parse,
                self.current_token.position,
                format!("Expected identifier or ')', found {}", self.current_token),
            ));
        }
        self.advance();

        let mut new_scope = Scope::new(Some(scope));
        new_scope.args = Some(params.clone());
        let expr = self.expression(&mut new_scope)?;
        if !new_scope.scopes.is_empty() {
            assert!(new_scope.scopes.len() == 1);
            new_scope = new_scope.scopes.pop().unwrap();
            new_scope.parent = None;
        }
        scope.scopes.push(new_scope);
        Ok(Node::FuncDef(name, params, Box::new(expr)))
    }

    fn binary_op(
        &mut self,
        func1: fn(&mut Self, &mut Scope) -> ParseResult,
        ops: Vec<TokenType>,
        func2: fn(&mut Self, &mut Scope) -> ParseResult,
        scope: &mut Scope,
    ) -> ParseResult {
        let mut left = func1(self, scope)?;
        let mut token_type = self.current_token.token_type.clone();
        while ops.contains(&token_type) {
            let op = self.current_token.clone();
            self.advance();
            let right = func2(self, scope)?;
            left = Node::BinaryOp(op, Box::new(left), Box::new(right));
            token_type = self.current_token.token_type.clone();
        }
        Ok(left)
    }
}
