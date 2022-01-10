use super::utils::{Error, ErrorType, Node, Scope, Token, TokenType, ASSIGNMENT_OPERATORS};

/// A result type for parsing
type ParseResult = Result<Node, Error>;

/// Parses the List of Tokens into an AST
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
                "let" => {
                    self.advance();
                    let node = self.let_statement(true, scope)?;
                    scope.register_variable(node.clone());
                    Ok(node)
                }
                "if" => {
                    self.advance();
                    let condition = self.expression(scope)?;
                    let then_branch = self.statement(scope)?;
                    let else_ = if self.current_token.token_type
                        == TokenType::Keyword("else".to_string())
                    {
                        self.advance();
                        Some(Box::new(self.statement(scope)?))
                    } else {
                        None
                    };
                    Ok(Node::If(Box::new(condition), Box::new(then_branch), else_))
                }
                "ezascii" => {
                    self.advance();
                    Ok(Node::Ascii(Box::new(self.expression(scope)?)))
                }
                "ezout" => {
                    self.advance();
                    Ok(Node::Print(Box::new(self.expression(scope)?)))
                }
                _ => self.expression(scope),
            },
            TokenType::Identifier(_)
                if self.peek_type().is_some()
                    && ASSIGNMENT_OPERATORS.contains(&self.peek_type().unwrap()) =>
            {
                let node = self.let_statement(false, scope)?;
                scope.access_variable(node.clone());
                Ok(node)
            }
            _ => self.expression(scope),
        }
    }

    fn expression(&mut self, scope: &mut Scope) -> ParseResult {
        self.binary_op(
            Self::comparison,
            vec![TokenType::LAnd, TokenType::LOr, TokenType::LXor],
            Self::comparison,
            scope,
        )
    }

    fn let_statement(&mut self, init: bool, scope: &mut Scope) -> ParseResult {
        if let TokenType::Identifier(_) = self.current_token.token_type {
            let token = self.current_token.clone();
            self.advance();
            match self.current_token.token_type {
                TokenType::Assign if init => {
                    self.advance();
                    Ok(Node::VarAssign(token, Box::new(self.expression(scope)?)))
                }
                TokenType::Assign => {
                    self.advance();
                    Ok(Node::VarReassign(token, Box::new(self.expression(scope)?)))
                }
                ref x if ASSIGNMENT_OPERATORS.contains(x) && !init => {
                    let op = self.current_token.clone();
                    self.advance();
                    let right = self.expression(scope)?;

                    if [TokenType::DivAssign, TokenType::ModAssign].contains(&op.token_type) {
                        if let Node::Number(Token {
                            token_type: TokenType::Number(0),
                            ..
                        }) = right
                        {
                            return Err(Error::new(
                                ErrorType::DivisionByZero,
                                self.current_token.position,
                                "Division by zero".to_string(),
                            ));
                        }
                    }

                    Ok(Node::VarReassign(
                        token.clone(),
                        Box::new(Node::BinaryOp(
                            op.un_augmented(),
                            Box::new(Node::VarAccess(token)),
                            Box::new(right),
                        )),
                    ))
                }
                _ => Err(Error::new(
                    ErrorType::SyntaxError,
                    self.current_token.position,
                    format!("Expected '=', found {}", self.current_token),
                )),
            }
        } else {
            Err(Error::new(
                ErrorType::SyntaxError,
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
        if let TokenType::Identifier(_) = self.current_token.token_type {
            let atom = Node::VarAccess(self.current_token.clone());
            self.advance();
            if let TokenType::LParen = self.current_token.token_type {
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
                        ErrorType::SyntaxError,
                        self.current_token.position,
                        format!("Expected ')', found {}", self.current_token),
                    ));
                }
                self.advance();
                let node = Node::Call(Box::new(atom), args);
                scope.access_function(node.clone());
                return Ok(node);
            } else {
                self.token_index -= 1;
            }
        }
        self.current_token = self.tokens[self.token_index].clone();
        self.atom(scope)
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
                "ezin" => {
                    self.advance();
                    Ok(Node::Input(token))
                }
                "true" => {
                    self.advance();
                    Ok(Node::Number(token))
                }
                "false" => {
                    self.advance();
                    Ok(Node::Number(token))
                }
                "ezblank" => {
                    self.advance();
                    Ok(Node::None(token))
                }
                _ => Err(Error::new(
                    ErrorType::SyntaxError,
                    self.current_token.position,
                    format!("Unexpected keyword: {}", self.current_token),
                )),
            },
            TokenType::TernaryIf => {
                self.advance();
                let condition = self.expression(scope)?;
                let then_branch = self.expression(scope)?;
                if self.current_token.token_type != TokenType::Colon {
                    return Err(Error::new(
                        ErrorType::SyntaxError,
                        self.current_token.position,
                        format!("Expected ':', found {}", self.current_token),
                    ));
                }
                self.advance();
                let else_branch = self.expression(scope)?;
                Ok(Node::Ternary(
                    Box::new(condition),
                    Box::new(then_branch),
                    Box::new(else_branch),
                ))
            }
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
                        ErrorType::SyntaxError,
                        self.current_token.position,
                        format!("Expected ')', found {}", self.current_token),
                    ));
                }
                self.advance();
                Ok(node)
            }
            TokenType::LCurly => {
                self.advance();
                let mut new_scope = Scope::new(Some(scope));
                let node = self.statements(TokenType::RCurly, &mut new_scope)?;
                scope.scopes.push(new_scope);
                Ok(node)
            }
            TokenType::Number(_) => {
                self.advance();
                Ok(Node::Number(token))
            }
            _ => Err(Error::new(
                ErrorType::SyntaxError,
                self.current_token.position,
                format!("Unexpected token: {}", self.current_token),
            )),
        }
    }

    fn function_definition(&mut self, scope: &mut Scope) -> ParseResult {
        let name = if let TokenType::Identifier(_) = self.current_token.token_type {
            self.current_token.clone()
        } else {
            return Err(Error::new(
                ErrorType::SyntaxError,
                self.current_token.position,
                format!("Expected an identifier, found {}", self.current_token),
            ));
        };
        self.advance();
        if self.current_token.token_type != TokenType::LParen {
            return Err(Error::new(
                ErrorType::SyntaxError,
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
                        ErrorType::SyntaxError,
                        self.current_token.position,
                        format!("Expected identifier, found {}", self.current_token),
                    ));
                }
            }
            if self.current_token.token_type != TokenType::RParen {
                return Err(Error::new(
                    ErrorType::SyntaxError,
                    self.current_token.position,
                    format!("Expected ')' or ',', found {}", self.current_token),
                ));
            }
        } else if self.current_token.token_type != TokenType::RParen {
            return Err(Error::new(
                ErrorType::SyntaxError,
                self.current_token.position,
                format!("Expected identifier or ')', found {}", self.current_token),
            ));
        }
        self.advance();

        let mut new_scope = Scope::new(Some(scope));
        new_scope.args = Some(params.clone());
        let expr = self.expression(&mut new_scope)?;
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
            if [TokenType::Div, TokenType::Mod].contains(&op.token_type) {
                if let Node::Number(Token {
                    token_type: TokenType::Number(0),
                    ..
                }) = right
                {
                    return Err(Error::new(
                        ErrorType::DivisionByZero,
                        self.current_token.position,
                        "Division by zero".to_string(),
                    ));
                }
            }
            left = Node::BinaryOp(op, Box::new(left), Box::new(right));
            token_type = self.current_token.token_type.clone();
        }
        Ok(left)
    }
}

/// Parses the given vector of tokens into an AST.
/// Returns the root node of the AST.
/// # Errors
/// If the tokens cannot be parsed into an AST, an error is returned.
pub fn parse(tokens: Vec<Token>) -> ParseResult {
    let token = tokens[0].clone();
    let mut global = Scope::new(None);
    let mut obj = Parser {
        tokens,
        token_index: 0,
        current_token: token,
    };
    let ast = obj.statements(TokenType::Eof, &mut global)?;
    let ast = match check_undefined(&mut global) {
        Some(err) => return Err(err),
        None => ast,
    };
    match keyword_checks(&ast) {
        Some(err) => Err(err),
        None => Ok(ast),
    }
}

/// Checks for undefined functions and variables.
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
                        ErrorType::UndefinedFunction,
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

/// Checks for invalid placement and use of keywords
fn keyword_checks(ast: &Node) -> Option<Error> {
    fn check_return(node: &Node) -> Option<Token> {
        match node {
            Node::None(_) => None,
            Node::Number(_) => None,
            Node::BinaryOp(_, n1, n2) => {
                let n1 = check_return(n1);
                if n1.is_some() {
                    return n1;
                }
                let n2 = check_return(n2);
                if n2.is_some() {
                    return n2;
                }
                None
            }
            Node::UnaryOp(_, n1) => check_return(n1),
            Node::VarAssign(_, n1) => check_return(n1),
            Node::VarAccess(_) => None,
            Node::VarReassign(_, n1) => check_return(n1),
            Node::Statements(_) => None,
            Node::Ternary(n1, n2, n3) => {
                let n1 = check_return(n1);
                if n1.is_some() {
                    return n1;
                }
                let n2 = check_return(n2);
                if n2.is_some() {
                    return n2;
                }
                let n3 = check_return(n3);
                if n3.is_some() {
                    return n3;
                }
                None
            }
            Node::If(n1, n2, n3) => {
                let n1 = check_return(n1);
                if n1.is_some() {
                    return n1;
                }
                let n2 = check_return(n2);
                if n2.is_some() {
                    return n2;
                }
                if let Some(n3) = n3 {
                    let n3 = check_return(n3);
                    if n3.is_some() {
                        return n3;
                    }
                }
                None
            }
            Node::Call(n1, n2) => {
                let n1 = check_return(n1);
                if n1.is_some() {
                    return n1;
                }
                for i in n2.iter().map(check_return) {
                    if i.is_some() {
                        return i;
                    }
                }
                None
            }
            Node::FuncDef(_, _, _) => None,
            Node::Return(t, _) => Some(t.clone()),
            Node::Print(n1) => check_return(n1),
            Node::Ascii(n1) => check_return(n1),
            Node::Input(_) => None,
        }
    }
    match ast {
        Node::Statements(nodes) => {
            for node in nodes.iter() {
                if let Some(token) = check_return(node) {
                    return Some(Error::new(
                        ErrorType::InvalidReturn,
                        token.position,
                        "Return statement cannot be in the global scope".to_string(),
                    ));
                }
            }
            None
        }
        _ => unreachable!(),
    }
}
