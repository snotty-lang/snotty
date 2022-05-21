use crate::utils::{
    Error, ErrorType, Node, Position, Scope, Token, TokenType, Type, ASSIGNMENT_OPERATORS,
};

/// A result type for parsing
type ParseResult = Result<Node, Error>;
type Signatures = Vec<(Token, Vec<Type>, Type)>;
type Structs = Vec<(Token, Vec<(Token, Type)>)>;

/// Parses the List of Tokens into an AST
#[derive(Clone)]
pub struct Parser {
    tokens: Vec<Token>,
    token_index: usize,
    current_token: Token,
    statics: Vec<String>,
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

    fn statements(
        &mut self,
        end_token: TokenType,
        global: bool,
        scope: &mut Scope,
    ) -> Result<(Node, Option<Type>), Error> {
        let mut statements = vec![];
        let mut pos = self.current_token.position.clone();
        if self.current_token.token_type == end_token {
            let end_pos = self.current_token.position.clone();
            pos.end = end_pos.end;
            pos.line_end = end_pos.line_end;
            self.advance();
            return Ok((Node::Statements(statements, Type::None, pos), None));
        }
        if !global {
            self.advance();
        }

        let mut type_ = None;

        while self.current_token.token_type != end_token {
            match self.current_token.token_type {
                TokenType::Eol => self.advance(),
                _ => {
                    let (n, t) = self.statement(scope)?;
                    if type_.is_none() {
                        type_ = t;
                    } else if let Some(t) = t {
                        if global {
                            return Err(Error::new(
                                ErrorType::InvalidReturn,
                                n.position(),
                                "Cannot return in global scope".to_string(),
                            ));
                        }
                        if *type_.as_ref().unwrap() != t {
                            return Err(Error::new(
                                ErrorType::TypeError,
                                n.position(),
                                format!("Expected type {:?} but found {:?}", type_, t),
                            ));
                        }
                    }
                    statements.push(n)
                }
            }
        }
        let end_pos = self.current_token.position.clone();
        pos.end = end_pos.end;
        pos.line_end = end_pos.line_end;
        self.advance();
        Ok((
            Node::Statements(statements, type_.clone().unwrap_or(Type::None), pos),
            type_,
        ))
    }

    fn statement(&mut self, scope: &mut Scope) -> Result<(Node, Option<Type>), Error> {
        let idx = self.token_index;
        match self.current_token.token_type {
            TokenType::Keyword(ref keyword) => match keyword.as_ref() {
                "while" => {
                    let mut pos = self.current_token.position.clone();
                    self.advance();
                    if self.current_token.token_type != TokenType::LParen {
                        return Err(Error::new(
                            ErrorType::SyntaxError,
                            self.current_token.position.clone(),
                            "Expected '(' after 'while'".to_string(),
                        ));
                    }
                    self.advance();
                    let condition = self.expression(scope)?;
                    if condition.get_type() != Type::Boolean {
                        return Err(Error::new(
                            ErrorType::TypeError,
                            condition.position(),
                            "Condition in a while loop can only be a bool".to_string(),
                        ));
                    }
                    if self.current_token.token_type != TokenType::RParen {
                        return Err(Error::new(
                            ErrorType::SyntaxError,
                            self.current_token.position.clone(),
                            format!("Expected ')' found '{}'", self.current_token.token_type),
                        ));
                    }
                    self.advance();
                    let (body, t) = self.statement(scope)?;
                    pos.end = body.position().end;
                    pos.line_end = body.position().line_end;
                    Ok((Node::While(Box::new(condition), Box::new(body), pos), t))
                }
                "return" => {
                    let pos = self.current_token.position.clone();
                    self.advance();
                    let expr = self.expression(scope)?;
                    let t = expr.get_type();
                    Ok((Node::Return(Box::new(expr), pos), Some(t)))
                }
                "let" => {
                    self.advance();
                    let node = self.assignment(true, scope)?;
                    scope.register_variable(node.clone());
                    Ok((node, None))
                }
                "static" => {
                    self.advance();
                    let node = self.static_assignment()?;
                    scope.register_variable(node.clone());
                    Ok((node, None))
                }
                "for" => {
                    let mut pos = self.current_token.position.clone();
                    self.advance();
                    if self.current_token.token_type != TokenType::LParen {
                        return Err(Error::new(
                            ErrorType::SyntaxError,
                            self.current_token.position.clone(),
                            "Expected '(' after 'for'".to_string(),
                        ));
                    }
                    self.advance();
                    let (init, ti) = self.statement(scope)?;
                    if self.current_token.token_type != TokenType::Colon {
                        return Err(Error::new(
                            ErrorType::SyntaxError,
                            self.current_token.position.clone(),
                            format!("Expected ':' found '{}'", self.current_token.token_type),
                        ));
                    }
                    self.advance();
                    let condition = self.expression(scope)?;
                    if condition.get_type() != Type::Boolean {
                        return Err(Error::new(
                            ErrorType::TypeError,
                            condition.position(),
                            "Condition in a for loop can only be a bool".to_string(),
                        ));
                    }
                    if self.current_token.token_type != TokenType::Colon {
                        return Err(Error::new(
                            ErrorType::SyntaxError,
                            self.current_token.position.clone(),
                            format!("Expected ':' found '{}'", self.current_token.token_type),
                        ));
                    }
                    self.advance();
                    let (step, ts) = self.statement(scope)?;
                    if self.current_token.token_type != TokenType::RParen {
                        return Err(Error::new(
                            ErrorType::SyntaxError,
                            self.current_token.position.clone(),
                            format!("Expected ')' found '{}'", self.current_token.token_type),
                        ));
                    }
                    self.advance();
                    let (body, tb) = self.statement(scope)?;
                    if matches!((&ti, &tb), (Some(ti), Some(tb)) if ti != tb) {
                        return Err(Error::new(
                            ErrorType::TypeError,
                            init.position(),
                            format!("Expected type {:?} but found {:?}", ti, tb),
                        ));
                    }
                    if matches!((&ti, &ts), (Some(ti), Some(ts)) if ti != ts) {
                        return Err(Error::new(
                            ErrorType::TypeError,
                            init.position(),
                            format!("Expected type {:?} but found {:?}", ti, ts),
                        ));
                    }
                    if matches!((&ts, &tb), (Some(ts), Some(tb)) if ts != tb) {
                        return Err(Error::new(
                            ErrorType::TypeError,
                            init.position(),
                            format!("Expected type {:?} but found {:?}", ts, tb),
                        ));
                    }
                    pos.end = body.position().end;
                    pos.line_end = body.position().end;
                    Ok((
                        Node::For(
                            Box::new(init),
                            Box::new(condition),
                            Box::new(step),
                            Box::new(body),
                            pos,
                        ),
                        match (ti, ts, tb) {
                            (Some(ti), ..) => Some(ti),
                            (.., Some(tb)) => Some(tb),
                            (_, Some(tc), _) => Some(tc),
                            _ => None,
                        },
                    ))
                }
                "if" => {
                    let mut pos = self.current_token.position.clone();
                    self.advance();
                    if self.current_token.token_type != TokenType::LParen {
                        return Err(Error::new(
                            ErrorType::SyntaxError,
                            self.current_token.position.clone(),
                            "Expected '(' after 'if'".to_string(),
                        ));
                    }
                    self.advance();
                    let condition = self.expression(scope)?;
                    if condition.get_type() != Type::Boolean {
                        return Err(Error::new(
                            ErrorType::TypeError,
                            condition.position(),
                            "Condition in an if statement can only be a bool".to_string(),
                        ));
                    }
                    if self.current_token.token_type != TokenType::RParen {
                        return Err(Error::new(
                            ErrorType::SyntaxError,
                            self.current_token.position.clone(),
                            format!("Expected ')' found '{}'", self.current_token.token_type),
                        ));
                    }
                    self.advance();
                    let (then_branch, tt) = self.statement(scope)?;
                    let (else_, end_pos, te) = if self.current_token.token_type
                        == TokenType::Keyword("else".to_string())
                    {
                        self.advance();
                        let (node, te) = self.statement(scope)?;
                        let pos = node.position();
                        (Some(Box::new(node)), pos, te)
                    } else {
                        (None, self.current_token.position.clone(), None)
                    };
                    if matches!((&tt, &te), (Some(tt), Some(te)) if tt != te) {
                        return Err(Error::new(
                            ErrorType::TypeError,
                            condition.position(),
                            format!("Expected type {:?} but found {:?}", tt, te),
                        ));
                    }
                    pos.end = end_pos.end;
                    pos.line_end = end_pos.line_end;
                    Ok((
                        Node::If(Box::new(condition), Box::new(then_branch), else_, pos),
                        match (tt, te) {
                            (Some(tt), _) => Some(tt),
                            (_, Some(te)) => Some(te),
                            _ => None,
                        },
                    ))
                }
                "ezascii" => {
                    let mut pos = self.current_token.position.clone();
                    self.advance();
                    let mut nodes = vec![self.expression(scope)?];
                    while let TokenType::Comma = self.current_token.token_type {
                        self.advance();
                        nodes.push(self.expression(scope)?);
                    }
                    pos.end = self.current_token.position.end;
                    pos.line_end = self.current_token.position.line_end;
                    Ok((Node::Ascii(nodes, pos), None))
                }
                "ezout" => {
                    let mut pos = self.current_token.position.clone();
                    self.advance();
                    let mut nodes = vec![self.expression(scope)?];
                    while let TokenType::Comma = self.current_token.token_type {
                        self.advance();
                        nodes.push(self.expression(scope)?);
                    }
                    pos.end = self.current_token.position.end;
                    pos.line_end = self.current_token.position.line_end;
                    Ok((Node::Print(nodes, pos), None))
                }
                "ez" => {
                    self.advance();
                    Ok((self.function_definition(scope)?, None))
                }
                "struct" => {
                    self.advance();
                    let node = self.struct_definition(&mut Some(scope))?;
                    scope.register_struct(node.clone());
                    Ok((node, None))
                }
                _ => Ok((self.expression(scope)?, None)),
            },
            TokenType::Identifier(_)
                if self.peek_type().is_some()
                    && ASSIGNMENT_OPERATORS.contains(&self.peek_type().unwrap()) =>
            {
                let node = self.assignment(false, scope)?;
                scope.access_variable(&node)?;
                Ok((node, None))
            }
            TokenType::LCurly => {
                let mut new_scope = Scope::new(Some(scope));
                let node = self.statements(TokenType::RCurly, false, &mut new_scope)?;
                scope.scopes.push(new_scope);
                Ok(node)
            }
            TokenType::Identifier(_) if matches!(self.peek_type(), Some(TokenType::LSquare)) => {
                let token = self.current_token.clone();
                self.advance();
                self.advance();
                let index = self.expression(scope)?;
                if self.current_token.token_type != TokenType::RSquare {
                    return Err(Error::new(
                        ErrorType::SyntaxError,
                        self.current_token.position.clone(),
                        format!("Expected ']', found {}", self.current_token),
                    ));
                }
                self.advance();
                let node = if !ASSIGNMENT_OPERATORS.contains(&self.current_token.token_type) {
                    self.token_index = idx;
                    self.current_token = self.tokens[idx].clone();
                    return Ok((self.expression(scope)?, None));
                } else if self.current_token.token_type == TokenType::Assign {
                    self.advance();
                    Node::IndexAssign(token, Box::new(index), Box::new(self.expression(scope)?))
                } else {
                    let op = self.current_token.clone();
                    self.advance();
                    let right = self.expression(scope)?;
                    let t = scope.access_variable_by_token(&token)?;
                    let rt = match right.get_type().get_result_type(&t, &op) {
                        Some(t) => t,
                        None => {
                            return Err(Error::new(
                                ErrorType::TypeError,
                                op.position.clone(),
                                format!(
                                    "Cannot apply operator {} to {} and {}",
                                    op,
                                    t,
                                    right.get_type()
                                ),
                            ))
                        }
                    };
                    Node::IndexAssign(
                        token.clone(),
                        Box::new(index),
                        Box::new(Node::BinaryOp(
                            op.un_augmented(),
                            Box::new(Node::VarAccess(token, t)),
                            Box::new(right),
                            rt,
                        )),
                    )
                };
                Ok((node, None))
            }
            TokenType::Mul => {
                let mut pos = self.current_token.position.clone();
                let idx = self.token_index;
                let node = self.expression(scope)?;
                if !ASSIGNMENT_OPERATORS.contains(&self.current_token.token_type) {
                    self.token_index = idx;
                    self.current_token = self.tokens[idx].clone();
                    Ok((self.expression(scope)?, None))
                } else if self.current_token.token_type == TokenType::Assign {
                    self.advance();
                    let right = self.expression(scope)?;
                    if node.get_type() != right.get_type() {
                        return Err(Error::new(
                            ErrorType::TypeError,
                            right.position(),
                            format!("Cannot assign {} to {}", right.get_type(), node.get_type()),
                        ));
                    }
                    pos.end = self.current_token.position.end;
                    pos.line_end = self.current_token.position.line_end;
                    Ok((
                        Node::DerefAssign(Box::new(node), Box::new(right), pos),
                        None,
                    ))
                } else {
                    let op = self.current_token.clone().un_augmented();
                    self.advance();
                    let right = self.expression(scope)?;
                    pos.end = self.current_token.position.end;
                    pos.line_end = self.current_token.position.line_end;
                    let t = node.get_type();
                    let rt = match t.get_result_type(&right.get_type(), &op) {
                        Some(t) => t,
                        None => {
                            return Err(Error::new(
                                ErrorType::TypeError,
                                right.position(),
                                format!(
                                    "Cannot apply operator {} to types {} and {}",
                                    op,
                                    t,
                                    right.get_type()
                                ),
                            ))
                        }
                    };
                    if rt != t {
                        return Err(Error::new(
                            ErrorType::TypeError,
                            right.position(),
                            format!("Cannot assign {} to {}", right.get_type(), t),
                        ));
                    }
                    let node = Node::DerefAssign(
                        Box::new(node.clone()),
                        Box::new(Node::BinaryOp(
                            op,
                            Box::new(Node::Deref(Box::new(node), t, pos.clone())),
                            Box::new(right),
                            rt,
                        )),
                        pos,
                    );
                    scope.access_variable(&node)?;
                    Ok((node, None))
                }
            }
            TokenType::Pow => {
                let mut pos = self.current_token.position.clone();
                let idx = self.token_index;
                let node = self.expression(scope)?;
                if !ASSIGNMENT_OPERATORS.contains(&self.current_token.token_type) {
                    self.token_index = idx;
                    self.current_token = self.tokens[idx].clone();
                    Ok((self.expression(scope)?, None))
                } else if self.current_token.token_type == TokenType::Assign {
                    self.advance();
                    pos.end = self.current_token.position.end;
                    pos.line_end = self.current_token.position.line_end;
                    Ok((
                        Node::DerefAssign(Box::new(node), Box::new(self.expression(scope)?), pos),
                        None,
                    ))
                } else {
                    let op = self.current_token.clone().un_augmented();
                    self.advance();
                    let right = self.expression(scope)?;
                    pos.end = self.current_token.position.end;
                    pos.line_end = self.current_token.position.line_end;
                    let t = node.get_type();
                    let rt = match t.get_result_type(&right.get_type(), &op) {
                        Some(t) => t,
                        None => {
                            return Err(Error::new(
                                ErrorType::TypeError,
                                op.position.clone(),
                                format!(
                                    "Cannot apply operator {} to types {} and {}",
                                    op,
                                    t,
                                    right.get_type()
                                ),
                            ))
                        }
                    };
                    let node = Node::DerefAssign(
                        Box::new(node.clone()),
                        Box::new(Node::BinaryOp(
                            op,
                            Box::new(Node::Deref(Box::new(node), t, pos.clone())),
                            Box::new(right),
                            rt,
                        )),
                        pos,
                    );
                    scope.access_variable(&node)?;
                    Ok((node, None))
                }
            }
            _ => Ok((self.expression(scope)?, None)),
        }
    }

    fn struct_definition(&mut self, scope: &mut Option<&mut Scope>) -> ParseResult {
        if let TokenType::Identifier(_) = self.current_token.token_type {
            let name = self.current_token.clone();
            let mut pos = name.position.clone();
            self.advance();
            match self.current_token.token_type {
                TokenType::Eol => {
                    self.advance();
                    pos.end = self.current_token.position.end;
                    pos.line_end = self.current_token.position.line_end;
                    Ok(Node::Struct(name, vec![], pos))
                }
                TokenType::LCurly => {
                    self.advance();
                    let mut fields = vec![];
                    if let TokenType::Identifier(_) = self.current_token.token_type {
                        let field = self.current_token.clone();
                        if fields.iter().any(|(f, _)| *f == field) {
                            return Err(Error::new(
                                ErrorType::Redefinition,
                                field.position,
                                format!("field name '{}' is already used", field.token_type),
                            ));
                        }
                        self.advance();
                        if self.current_token.token_type != TokenType::Colon {
                            return Err(Error::new(
                                ErrorType::SyntaxError,
                                self.current_token.position.clone(),
                                "Expected ':' after field name".to_string(),
                            ));
                        }
                        self.advance();
                        let field_type = self.make_type(scope)?;
                        fields.push((field, field_type));
                        while self.current_token.token_type == TokenType::Comma {
                            self.advance();
                            if self.current_token.token_type == TokenType::RCurly {
                                break;
                            }
                            if !matches!(self.current_token.token_type, TokenType::Identifier(_)) {
                                return Err(Error::new(
                                    ErrorType::SyntaxError,
                                    self.current_token.position.clone(),
                                    "Expected field name".to_string(),
                                ));
                            }
                            let field = self.current_token.clone();
                            self.advance();
                            if self.current_token.token_type != TokenType::Colon {
                                return Err(Error::new(
                                    ErrorType::SyntaxError,
                                    self.current_token.position.clone(),
                                    "Expected ':' after field name".to_string(),
                                ));
                            }
                            self.advance();
                            let field_type = self.make_type(scope)?;
                            fields.push((field, field_type));
                        }
                        if self.current_token.token_type != TokenType::RCurly {
                            return Err(Error::new(
                                ErrorType::SyntaxError,
                                self.current_token.position.clone(),
                                "Expected '}' after struct definition".to_string(),
                            ));
                        }
                        self.advance();
                        pos.end = self.current_token.position.end;
                        pos.line_end = self.current_token.position.line_end;
                        Ok(Node::Struct(name, fields, pos))
                    } else {
                        Err(Error::new(
                            ErrorType::SyntaxError,
                            self.current_token.position.clone(),
                            format!(
                                "Expected field name, found '{}'",
                                self.current_token.token_type
                            ),
                        ))
                    }
                }
                _ => Err(Error::new(
                    ErrorType::SyntaxError,
                    self.current_token.position.clone(),
                    "Expected Struct Declaration".to_string(),
                )),
            }
        } else {
            Err(Error::new(
                ErrorType::SyntaxError,
                self.current_token.position.clone(),
                format!(
                    "Expected identifier, found {}",
                    self.current_token.token_type
                ),
            ))
        }
    }

    fn make_type(&mut self, scope: &mut Option<&mut Scope>) -> Result<Type, Error> {
        match self.current_token.token_type {
            TokenType::Keyword(ref keyword) => match keyword.as_ref() {
                "int" => {
                    self.advance();
                    Ok(Type::Number)
                }
                "bool" => {
                    self.advance();
                    Ok(Type::Boolean)
                }
                "char" => {
                    self.advance();
                    Ok(Type::Char)
                }
                "struct" => {
                    self.advance();
                    if let TokenType::Identifier(_) = self.current_token.token_type {
                        let name = self.current_token.clone();
                        if let Some(scope) = scope {
                            scope.access_struct_by_token(&self.current_token)?;
                        }
                        self.advance();
                        Ok(Type::Struct(name))
                    } else {
                        Err(Error::new(
                            ErrorType::SyntaxError,
                            self.current_token.position.clone(),
                            "Expected struct name".to_string(),
                        ))
                    }
                }
                _ => Err(Error::new(
                    ErrorType::SyntaxError,
                    self.current_token.position.clone(),
                    format!("Expected type, found {}", keyword),
                )),
            },
            TokenType::BAnd => {
                self.advance();
                Ok(Type::Ref(Box::new(self.make_type(scope)?)))
            }
            TokenType::LAnd => {
                self.advance();
                Ok(Type::Ref(Box::new({
                    Type::Ref(Box::new(self.make_type(scope)?))
                })))
            }
            TokenType::Mul => {
                self.advance();
                Ok(Type::Pointer(Box::new(self.make_type(scope)?)))
            }
            TokenType::Pow => {
                self.advance();
                Ok(Type::Pointer(Box::new({
                    Type::Pointer(Box::new(self.make_type(scope)?))
                })))
            }
            TokenType::Eol => {
                self.advance();
                Ok(Type::None)
            }
            _ => Err(Error::new(
                ErrorType::SyntaxError,
                self.current_token.position.clone(),
                format!("Expected type, found {}", self.current_token),
            )),
        }
    }

    fn find_signs(&mut self) -> Result<(Signatures, Vec<String>, Structs), Error> {
        let mut scope = Scope::new(None);
        let mut signatures = vec![];
        let mut statics = vec![];
        let mut structs = vec![];
        while self.current_token.token_type != TokenType::Eof {
            match self.current_token.token_type {
                TokenType::Keyword(ref s) if s == "ez" => {
                    self.advance();
                    signatures.push(self.function_signature(&mut None)?)
                }
                TokenType::Keyword(ref s) if s == "struct" => {
                    self.advance();
                    let node = self.struct_definition(&mut None)?;
                    let (token, fields) = if let Node::Struct(token, fields, _) = node {
                        (token, fields)
                    } else {
                        unreachable!()
                    };
                    if structs.iter().any(|(i, _)| *i == token) {
                        return Err(Error::new(
                            ErrorType::Redefinition,
                            self.current_token.position.clone(),
                            format!("A struct with the name of {} already exists", token),
                        ));
                    }
                    scope.register_struct_premature((token.clone(), fields.clone()));
                    structs.push((token, fields))
                }
                TokenType::Keyword(ref s) if s == "static" => {
                    self.advance();
                    let node = self.static_assignment()?;
                    let ident = if let Node::StaticVar(
                        Token {
                            token_type: TokenType::Identifier(ident),
                            ..
                        },
                        _,
                    ) = node
                    {
                        ident
                    } else {
                        unreachable!()
                    };
                    if statics.contains(&ident) {
                        return Err(Error::new(
                            ErrorType::Redefinition,
                            self.current_token.position.clone(),
                            format!(
                                "A static variable with the name of '{}' already exists",
                                ident
                            ),
                        ));
                    }
                    statics.push(ident)
                }
                _ => self.advance(),
            }
        }
        Ok((signatures, statics, structs))
    }

    fn assignment(&mut self, init: bool, scope: &mut Scope) -> ParseResult {
        if let TokenType::Identifier(_) = self.current_token.token_type {
            let token = self.current_token.clone();
            self.advance();
            match self.current_token.token_type {
                TokenType::Assign if init => {
                    if let TokenType::Identifier(ref ident) = token.token_type {
                        if self.statics.contains(ident) {
                            return Err(Error::new(
                                ErrorType::Redefinition,
                                token.position,
                                format!(
                                    "A static variable with the name of '{}' already defined",
                                    ident
                                ),
                            ));
                        }
                    }
                    self.advance();
                    let expr = self.expression(scope)?;
                    let t = expr.get_type();
                    Ok(Node::VarAssign(token, Box::new(expr), t))
                }
                TokenType::Assign => {
                    self.advance();
                    let node = self.expression(scope)?;
                    let t = scope.access_variable_by_token(&token)?;
                    if node.get_type() != t {
                        return Err(Error::new(
                            ErrorType::TypeError,
                            node.position(),
                            format!("Cannot assign {} to {}", t, node.get_type()),
                        ));
                    }
                    Ok(Node::VarReassign(token, Box::new(node)))
                }
                ref x if ASSIGNMENT_OPERATORS.contains(x) && !init => {
                    let op = self.current_token.clone().un_augmented();
                    self.advance();
                    let right = self.expression(scope)?;
                    let t = scope.access_variable_by_token(&token)?;
                    let rt = match t.get_result_type(&right.get_type(), &op) {
                        Some(t) => t,
                        None => {
                            return Err(Error::new(
                                ErrorType::TypeError,
                                right.position(),
                                format!("Cannot assign {} to {}", right.get_type(), t),
                            ))
                        }
                    };
                    if rt != t {
                        return Err(Error::new(
                            ErrorType::TypeError,
                            right.position(),
                            format!("Cannot assign {} to {}", right.get_type(), t),
                        ));
                    }
                    Ok(Node::VarReassign(
                        token.clone(),
                        Box::new(Node::BinaryOp(
                            op,
                            Box::new(Node::VarAccess(token, t)),
                            Box::new(right),
                            rt,
                        )),
                    ))
                }
                _ => Err(Error::new(
                    ErrorType::SyntaxError,
                    self.current_token.position.clone(),
                    format!("Expected '=', found {}", self.current_token),
                )),
            }
        } else {
            Err(Error::new(
                ErrorType::SyntaxError,
                self.current_token.position.clone(),
                format!("Expected an identifier, found {}", self.current_token),
            ))
        }
    }

    fn static_assignment(&mut self) -> ParseResult {
        if let TokenType::Identifier(_) = self.current_token.token_type {
            let token = self.current_token.clone();
            self.advance();
            match self.current_token.token_type {
                TokenType::Assign => {
                    self.advance();
                    Ok(Node::StaticVar(token, Box::new(self.const_expression()?)))
                }
                _ => Err(Error::new(
                    ErrorType::SyntaxError,
                    self.current_token.position.clone(),
                    format!("Expected '=', found {}", self.current_token),
                )),
            }
        } else {
            Err(Error::new(
                ErrorType::SyntaxError,
                self.current_token.position.clone(),
                format!("Expected an identifier, found {}", self.current_token),
            ))
        }
    }

    fn expression(&mut self, scope: &mut Scope) -> ParseResult {
        let node = self.binary_op(
            Self::comparison,
            vec![TokenType::LAnd, TokenType::LOr, TokenType::LXor],
            Self::comparison,
            scope,
        )?;
        if self.current_token.token_type == TokenType::TernaryIf {
            self.advance();
            let then_branch = self.expression(scope)?;
            if self.current_token.token_type != TokenType::Colon {
                return Err(Error::new(
                    ErrorType::SyntaxError,
                    self.current_token.position.clone(),
                    format!("Expected ':', found {}", self.current_token),
                ));
            }
            self.advance();
            let else_branch = self.expression(scope)?;
            let t = then_branch.get_type();
            if t != else_branch.get_type() {
                return Err(Error::new(
                    ErrorType::TypeError,
                    else_branch.position(),
                    format!(
                        "The types of the branches of the ternary if expression must be the same, found {} and {}",
                        t, else_branch.get_type()
                    ),
                ));
            }
            let mut pos = node.position();
            let end_pos = else_branch.position();
            pos.end = end_pos.end;
            pos.line_end = end_pos.line_end;
            Ok(Node::Ternary(
                Box::new(node),
                Box::new(then_branch),
                Box::new(else_branch),
                t,
                pos,
            ))
        } else {
            Ok(node)
        }
    }

    fn comparison(&mut self, scope: &mut Scope) -> ParseResult {
        if let TokenType::LNot = self.current_token.token_type {
            let token = self.current_token.clone();
            self.advance();
            let node = self.comparison(scope)?;
            let t = match node.get_type().get_result_type_unary(&token) {
                Some(t) => t,
                None => {
                    return Err(Error::new(
                        ErrorType::TypeError,
                        node.position(),
                        format!("Cannot apply Logical Not to {}", node.get_type()),
                    ))
                }
            };
            Ok(Node::UnaryOp(token, Box::new(node), t))
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
                let t = match node.get_type().get_result_type_unary(&token) {
                    Some(t) => t,
                    None => {
                        return Err(Error::new(
                            ErrorType::TypeError,
                            node.position(),
                            format!("Cannot ++/-- type {}", node.get_type()),
                        ))
                    }
                };
                Ok(Node::UnaryOp(token, Box::new(node), t))
            }
            _ => {
                let node = self.power(scope)?;
                let token = self.current_token.clone();
                match token.token_type {
                    TokenType::Inc | TokenType::Dec => {
                        self.advance();
                        let t = match node.get_type().get_result_type_unary(&token) {
                            Some(t) => t,
                            None => {
                                return Err(Error::new(
                                    ErrorType::TypeError,
                                    node.position(),
                                    format!("Cannot ++/-- type {}", node.get_type()),
                                ))
                            }
                        };
                        Ok(Node::UnaryOp(token, Box::new(node), t))
                    }
                    _ => Ok(node),
                }
            }
        }
    }

    fn power(&mut self, scope: &mut Scope) -> ParseResult {
        self.binary_op(Self::convert, vec![TokenType::Pow], Self::convert, scope)
    }

    fn const_expression(&mut self) -> ParseResult {
        let node = self.const_binary_op(
            Self::const_comparison,
            vec![TokenType::LAnd, TokenType::LOr, TokenType::LXor],
            Self::const_comparison,
        )?;
        if self.current_token.token_type == TokenType::TernaryIf {
            self.advance();
            let then_branch = self.const_expression()?;
            if self.current_token.token_type != TokenType::Colon {
                return Err(Error::new(
                    ErrorType::SyntaxError,
                    self.current_token.position.clone(),
                    format!("Expected ':', found {}", self.current_token),
                ));
            }
            self.advance();
            let else_branch = self.const_expression()?;
            let t = then_branch.get_type();
            if t != else_branch.get_type() {
                return Err(Error::new(
                    ErrorType::TypeError,
                    else_branch.position(),
                    format!(
                        "The types of the branches of the ternary if expression must be the same, found {} and {}",
                        t, else_branch.get_type()
                    ),
                ));
            }
            let mut pos = node.position();
            let end_pos = else_branch.position();
            pos.end = end_pos.end;
            pos.line_end = end_pos.line_end;
            Ok(Node::Ternary(
                Box::new(node),
                Box::new(then_branch),
                Box::new(else_branch),
                t,
                pos,
            ))
        } else {
            Ok(node)
        }
    }

    fn const_comparison(&mut self) -> ParseResult {
        if let TokenType::LNot = self.current_token.token_type {
            let token = self.current_token.clone();
            self.advance();
            let node = self.const_comparison()?;
            let t = match node.get_type().get_result_type_unary(&token) {
                Some(t) => t,
                None => {
                    return Err(Error::new(
                        ErrorType::SyntaxError,
                        self.current_token.position.clone(),
                        format!("Expected a boolean, found {}", node.get_type()),
                    ))
                }
            };
            Ok(Node::UnaryOp(token, Box::new(node), t))
        } else {
            self.const_binary_op(
                Self::const_bitwise,
                vec![
                    TokenType::Eq,
                    TokenType::Neq,
                    TokenType::Lt,
                    TokenType::Gt,
                    TokenType::Le,
                    TokenType::Ge,
                ],
                Self::const_bitwise,
            )
        }
    }

    fn const_bitwise(&mut self) -> ParseResult {
        self.const_binary_op(
            Self::const_arithmetic,
            vec![
                TokenType::BAnd,
                TokenType::BOr,
                TokenType::BXor,
                TokenType::Shl,
                TokenType::Shr,
            ],
            Self::const_arithmetic,
        )
    }

    fn const_arithmetic(&mut self) -> ParseResult {
        self.const_binary_op(
            Self::const_term,
            vec![TokenType::Add, TokenType::Sub],
            Self::const_term,
        )
    }

    fn const_term(&mut self) -> ParseResult {
        self.const_binary_op(
            Self::const_factor,
            vec![TokenType::Mul, TokenType::Div, TokenType::Mod],
            Self::const_factor,
        )
    }

    fn const_factor(&mut self) -> ParseResult {
        let token = self.current_token.clone();
        match token.token_type {
            TokenType::Sub | TokenType::BNot | TokenType::Inc | TokenType::Dec => {
                self.advance();
                let node = self.const_factor()?;
                let t = match node.get_type().get_result_type_unary(&token) {
                    Some(t) => t,
                    None => {
                        return Err(Error::new(
                            ErrorType::TypeError,
                            self.current_token.position.clone(),
                            format!("Cannot ++/--/~ or negate type {}", node.get_type()),
                        ))
                    }
                };
                Ok(Node::UnaryOp(token, Box::new(node), t))
            }
            _ => {
                let node = self.const_power()?;
                let token = self.current_token.clone();
                match token.token_type {
                    TokenType::Inc | TokenType::Dec => {
                        self.advance();
                        let t = match node.get_type().get_result_type_unary(&token) {
                            Some(t) => t,
                            None => {
                                return Err(Error::new(
                                    ErrorType::TypeError,
                                    self.current_token.position.clone(),
                                    format!("Cannot ++/-- type {}", node.get_type()),
                                ))
                            }
                        };
                        Ok(Node::UnaryOp(token, Box::new(node), t))
                    }
                    _ => Ok(node),
                }
            }
        }
    }

    fn const_power(&mut self) -> ParseResult {
        self.const_binary_op(Self::const_atom, vec![TokenType::Pow], Self::const_atom)
    }

    fn convert(&mut self, scope: &mut Scope) -> ParseResult {
        let mut left = self.access_attr(scope)?;
        let mut token_type = self.current_token.token_type.clone();
        while let TokenType::Keyword(ref s) = token_type {
            if s != "as" {
                break;
            }
            let op = self.current_token.clone();
            self.advance();
            let right = self.make_type(&mut Some(scope))?;
            if !left.get_type().can_be_converted(&right) {
                return Err(Error::new(
                    ErrorType::TypeError,
                    op.position,
                    format!("Cannot convert type {} to type {}", left.get_type(), right),
                ));
            };
            left.convert(right);
            token_type = self.current_token.token_type.clone();
        }
        Ok(left)
    }

    fn access_attr(&mut self, scope: &mut Scope) -> ParseResult {
        let mut left = self.call(scope)?;
        while self.current_token.token_type == TokenType::Dot {
            self.advance();
            if !matches!(self.current_token.token_type, TokenType::Identifier(_)) {
                return Err(Error::new(
                    ErrorType::SyntaxError,
                    self.current_token.position.clone(),
                    "Expected identifier".to_string(),
                ));
            }
            let t = if let Some(t) = left.get_type().has_attr(&self.current_token) {
                t
            } else {
                return Err(Error::new(
                    ErrorType::TypeError,
                    self.current_token.position.clone(),
                    format!(
                        "Cannot access attribute {} on type {}",
                        self.current_token,
                        left.get_type()
                    ),
                ));
            };
            left = Node::AttrAccess(Box::new(left), self.current_token.clone(), t);
            self.advance();
        }
        Ok(left)
    }

    fn call(&mut self, scope: &mut Scope) -> ParseResult {
        if let TokenType::Identifier(_) = self.current_token.token_type {
            let atom = self.current_token.clone();
            let mut pos = self.current_token.position.clone();
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
                        self.current_token.position.clone(),
                        format!("Expected ')', found {}", self.current_token),
                    ));
                }
                self.advance();
                pos.end = self.current_token.position.end;
                pos.line_end = self.current_token.position.line_end;
                let mut node = Node::Call(atom, args, Type::None, pos);
                let t1 = scope.access_function(&node)?;
                if let Node::Call(_, _, ref mut t, _) = node {
                    *t = t1;
                }
                return Ok(node);
            } else if let TokenType::LCurly = self.current_token.token_type {
                self.advance();
                let mut fields = vec![];
                while self.current_token.token_type != TokenType::RCurly {
                    if let TokenType::Identifier(_) = self.current_token.token_type {
                        let field = self.current_token.clone();
                        self.advance();
                        if self.current_token.token_type != TokenType::Colon {
                            return Err(Error::new(
                                ErrorType::SyntaxError,
                                self.current_token.position.clone(),
                                format!("Expected ':', found {}", self.current_token),
                            ));
                        }
                        self.advance();
                        fields.push((field, self.expression(scope)?));
                        if self.current_token.token_type != TokenType::Comma {
                            break;
                        }
                        self.advance();
                    } else if self.current_token.token_type == TokenType::RCurly {
                        break;
                    } else {
                        return Err(Error::new(
                            ErrorType::SyntaxError,
                            self.current_token.position.clone(),
                            format!("Expected a field name, found {}", self.current_token),
                        ));
                    }
                }
                if self.current_token.token_type != TokenType::RCurly {
                    return Err(Error::new(
                        ErrorType::SyntaxError,
                        self.current_token.position.clone(),
                        format!("Expected '{{', found {}", self.current_token),
                    ));
                }
                self.advance();
                pos.end = self.current_token.position.end;
                pos.line_end = self.current_token.position.line_end;
                let node = Node::StructConstructor(atom, fields, pos);
                scope.access_struct(&node)?;
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
                "ezin" => {
                    self.advance();
                    Ok(Node::Input(token.position))
                }
                "true" => {
                    self.advance();
                    Ok(Node::Boolean(token))
                }
                "false" => {
                    self.advance();
                    Ok(Node::Boolean(token))
                }
                _ => Err(Error::new(
                    ErrorType::SyntaxError,
                    self.current_token.position.clone(),
                    format!("Unexpected keyword: {}", self.current_token),
                )),
            },
            TokenType::String(_) => {
                self.advance();
                Ok(Node::String(token))
            }
            TokenType::Eol => {
                self.advance();
                Ok(Node::None(token.position))
            }
            TokenType::Char(_) => {
                self.advance();
                Ok(Node::Char(token))
            }
            TokenType::Identifier(_) => {
                self.advance();
                if let TokenType::LSquare = self.current_token.token_type {
                    self.advance();
                    let index = self.expression(scope)?;
                    if self.current_token.token_type != TokenType::RSquare {
                        return Err(Error::new(
                            ErrorType::SyntaxError,
                            self.current_token.position.clone(),
                            format!("Expected ']', found {}", self.current_token),
                        ));
                    }
                    self.advance();
                    let mut pos = token.position.clone();
                    pos.end = self.current_token.position.end;
                    pos.line_end = self.current_token.position.line_end;
                    let t = scope.access_array_by_token(&token, &index)?;
                    Ok(Node::Index(token, Box::new(index), t, pos))
                } else {
                    let t = scope.access_variable_by_token(&token)?;
                    Ok(Node::VarAccess(token, t))
                }
            }
            TokenType::LParen => {
                self.advance();
                if self.current_token.token_type == TokenType::RParen {
                    let mut pos = token.position;
                    pos.end = self.current_token.position.end;
                    pos.line_end = self.current_token.position.line_end;
                    self.advance();
                    return Ok(Node::None(pos));
                }
                let node = self.expression(scope)?;
                if self.current_token.token_type != TokenType::RParen {
                    return Err(Error::new(
                        ErrorType::SyntaxError,
                        self.current_token.position.clone(),
                        format!("Expected ')', found {}", self.current_token),
                    ));
                }
                self.advance();
                Ok(node)
            }
            TokenType::LSquare => {
                self.advance();
                let mut elements = vec![];
                if self.current_token.token_type == TokenType::RSquare {
                    let mut pos = token.position;
                    pos.end = self.current_token.position.end;
                    pos.line_end = self.current_token.position.line_end;
                    self.advance();
                    return Ok(Node::Array(elements, Type::None, pos));
                }
                let e = self.expression(scope)?;
                let t = e.get_type();
                elements.push(e);
                while self.current_token.token_type == TokenType::Comma {
                    self.advance();
                    let e = self.expression(scope)?;
                    if e.get_type() != t {
                        return Err(Error::new(
                            ErrorType::TypeError,
                            e.position(),
                            format!(
                                "The types of the elements of the array must be the same, found {} and {}",
                                t, e.get_type()
                            ),
                        ));
                    }
                    elements.push(e);
                }
                if self.current_token.token_type != TokenType::RSquare {
                    return Err(Error::new(
                        ErrorType::SyntaxError,
                        self.current_token.position.clone(),
                        format!("Expected ']', found {}", self.current_token),
                    ));
                }
                let mut pos = token.position;
                pos.end = self.current_token.position.end;
                pos.line_end = self.current_token.position.line_end;
                self.advance();
                Ok(Node::Array(elements, t, pos))
            }
            TokenType::Number(_) => {
                self.advance();
                Ok(Node::Number(token))
            }
            TokenType::Mul => {
                let mut pos = token.position;
                pos.end = self.current_token.position.end;
                pos.line_end = self.current_token.position.line_end;
                self.advance();
                if matches!(self.current_token.token_type, TokenType::Keyword(ref s) if s == "point")
                {
                    self.advance();
                    Ok(Node::Pointer(Box::new(self.expression(scope)?), pos))
                } else {
                    let e = self.atom(scope)?;
                    let t = if let Type::Ref(t) | Type::Pointer(t) = e.get_type() {
                        *t
                    } else {
                        return Err(Error::new(
                            ErrorType::TypeError,
                            e.position(),
                            format!("Expected a reference or a pointer, found {}", e.get_type()),
                        ));
                    };
                    Ok(Node::Deref(Box::new(e), t, pos))
                }
            }
            TokenType::Pow => {
                self.advance();
                let mut pos = token.position;
                pos.end = self.current_token.position.end - 1;
                pos.line_end = self.current_token.position.line_end;
                let e = self.atom(scope)?;
                let (a, b) = if let Type::Ref(a) | Type::Pointer(a) = e.get_type() {
                    (
                        if let Type::Ref(b) | Type::Pointer(b) = *a.clone() {
                            *b
                        } else {
                            return Err(Error::new(
                                ErrorType::TypeError,
                                e.position(),
                                format!("Expected a reference or a pointer, found {}", a),
                            ));
                        },
                        *a,
                    )
                } else {
                    return Err(Error::new(
                        ErrorType::TypeError,
                        e.position(),
                        format!("Expected a reference or a pointer, found {}", e.get_type()),
                    ));
                };
                let node = Node::Deref(Box::new(e), a, pos.clone());
                pos.start += 1;
                Ok(Node::Deref(Box::new(node), b, pos))
            }
            TokenType::BAnd => {
                self.advance();
                let mut pos = token.position;
                pos.end = self.current_token.position.end;
                pos.line_end = self.current_token.position.line_end;
                let e = self.atom(scope)?;
                let t = e.get_type();
                Ok(Node::Ref(Box::new(e), t, pos))
            }
            TokenType::LAnd => {
                self.advance();
                let mut pos = token.position;
                pos.end = self.current_token.position.end - 1;
                pos.line_end = self.current_token.position.line_end;
                let e = self.atom(scope)?;
                let t = e.get_type();
                let node = Node::Ref(Box::new(e), t, pos.clone());
                let t = node.get_type();
                pos.start += 1;
                Ok(Node::Ref(Box::new(node), t, pos))
            }
            _ => Err(Error::new(
                ErrorType::SyntaxError,
                self.current_token.position.clone(),
                format!("Unexpected token: {}", self.current_token),
            )),
        }
    }

    fn const_atom(&mut self) -> ParseResult {
        let token = self.current_token.clone();
        match token.token_type {
            TokenType::Keyword(ref keyword) => match keyword.as_ref() {
                "true" => {
                    self.advance();
                    Ok(Node::Boolean(token))
                }
                "false" => {
                    self.advance();
                    Ok(Node::Boolean(token))
                }
                _ => Err(Error::new(
                    ErrorType::SyntaxError,
                    self.current_token.position.clone(),
                    format!("Unexpected keyword: {}", self.current_token),
                )),
            },
            TokenType::String(_) => {
                self.advance();
                Ok(Node::String(token))
            }
            TokenType::Char(_) => {
                self.advance();
                Ok(Node::Char(token))
            }
            TokenType::Number(_) => {
                self.advance();
                Ok(Node::Number(token))
            }
            TokenType::LParen => {
                self.advance();
                if self.current_token.token_type == TokenType::RParen {
                    let mut pos = token.position;
                    pos.end = self.current_token.position.end;
                    pos.line_end = self.current_token.position.line_end;
                    self.advance();
                    return Ok(Node::None(pos));
                }
                let node = self.const_expression()?;
                if self.current_token.token_type != TokenType::RParen {
                    return Err(Error::new(
                        ErrorType::SyntaxError,
                        self.current_token.position.clone(),
                        format!("Expected ')', found {}", self.current_token),
                    ));
                }
                self.advance();
                Ok(node)
            }
            _ => Err(Error::new(
                ErrorType::SyntaxError,
                self.current_token.position.clone(),
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
                self.current_token.position.clone(),
                format!("Expected identifier, found {}", self.current_token),
            ));
        };
        self.advance();
        if self.current_token.token_type != TokenType::LParen {
            return Err(Error::new(
                ErrorType::SyntaxError,
                self.current_token.position.clone(),
                format!("Expected '(', found {}", self.current_token),
            ));
        }
        self.advance();
        let mut params = vec![];
        if let TokenType::Identifier(_) = self.current_token.token_type {
            let p = self.current_token.clone();
            self.advance();
            if self.current_token.token_type != TokenType::Colon {
                return Err(Error::new(
                    ErrorType::SyntaxError,
                    self.current_token.position.clone(),
                    format!("Expected ':', found {}", self.current_token),
                ));
            }
            self.advance();
            let t = self.make_type(&mut Some(scope))?;
            params.push((p, t));
            while self.current_token.token_type == TokenType::Comma {
                self.advance();
                if let TokenType::Identifier(_) = self.current_token.token_type {
                    let p = self.current_token.clone();
                    self.advance();
                    if self.current_token.token_type != TokenType::Colon {
                        return Err(Error::new(
                            ErrorType::SyntaxError,
                            self.current_token.position.clone(),
                            format!("Expected ':', found {}", self.current_token),
                        ));
                    }
                    self.advance();
                    let t = self.make_type(&mut Some(scope))?;
                    params.push((p, t));
                } else {
                    return Err(Error::new(
                        ErrorType::SyntaxError,
                        self.current_token.position.clone(),
                        format!("Expected identifier, found {}", self.current_token),
                    ));
                }
            }
            if self.current_token.token_type != TokenType::RParen {
                return Err(Error::new(
                    ErrorType::SyntaxError,
                    self.current_token.position.clone(),
                    format!("Expected ')' or ',', found {}", self.current_token),
                ));
            }
        } else if self.current_token.token_type != TokenType::RParen {
            return Err(Error::new(
                ErrorType::SyntaxError,
                self.current_token.position.clone(),
                format!("Expected identifier or ')', found {}", self.current_token),
            ));
        }
        self.advance();

        let ret = if self.current_token.token_type == TokenType::Arrow {
            self.advance();
            self.make_type(&mut Some(scope))?
        } else {
            Type::None
        };

        let mut new_scope = Scope::new(Some(scope));
        new_scope.args = Some(params.clone());
        let (stmt, t) = self.statement(&mut new_scope)?;
        if *t.as_ref().unwrap_or(&Type::None) != ret {
            return Err(Error::new(
                ErrorType::TypeError,
                stmt.position(),
                format!(
                    "Expected return type {}, found {}",
                    ret,
                    t.unwrap_or(Type::None)
                ),
            ));
        }
        scope.scopes.push(new_scope);
        let mut pos = name.position.clone();
        pos.end = stmt.position().end;
        pos.line_end = stmt.position().line_end;
        Ok(Node::FuncDef(name, params, Box::new(stmt), ret, pos))
    }

    fn function_signature(
        &mut self,
        scope: &mut Option<&mut Scope>,
    ) -> Result<(Token, Vec<Type>, Type), Error> {
        let name = if let TokenType::Identifier(_) = self.current_token.token_type {
            self.current_token.clone()
        } else {
            return Err(Error::new(
                ErrorType::SyntaxError,
                self.current_token.position.clone(),
                format!("Expected identifier, found {}", self.current_token),
            ));
        };
        self.advance();
        if self.current_token.token_type != TokenType::LParen {
            return Err(Error::new(
                ErrorType::SyntaxError,
                self.current_token.position.clone(),
                format!("Expected '(', found {}", self.current_token),
            ));
        }
        self.advance();
        let mut params = vec![];
        if let TokenType::Identifier(_) = self.current_token.token_type {
            self.advance();
            if self.current_token.token_type != TokenType::Colon {
                return Err(Error::new(
                    ErrorType::SyntaxError,
                    self.current_token.position.clone(),
                    format!("Expected ':', found {}", self.current_token),
                ));
            }
            self.advance();
            let t = self.make_type(scope)?;
            params.push(t);
            while self.current_token.token_type == TokenType::Comma {
                self.advance();
                if let TokenType::Identifier(_) = self.current_token.token_type {
                    self.advance();
                    if self.current_token.token_type != TokenType::Colon {
                        return Err(Error::new(
                            ErrorType::SyntaxError,
                            self.current_token.position.clone(),
                            format!("Expected ':', found {}", self.current_token),
                        ));
                    }
                    self.advance();
                    let t = self.make_type(scope)?;
                    params.push(t);
                } else {
                    return Err(Error::new(
                        ErrorType::SyntaxError,
                        self.current_token.position.clone(),
                        format!("Expected identifier, found {}", self.current_token),
                    ));
                }
            }
            if self.current_token.token_type != TokenType::RParen {
                return Err(Error::new(
                    ErrorType::SyntaxError,
                    self.current_token.position.clone(),
                    format!("Expected ')' or ',', found {}", self.current_token),
                ));
            }
        } else if self.current_token.token_type != TokenType::RParen {
            return Err(Error::new(
                ErrorType::SyntaxError,
                self.current_token.position.clone(),
                format!("Expected identifier or ')', found {}", self.current_token),
            ));
        }
        self.advance();

        let ret = if self.current_token.token_type == TokenType::Arrow {
            self.advance();
            self.make_type(scope)?
        } else {
            Type::None
        };
        Ok((name, params, ret))
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
            let t = match left.get_type().get_result_type(&right.get_type(), &op) {
                Some(t) => t,
                None => {
                    return Err(Error::new(
                        ErrorType::TypeError,
                        op.position.clone(),
                        format!(
                            "Cannot apply operator {} to types {} and {}",
                            op,
                            left.get_type(),
                            right.get_type()
                        ),
                    ))
                }
            };
            left = Node::BinaryOp(op, Box::new(left), Box::new(right), t);
            token_type = self.current_token.token_type.clone();
        }
        Ok(left)
    }

    fn const_binary_op(
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
            let t = match left.get_type().get_result_type(&right.get_type(), &op) {
                Some(t) => t,
                None => {
                    return Err(Error::new(
                        ErrorType::TypeError,
                        op.position.clone(),
                        format!(
                            "Cannot apply operator {} to types {} and {}",
                            op,
                            left.get_type(),
                            right.get_type()
                        ),
                    ))
                }
            };
            left = Node::BinaryOp(op, Box::new(left), Box::new(right), t);
            token_type = self.current_token.token_type.clone();
        }
        Ok(left)
    }
}

/// Parses the given vector of tokens into an AST.
/// Returns the root node of the AST.
/// # Errors
/// If the tokens cannot be parsed into an AST, an error is returned.
pub fn parse(tokens: Vec<Token>) -> Result<(Node, Vec<Node>), Error> {
    let token = tokens[0].clone();
    let mut global = Scope::new(None);
    let mut obj = Parser {
        tokens,
        token_index: 0,
        current_token: token,
        statics: vec![],
    };
    let (signs, statics, structs) = obj.clone().find_signs()?;
    signs
        .iter()
        .for_each(|s| global.register_signature(s.clone()));
    structs
        .iter()
        .for_each(|s| global.register_struct_premature(s.clone()));
    obj.statics = statics;
    let mut ast = obj.statements(TokenType::Eof, true, &mut global)?.0;
    if let Some(err) = keyword_checks(&ast) {
        return Err(err);
    }
    if let Some(err) = check_recursive(&ast, &mut vec![]) {
        return Err(err);
    }
    if let Some(err) = check_numbers(&ast) {
        return Err(err);
    }
    let statics = get_static(&mut ast);
    if let Some(err) = expand_inline(&mut ast, vec![]) {
        return Err(err);
    }
    Ok((ast, statics))
}

/// Checks for invalid placement and use of keywords
fn keyword_checks(ast: &Node) -> Option<Error> {
    match ast {
        Node::Statements(nodes, ..) => {
            for node in nodes.iter() {
                if let Some(position) = check_return(node) {
                    return Some(Error::new(
                        ErrorType::InvalidReturn,
                        position,
                        "Return statement cannot be in the global scope".to_string(),
                    ));
                }
            }
            None
        }
        _ => unreachable!(),
    }
}

fn check_return(node: &Node) -> Option<Position> {
    match node {
        Node::BinaryOp(_, n1, n2, _)
        | Node::IndexAssign(_, n1, n2)
        | Node::While(n1, n2, _)
        | Node::DerefAssign(n1, n2, _) => {
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
        Node::For(n1, n2, n3, n4, _) => {
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
            let n4 = check_return(n4);
            if n4.is_some() {
                return n4;
            }
            None
        }
        Node::Struct(..) => None,
        Node::UnaryOp(_, n1, _) => check_return(n1),
        Node::Converted(n, _) => check_return(n),
        Node::VarAssign(_, n1, _) => check_return(n1),
        Node::AttrAccess(n, ..) => check_return(n),
        Node::StaticVar(_, n1) => check_return(n1),
        Node::VarAccess(..) => None,
        Node::VarReassign(_, n1) => check_return(n1),
        Node::Statements(nodes, ..) => {
            let mut ret = None;
            for node in nodes {
                let n = check_return(node);
                if n.is_some() {
                    ret = n;
                    break;
                }
            }
            ret
        }
        Node::StructConstructor(_, nodes, _) => {
            let mut ret = None;
            for (_, node) in nodes {
                let n = check_return(node);
                if n.is_some() {
                    ret = n;
                    break;
                }
            }
            ret
        }
        Node::Ternary(n1, n2, n3, ..) => {
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
        Node::If(n1, n2, n3, _) => {
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
        Node::Call(_, n1, ..) => {
            for i in n1.iter().map(check_return) {
                if i.is_some() {
                    return i;
                }
            }
            None
        }
        Node::Index(_, n1, ..) => check_return(n1),
        Node::FuncDef(..) => None,
        Node::Return(_, pos) => Some(pos.clone()),
        Node::Ref(n1, ..) | Node::Deref(n1, ..) | Node::Pointer(n1, ..) => check_return(n1),
        Node::Print(n1, _) | Node::Ascii(n1, _) => {
            for n in n1 {
                if let Some(t) = check_return(n) {
                    return Some(t);
                }
            }
            None
        }
        Node::String(_) => None,
        Node::Number(_) => None,
        Node::Boolean(_) => None,
        Node::Input(..) => None,
        Node::None(_) => None,
        Node::Char(..) => None,
        Node::Array(..) => None,
        Node::Expanded(..) => unreachable!(),
        // _ => None,
    }
}

/// Expands inline functions
fn expand_inline(ast: &mut Node, mut functions: Vec<Node>) -> Option<Error> {
    if let Some(mut functions2) = find_functions(ast) {
        let len = functions.len();
        functions.extend(functions2.iter().map(|f| (*f).clone()));
        for (i, f) in functions2.iter_mut().enumerate() {
            if let Node::FuncDef(_, _, f, ..) = f {
                if let err @ Some(_) = expand_inline(f, functions.clone()) {
                    return err;
                }
            }
            functions[i + len] = (*f).clone();
        }
        remove_inline(ast);
    }
    insert_function(ast, &functions)
    // println!("{ast}\n{:?}\n", functions.iter().map(|d| d.to_string()).collect::<Vec<_>>());
}

fn remove_inline(node: &mut Node) {
    match node {
        Node::FuncDef(.., p) => *node = Node::None(p.clone()),
        Node::Struct(..) => (),
        Node::Call(_, n, ..)
        | Node::Statements(n, ..)
        | Node::Print(n, _)
        | Node::Array(n, ..)
        | Node::Ascii(n, _) => {
            for n in n.iter_mut().rev() {
                remove_inline(n);
            }
        }
        Node::StructConstructor(_, n, _) => {
            for (_, n) in n {
                remove_inline(n);
            }
        }
        Node::IndexAssign(_, n1, n2)
        | Node::DerefAssign(n1, n2, _)
        | Node::If(n1, n2, None, _)
        | Node::While(n1, n2, _)
        | Node::BinaryOp(_, n1, n2, _) => {
            remove_inline(n1);
            remove_inline(n2);
        }
        Node::String(_) => (),
        Node::Number(_) => (),
        Node::Boolean(_) => (),
        Node::Index(_, n, ..)
        | Node::Ref(n, ..)
        | Node::Pointer(n, ..)
        | Node::Deref(n, ..)
        | Node::Return(n, ..)
        | Node::AttrAccess(n, ..)
        | Node::UnaryOp(_, n, _)
        | Node::Converted(n, _)
        | Node::VarAssign(_, n, _)
        | Node::StaticVar(_, n)
        | Node::VarReassign(_, n) => remove_inline(n),
        Node::VarAccess(..) => (),
        Node::Input(..) => (),
        Node::Ternary(n1, n2, n3, ..) | Node::If(n1, n2, Some(n3), _) => {
            remove_inline(n1);
            remove_inline(n2);
            remove_inline(n3);
        }
        Node::None(_) => (),
        Node::Char(_) => (),
        Node::For(n1, n2, n3, n4, _) => {
            remove_inline(n1);
            remove_inline(n2);
            remove_inline(n3);
            remove_inline(n4);
        }
        Node::Expanded(_, _) => unreachable!(),
    }
}

fn insert_function(node: &mut Node, functions: &[Node]) -> Option<Error> {
    match node {
        Node::Call(name, args, ..) => {
            let func = match functions.iter().find(|f| match f {
                Node::FuncDef(n, a, ..) => {
                    n == name
                        && args.len() == a.len()
                        && args
                            .iter()
                            .zip(a.iter())
                            .all(|(a, (_, p))| a.get_type() == *p)
                }
                _ => false,
            }) {
                Some(f) => f,
                None => {
                    return Some(Error::new(
                        ErrorType::UndefinedFunction,
                        name.position.clone(),
                        format!("Function {} is not defined", name),
                    ))
                }
            };
            let (params, body, ret) = match func {
                Node::FuncDef(_, p, b, ret, ..) => (p, b.clone(), ret),
                _ => unreachable!(),
            };
            let mut expanded = vec![];
            for ((arg, type_), param) in params.iter().zip(args) {
                expanded.push(Node::VarAssign(
                    arg.clone(),
                    Box::new(param.clone()),
                    type_.clone(),
                ))
            }
            expanded.push(*body);
            *node = Node::Expanded(expanded, ret.clone());
            None
        }
        Node::Statements(nodes, ..) => {
            for node in nodes.iter_mut().rev() {
                if let a @ Some(_) = insert_function(node, functions) {
                    return a;
                }
            }
            None
        }
        Node::StructConstructor(_, n, _) => {
            for (_, n) in n {
                if let a @ Some(_) = insert_function(n, functions) {
                    return a;
                }
            }
            None
        }
        Node::Print(n, _) | Node::Array(n, ..) | Node::Ascii(n, _) => {
            for n in n {
                if let a @ Some(_) = insert_function(n, functions) {
                    return a;
                }
            }
            None
        }
        Node::Struct(..) => None,
        Node::IndexAssign(_, n1, n2)
        | Node::DerefAssign(n1, n2, _)
        | Node::If(n1, n2, None, _)
        | Node::While(n1, n2, _)
        | Node::BinaryOp(_, n1, n2, _) => {
            if let a @ Some(_) = insert_function(n1, functions) {
                return a;
            }
            insert_function(n2, functions)
        }
        Node::Number(_) => None,
        Node::Boolean(_) => None,
        Node::Index(_, n, ..)
        | Node::Ref(n, ..)
        | Node::Deref(n, ..)
        | Node::Pointer(n, ..)
        | Node::Return(n, ..)
        | Node::Converted(n, _)
        | Node::FuncDef(_, _, n, ..)
        | Node::AttrAccess(n, ..)
        | Node::StaticVar(_, n)
        | Node::UnaryOp(_, n, _)
        | Node::VarAssign(_, n, _)
        | Node::VarReassign(_, n) => insert_function(n, functions),
        Node::VarAccess(..) => None,
        Node::String(_) => None,
        Node::Input(..) => None,
        Node::Ternary(n1, n2, n3, ..) | Node::If(n1, n2, Some(n3), _) => {
            if let a @ Some(_) = insert_function(n1, functions) {
                return a;
            }
            if let a @ Some(_) = insert_function(n2, functions) {
                return a;
            }
            insert_function(n3, functions)
        }
        Node::None(_) => None,
        Node::Char(_) => None,
        Node::For(n1, n2, n3, n4, _) => {
            if let a @ Some(_) = insert_function(n1, functions) {
                return a;
            }
            if let a @ Some(_) = insert_function(n2, functions) {
                return a;
            }
            if let a @ Some(_) = insert_function(n3, functions) {
                return a;
            }
            insert_function(n4, functions)
        }
        Node::Expanded(_, _) => unreachable!(),
    }
}

fn find_functions(node: &mut Node) -> Option<Vec<&mut Node>> {
    match node {
        Node::FuncDef(..) => Some(vec![node]),

        Node::Statements(nodes, ..) => {
            let mut new = vec![];
            for node in nodes.iter_mut().rev() {
                if let Some(ref mut i) = find_functions(node) {
                    new.append(i);
                }
            }
            Some(new)
        }
        Node::StructConstructor(_, n, _) => {
            for (_, n) in n {
                if let a @ Some(_) = find_functions(n) {
                    return a;
                }
            }
            None
        }
        Node::Call(_, n, ..) | Node::Print(n, _) | Node::Array(n, ..) | Node::Ascii(n, _) => {
            for n in n {
                if let a @ Some(_) = find_functions(n) {
                    return a;
                }
            }
            None
        }
        Node::Struct(..) => None,
        Node::IndexAssign(_, n1, n2)
        | Node::DerefAssign(n1, n2, _)
        | Node::If(n1, n2, None, _)
        | Node::While(n1, n2, _)
        | Node::BinaryOp(_, n1, n2, _) => {
            if let a @ Some(_) = find_functions(n1) {
                return a;
            }
            find_functions(n2)
        }
        Node::Number(_) => None,
        Node::Boolean(_) => None,
        Node::Index(_, n, ..)
        | Node::Ref(n, ..)
        | Node::Pointer(n, ..)
        | Node::Converted(n, _)
        | Node::Deref(n, ..)
        | Node::Return(n, ..)
        | Node::AttrAccess(n, ..)
        | Node::UnaryOp(_, n, _)
        | Node::VarAssign(_, n, _)
        | Node::StaticVar(_, n)
        | Node::VarReassign(_, n) => find_functions(n),
        Node::VarAccess(..) => None,
        Node::String(_) => None,
        Node::Input(..) => None,
        Node::Ternary(n1, n2, n3, ..) | Node::If(n1, n2, Some(n3), _) => {
            if let a @ Some(_) = find_functions(n1) {
                return a;
            }
            if let a @ Some(_) = find_functions(n2) {
                return a;
            }
            find_functions(n3)
        }
        Node::None(_) => None,
        Node::Char(_) => None,
        Node::For(n1, n2, n3, n4, _) => {
            if let a @ Some(_) = find_functions(n1) {
                return a;
            }
            if let a @ Some(_) = find_functions(n2) {
                return a;
            }
            if let a @ Some(_) = find_functions(n3) {
                return a;
            }
            find_functions(n4)
        }
        Node::Expanded(_, _) => unreachable!(),
    }
}

/// Checks for Recursive Functions
fn check_recursive(node: &Node, stack: &mut Vec<Token>) -> Option<Error> {
    match node {
        Node::FuncDef(t, _, body, ..) => {
            stack.push(t.clone());
            let s = check_recursive(body, stack);
            stack.pop();
            s
        }
        Node::Statements(nodes, ..) => {
            for node in nodes.iter().rev() {
                if let a @ Some(_) = check_recursive(node, stack) {
                    return a;
                }
            }
            None
        }
        Node::StructConstructor(_, n, _) => {
            for (_, n) in n {
                if let a @ Some(_) = check_recursive(n, stack) {
                    return a;
                }
            }
            None
        }
        Node::Print(n, _) | Node::Array(n, ..) | Node::Ascii(n, _) => {
            for n in n {
                if let a @ Some(_) = check_recursive(n, stack) {
                    return a;
                }
            }
            None
        }
        Node::Call(token, n, ..) => {
            if stack.iter().any(|t| t == token) {
                return Some(Error::new(
                    ErrorType::RecursionError,
                    token.position.clone(),
                    format!(
                        "Recursive function {} is calling itself",
                        stack.last().unwrap()
                    ),
                ));
            }
            for n in n {
                if let a @ Some(_) = check_recursive(n, stack) {
                    return a;
                }
            }
            None
        }
        Node::Struct(..) => None,
        Node::IndexAssign(_, n1, n2)
        | Node::DerefAssign(n1, n2, _)
        | Node::If(n1, n2, None, _)
        | Node::While(n1, n2, _)
        | Node::BinaryOp(_, n1, n2, _) => {
            if let a @ Some(_) = check_recursive(n1, stack) {
                return a;
            }
            check_recursive(n2, stack)
        }
        Node::Number(_) => None,
        Node::Boolean(_) => None,
        Node::Index(_, n, ..)
        | Node::Ref(n, ..)
        | Node::Deref(n, ..)
        | Node::Pointer(n, ..)
        | Node::Return(n, ..)
        | Node::Converted(n, _)
        | Node::AttrAccess(n, ..)
        | Node::StaticVar(_, n)
        | Node::UnaryOp(_, n, _)
        | Node::VarAssign(_, n, _)
        | Node::VarReassign(_, n) => check_recursive(n, stack),
        Node::VarAccess(..) => None,
        Node::String(_) => None,
        Node::Input(..) => None,
        Node::Ternary(n1, n2, n3, ..) | Node::If(n1, n2, Some(n3), _) => {
            if let a @ Some(_) = check_recursive(n1, stack) {
                return a;
            }
            if let a @ Some(_) = check_recursive(n2, stack) {
                return a;
            }
            check_recursive(n3, stack)
        }
        Node::None(_) => None,
        Node::Char(_) => None,
        Node::For(n1, n2, n3, n4, _) => {
            if let a @ Some(_) = check_recursive(n1, stack) {
                return a;
            }
            if let a @ Some(_) = check_recursive(n2, stack) {
                return a;
            }
            if let a @ Some(_) = check_recursive(n3, stack) {
                return a;
            }
            check_recursive(n4, stack)
        }
        Node::Expanded(_, _) => unreachable!(),
    }
}

fn get_static(ast: &mut Node) -> Vec<Node> {
    find_static(ast)
        .unwrap_or_default()
        .iter()
        .map(|n| (*n).clone())
        .collect::<Vec<_>>()
}

fn find_static(node: &mut Node) -> Option<Vec<&mut Node>> {
    match node {
        Node::Statements(nodes, ..) => {
            let mut new = vec![];
            for node in nodes.iter_mut().rev() {
                if let Some(ref mut i) = find_static(node) {
                    new.append(i);
                }
            }
            Some(new)
        }
        Node::StructConstructor(_, n, _) => {
            for (_, n) in n {
                if let a @ Some(_) = find_static(n) {
                    return a;
                }
            }
            None
        }
        Node::Call(_, n, ..) | Node::Print(n, _) | Node::Array(n, ..) | Node::Ascii(n, _) => {
            for n in n {
                if let a @ Some(_) = find_static(n) {
                    return a;
                }
            }
            None
        }
        Node::Struct(..) => None,
        Node::IndexAssign(_, n1, n2)
        | Node::DerefAssign(n1, n2, _)
        | Node::If(n1, n2, None, _)
        | Node::While(n1, n2, _)
        | Node::BinaryOp(_, n1, n2, _) => {
            if let a @ Some(_) = find_static(n1) {
                return a;
            }
            find_static(n2)
        }
        Node::Number(_) => None,
        Node::Boolean(_) => None,
        Node::Index(_, n, ..)
        | Node::Ref(n, ..)
        | Node::Pointer(n, ..)
        | Node::Deref(n, ..)
        | Node::Return(n, ..)
        | Node::UnaryOp(_, n, _)
        | Node::Converted(n, _)
        | Node::VarAssign(_, n, _)
        | Node::AttrAccess(n, ..)
        | Node::FuncDef(_, _, n, _, _)
        | Node::VarReassign(_, n) => find_static(n),
        Node::VarAccess(..) => None,
        Node::String(_) => None,
        Node::Input(..) => None,
        Node::Ternary(n1, n2, n3, ..) | Node::If(n1, n2, Some(n3), _) => {
            if let a @ Some(_) = find_static(n1) {
                return a;
            }
            if let a @ Some(_) = find_static(n2) {
                return a;
            }
            find_static(n3)
        }
        Node::None(_) => None,
        Node::Char(_) => None,
        Node::For(n1, n2, n3, n4, _) => {
            if let a @ Some(_) = find_static(n1) {
                return a;
            }
            if let a @ Some(_) = find_static(n2) {
                return a;
            }
            if let a @ Some(_) = find_static(n3) {
                return a;
            }
            find_static(n4)
        }
        Node::Expanded(_, _) => unreachable!(),
        Node::StaticVar(..) => Some(vec![node]),
    }
}

/// Checks for Numbers above the i8 limit
fn check_numbers(node: &Node) -> Option<Error> {
    match node {
        Node::BinaryOp(_, n1, n2, _)
        | Node::IndexAssign(_, n1, n2)
        | Node::While(n1, n2, _)
        | Node::DerefAssign(n1, n2, _) => {
            let n1 = check_numbers(n1);
            if n1.is_some() {
                return n1;
            }
            let n2 = check_numbers(n2);
            if n2.is_some() {
                return n2;
            }
            None
        }
        Node::For(n1, n2, n3, n4, _) => {
            let n1 = check_numbers(n1);
            if n1.is_some() {
                return n1;
            }
            let n2 = check_numbers(n2);
            if n2.is_some() {
                return n2;
            }
            let n3 = check_numbers(n3);
            if n3.is_some() {
                return n3;
            }
            let n4 = check_numbers(n4);
            if n4.is_some() {
                return n4;
            }
            None
        }
        Node::Struct(..) => None,
        Node::UnaryOp(op, expr, _)
            if op.token_type == TokenType::Sub
                && matches!(
                    **expr,
                    Node::Number(Token {
                        token_type: TokenType::Number(0..=256),
                        ..
                    })
                ) =>
        {
            None
        }
        Node::UnaryOp(_, n1, _) => check_numbers(n1),
        Node::Converted(n, _) => check_numbers(n),
        Node::VarAssign(_, n1, _) => check_numbers(n1),
        Node::AttrAccess(n, ..) => check_numbers(n),
        Node::StaticVar(_, n1) => check_numbers(n1),
        Node::VarAccess(..) => None,
        Node::VarReassign(_, n1) => check_numbers(n1),
        Node::Statements(nodes, ..) => {
            let mut ret = None;
            for node in nodes {
                let n = check_numbers(node);
                if n.is_some() {
                    ret = n;
                    break;
                }
            }
            ret
        }
        Node::StructConstructor(_, nodes, _) => {
            let mut ret = None;
            for (_, node) in nodes {
                let n = check_numbers(node);
                if n.is_some() {
                    ret = n;
                    break;
                }
            }
            ret
        }
        Node::Ternary(n1, n2, n3, ..) => {
            let n1 = check_numbers(n1);
            if n1.is_some() {
                return n1;
            }
            let n2 = check_numbers(n2);
            if n2.is_some() {
                return n2;
            }
            let n3 = check_numbers(n3);
            if n3.is_some() {
                return n3;
            }
            None
        }
        Node::If(n1, n2, n3, _) => {
            let n1 = check_numbers(n1);
            if n1.is_some() {
                return n1;
            }
            let n2 = check_numbers(n2);
            if n2.is_some() {
                return n2;
            }
            if let Some(n3) = n3 {
                let n3 = check_numbers(n3);
                if n3.is_some() {
                    return n3;
                }
            }
            None
        }
        Node::Call(_, n1, ..) => {
            for i in n1.iter().map(check_numbers) {
                if i.is_some() {
                    return i;
                }
            }
            None
        }
        Node::Index(_, n1, ..) => check_numbers(n1),
        Node::FuncDef(..) => None,
        Node::Return(n, _) => check_numbers(n),
        Node::Ref(n1, ..) | Node::Deref(n1, ..) | Node::Pointer(n1, ..) => check_numbers(n1),
        Node::Print(n1, _) | Node::Ascii(n1, _) => {
            for n in n1 {
                if let Some(t) = check_numbers(n) {
                    return Some(t);
                }
            }
            None
        }
        Node::String(_) => None,
        Node::Number(Token {
            token_type: TokenType::Number(0..=255),
            ..
        }) => None,
        Node::Number(_) => Some(Error::new(
            ErrorType::NumberTooLarge,
            node.position(),
            "Number is too large".to_string(),
        )),
        Node::Boolean(_) => None,
        Node::Input(..) => None,
        Node::None(_) => None,
        Node::Char(..) => None,
        Node::Array(..) => None,
        Node::Expanded(..) => unreachable!(),
    }
}
