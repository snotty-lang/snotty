use super::{Error, ErrorType, Node, Token, TokenType, Type};
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum VarType {
    Variable(Type, Token),
    Function(Token, Vec<Type>),
    Struct(Vec<(Token, Type)>, Token),
}

/// Scope struct
/// It is used to find undefined variables and functions
#[derive(Debug, Clone)]
pub struct Scope {
    pub signatures: Vec<(Token, Vec<Type>, Type)>,
    pub structs: Vec<(Token, Vec<(Token, Type)>)>,
    pub defined: Vec<VarType>,
    pub args: Option<Vec<(Token, Type)>>,
    pub scopes: Vec<Scope>,
    pub parent: Option<Box<Scope>>,
}

impl Scope {
    pub fn new(parent: Option<&Scope>) -> Self {
        Self {
            structs: vec![],
            signatures: vec![],
            defined: vec![],
            scopes: vec![],
            args: None,
            parent: parent.map(|p| Box::new(p.clone())),
        }
    }

    pub fn register_struct(&mut self, struct_: Node) -> Option<Error> {
        let pos = struct_.position();
        if let Node::Struct(token, fields, _) = struct_ {
            if self
                .defined
                .iter()
                .any(|a| matches!(a, VarType::Struct(_, a) if *a == token))
            {
                return Some(Error::new(
                    ErrorType::Redefinition,
                    pos,
                    format!("Struct {} already defined", token),
                ));
            } else {
                self.defined.push(VarType::Struct(fields, token));
            }
        } else {
            unreachable!();
        }
        None
    }

    pub fn register_function(&mut self, func: Node) -> Option<Error> {
        let pos = func.position();
        if let Node::FuncDef(token, args, ..) = func {
            if self
                .defined
                .iter()
                .any(|a| matches!(a, VarType::Function(a, args1) if *a == token && args.len() == args1.len() && args1.iter().zip(args.iter()).all(|(a, (_, p))| a == p)))
            {
                return Some(Error::new(
                    ErrorType::Redefinition,
                    pos,
                    format!("Struct {} already defined", token),
                ));
            } else {
                self.defined.push(VarType::Function(
                    token,
                    args.iter().map(|a| a.1.clone()).collect(),
                ));
            }
        } else {
            unreachable!();
        }
        None
    }

    pub fn register_signature(&mut self, func: (Token, Vec<Type>, Type)) {
        self.signatures.push(func);
    }

    pub fn register_struct_premature(&mut self, struct_: (Token, Vec<(Token, Type)>)) {
        self.structs.push(struct_);
    }

    pub fn register_variable(&mut self, assign_node: Node) {
        if let Node::VarAssign(token, e, _) | Node::StaticVar(token, e) = assign_node {
            let t = e.get_type();
            self.defined.push(VarType::Variable(t, token));
        } else {
            unreachable!();
        }
    }

    pub fn access_variable(&mut self, node: &Node) -> Result<Type, Error> {
        match &node {
            Node::VarAccess(token, _) | Node::VarReassign(token, ..) => {
                if let Some(a) = self
                    .defined
                    .iter()
                    .rev()
                    .find(|a| matches!(a, VarType::Variable(_, n) if n == token))
                {
                    if let VarType::Variable(t, _) = a {
                        Ok(t.clone())
                    } else {
                        unreachable!();
                    }
                } else {
                    if self.args.is_some() {
                        if let Some(arg) =
                            self.args.as_ref().unwrap().iter().find(|t| t.0 == *token)
                        {
                            return Ok(arg.1.clone());
                        }
                    }
                    if let Some(ref mut parent) = self.parent {
                        return parent.access_variable(node);
                    }
                    return Err(Error::new(
                        ErrorType::UndefinedVariable,
                        token.position.clone(),
                        format!("Variable {} is not defined", token),
                    ));
                }
            }
            Node::IndexAssign(token, ..) | Node::Index(token, ..) => {
                if let Some(t) = self
                    .defined
                    .iter()
                    .rev()
                    .find(|a| matches!(a, VarType::Variable(_, n) if n == token))
                {
                    if let VarType::Variable(t, _) = t {
                        if let Type::Pointer(_) = t {
                            Ok(t.clone())
                        } else {
                            Err(Error::new(
                                ErrorType::SyntaxError,
                                token.position.clone(),
                                format!("Variable {} is not an array", token),
                            ))
                        }
                    } else {
                        unreachable!();
                    }
                } else {
                    if self.parent.is_some() {
                        let parent = self.parent.as_mut().unwrap();
                        return parent.access_variable(node);
                    }
                    if self.args.is_some() {
                        if let Some(arg) =
                            self.args.as_ref().unwrap().iter().find(|t| t.0 == *token)
                        {
                            return Ok(arg.1.clone());
                        }
                    }
                    return Err(Error::new(
                        ErrorType::UndefinedVariable,
                        token.position.clone(),
                        format!("Variable {} is not defined", token),
                    ));
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn access_variable_by_token(&mut self, token: &Token) -> Result<Type, Error> {
        if let Some(a) = self
            .defined
            .iter()
            .rev()
            .find(|a| matches!(a, VarType::Variable(_, n) if n == token))
        {
            if let VarType::Variable(t, _) = a {
                Ok(t.clone())
            } else {
                unreachable!();
            }
        } else {
            if self.args.is_some() {
                if let Some(arg) = self.args.as_ref().unwrap().iter().find(|t| t.0 == *token) {
                    return Ok(arg.1.clone());
                }
            }
            if self.parent.is_some() {
                let parent = self.parent.as_mut().unwrap();
                return parent.access_variable_by_token(token);
            }
            return Err(Error::new(
                ErrorType::UndefinedVariable,
                token.position.clone(),
                format!("Variable {} is not defined", token),
            ));
        }
    }

    pub fn access_array_by_token(&mut self, token: &Token, index: &Node) -> Result<Type, Error> {
        if let Some(t) = self
            .defined
            .iter()
            .rev()
            .find(|a| matches!(a, VarType::Variable(_, n) if n == token))
        {
            if let VarType::Variable(t, _) = t {
                if let Type::Pointer(t) = t {
                    Ok(*t.clone())
                } else {
                    Err(Error::new(
                        ErrorType::SyntaxError,
                        token.position.clone(),
                        format!("Variable {} is not an array", token),
                    ))
                }
            } else {
                unreachable!();
            }
        } else {
            if self.parent.is_some() {
                let parent = self.parent.as_mut().unwrap();
                return parent.access_array_by_token(token, index);
            }
            if self.args.is_some() {
                if let Some(arg) = self.args.as_ref().unwrap().iter().find(|t| t.0 == *token) {
                    return Ok(arg.1.clone());
                }
            }
            return Err(Error::new(
                ErrorType::UndefinedVariable,
                token.position.clone(),
                format!("Variable {} is not defined", token),
            ));
        }
    }

    pub fn access_function(&mut self, node: &Node) -> Result<Type, Error> {
        match &node {
            Node::Call(token1, args1, ..) => {
                if let Some(a) = self.signatures.iter().find(|(name, args, _)| {
                    name == token1
                        && args.len() == args1.len()
                        && args1
                            .iter()
                            .zip(args.iter())
                            .all(|(a, p)| a.get_type() == *p)
                }) {
                    Ok(a.2.clone())
                } else {
                    if let Some(ref mut parent) = self.parent {
                        return parent.access_function(node);
                    }
                    Err(Error::new(
                        ErrorType::UndefinedFunction,
                        token1.position.clone(),
                        format!("Function {} is not defined", token1),
                    ))
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn access_struct(&mut self, node: &Node) -> Result<Vec<(Token, Type)>, Error> {
        match &node {
            Node::StructConstructor(token1, attrs1, _) => {
                if let Some((_, attrs)) = self.structs.iter().find(|a| a.0 == *token1) {
                    if attrs.len() != attrs1.len()
                        || !attrs
                            .iter()
                            .all(|(t, _)| attrs1.iter().any(|(t1, _)| t1 == t))
                    {
                        return Err(Error::new(
                            ErrorType::TypeError,
                            token1.position.clone(),
                            format!("All fields of struct {} are not filled", token1,),
                        ));
                    }
                    if let Some((t, a, b)) = attrs
                        .iter()
                        .map(|(t, a)| {
                            if let Some((_, b)) = attrs1.iter().find(|(t1, _)| t1 == t) {
                                if *a != b.get_type() {
                                    Some((t, a, b))
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        })
                        .find(Option::is_some)
                        .flatten()
                    {
                        return Err(Error::new(
                            ErrorType::TypeError,
                            t.position.clone(),
                            format!(
                                "Field {} of struct {} has type {}, but the type passed is {}",
                                t,
                                token1,
                                a,
                                b.get_type()
                            ),
                        ));
                    }
                    Ok(attrs.clone())
                } else {
                    if let Some(ref mut parent) = self.parent {
                        return parent.access_struct(node);
                    }
                    Err(Error::new(
                        ErrorType::UndefinedStruct,
                        token1.position.clone(),
                        format!("Struct {} is not defined", token1),
                    ))
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn access_struct_by_token(&mut self, token: &Token) -> Result<Vec<(Token, Type)>, Error> {
        if let Some(a) = self.structs.iter().find(|a| a.0 == *token) {
            Ok(a.1.clone())
        } else {
            if let Some(ref mut parent) = self.parent {
                return parent.access_struct_by_token(token);
            }
            Err(Error::new(
                ErrorType::UndefinedStruct,
                token.position.clone(),
                format!("Struct {} is not defined", token),
            ))
        }
    }

    pub fn get_fields_by_token(&mut self, token: &Token) -> Option<&Vec<(Token, Type)>> {
        match token.token_type {
            TokenType::Identifier(ref t) => {
                if let Some(VarType::Struct(x, _)) = self.defined.iter().find(|a| {
                    if let VarType::Struct(
                        _,
                        Token {
                            token_type: TokenType::Identifier(ref name),
                            ..
                        },
                    ) = a
                    {
                        name == t
                    } else {
                        false
                    }
                }) {
                    return Some(x);
                }
            }
            _ => unreachable!(),
        }
        if let Some(ref mut parent) = self.parent {
            return parent.get_fields_by_token(token);
        }
        None
    }
}

impl fmt::Display for Scope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn delete_parent(scope: &mut Scope) {
            scope.parent = None;
            for child in &mut scope.scopes {
                delete_parent(child);
            }
        }
        let mut obj = self.clone();
        delete_parent(&mut obj);
        write!(f, "{:#?}", obj)
    }
}
