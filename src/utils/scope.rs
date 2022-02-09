use super::{Error, ErrorType, Node, Token, TokenType, Type};
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum VarType {
    Variable(Type, Token),
    Function(Vec<Type>, Type, Token),
    Struct(Vec<Type>, Token),
}

/// Scope struct
/// It is used to find undefined variables and functions
#[derive(Debug, Clone)]
pub struct Scope {
    pub unresolved_functions: Vec<Node>,
    pub unresolved_structs: Vec<Node>,
    pub defined: Vec<VarType>,
    pub args: Option<Vec<(Token, Type)>>,
    pub scopes: Vec<Scope>,
    pub parent: Option<Box<Scope>>,
}

impl Scope {
    pub fn new(parent: Option<&Scope>) -> Self {
        Self {
            unresolved_structs: vec![],
            unresolved_functions: vec![],
            defined: vec![],
            scopes: vec![],
            args: None,
            parent: parent.map(|p| Box::new(p.clone())),
        }
    }

    pub fn register_struct(&mut self, struct_: Node) -> Option<Error> {
        let pos = struct_.position();
        if let Node::Struct(token, fields, _) = struct_ {
            let s = VarType::Struct(fields.iter().map(|a| a.1.clone()).collect(), token.clone());
            if self.defined.contains(&s) {
                return Some(Error::new(
                    ErrorType::Redefinition,
                    pos,
                    format!("Struct {} already defined", token),
                ));
            } else {
                self.defined.push(s);
            }
        } else {
            unreachable!();
        }
        None
    }

    pub fn register_function(&mut self, function: Node) -> Option<Error> {
        let pos = function.position();
        if let Node::FuncDef(token, args, _, ret, _) = function {
            let func = VarType::Function(
                args.iter().map(|a| a.1.clone()).collect(),
                ret,
                token.clone(),
            );
            if self.defined.contains(&func) {
                Some(Error::new(
                    ErrorType::Redefinition,
                    pos,
                    format!("Function {} already defined", token),
                ))
            } else {
                self.defined.push(func);
                self.unresolved_functions.retain(|n| {
                    if let Node::Call(t, ..) = n {
                        return *t != token;
                    }
                    unreachable!()
                });
                None
            }
        } else {
            unreachable!();
        }
    }

    pub fn register_variable(&mut self, assign_node: Node) {
        if let Node::VarAssign(token, _, t) = assign_node {
            self.defined.push(VarType::Variable(t, token));
        } else {
            unreachable!();
        }
    }

    pub fn access_variable(&mut self, node: &Node) -> Result<Type, Error> {
        match &node {
            Node::VarAccess(token, _)
            | Node::VarReassign(token, ..)
            | Node::VarAssign(token, ..) => {
                if let Some(a) = self
                    .defined
                    .iter()
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
                    if self.parent.is_some() {
                        let parent = self.parent.as_mut().unwrap();
                        return parent.access_variable(node);
                    }
                    return Err(Error::new(
                        ErrorType::UndefinedVariable,
                        token.position.clone(),
                        format!("Variable {} is not defined", token),
                    ));
                }
            }
            Node::IndexAssign(token, index, ..) | Node::Index(token, index, ..) => {
                if let Some(t) = self
                    .defined
                    .iter()
                    .find(|a| matches!(a, VarType::Variable(_, n) if n == token))
                {
                    if let VarType::Variable(t, _) = t {
                        if let Type::Array(_, l) = t {
                            if let Node::Number(n) = &**index {
                                let n = if let TokenType::Number(n) = n.token_type {
                                    n
                                } else {
                                    unreachable!();
                                };
                                if n as usize >= *l {
                                    return Err(Error::new(
                                        ErrorType::IndexOutOfBounds,
                                        token.position.clone(),
                                        format!(
                                            "Length of the array is {}, but the index is {}",
                                            l, n
                                        ),
                                    ));
                                }
                            }
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
            .find(|a| matches!(a, VarType::Variable(_, n) if n == token))
        {
            if let VarType::Variable(t, _) = t {
                if let Type::Array(t, l) = t {
                    if let Node::Number(n) = index {
                        let n = if let TokenType::Number(n) = n.token_type {
                            n
                        } else {
                            unreachable!();
                        };
                        if n as usize >= *l {
                            return Err(Error::new(
                                ErrorType::IndexOutOfBounds,
                                token.position.clone(),
                                format!("Length of the array is {}, but the index is {}", l, n),
                            ));
                        }
                    }
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

    pub fn access_function(&mut self, node: &Node) {
        let mut found = false;
        match &node {
            Node::Call(token1, args1, ..) => {
                if self.defined.iter().any(|a| {
                    if let VarType::Function(args, _, name) = a {
                        name == token1 && args1.len() == args.len()
                    } else {
                        false
                    }
                }) {
                    found = true;
                }
            }
            _ => unreachable!(),
        }
        if !found {
            if let Some(ref mut parent) = self.parent {
                let old = parent.unresolved_functions.len();
                parent.access_function(node);
                if parent.unresolved_functions.len() <= old {
                    return self.unresolved_functions.retain(|n| n != node);
                }
            }
            if !self.unresolved_functions.contains(node) {
                self.unresolved_functions.push(node.clone())
            }
        } else if self.unresolved_functions.contains(node) {
            self.unresolved_functions.retain(|n| n != node);
        }
    }

    pub fn access_struct(&mut self, node: &Node) {
        let mut found = false;
        match &node {
            Node::StructConstructor(token1, args1, _) => {
                if self.defined.iter().any(|a| {
                    if let VarType::Struct(fields, name) = a {
                        name == token1 && args1.len() == fields.len()
                    } else {
                        false
                    }
                }) {
                    found = true;
                }
            }
            _ => unreachable!(),
        }
        if !found {
            if self.parent.is_some() {
                let parent = self.parent.as_mut().unwrap();
                let old = parent.unresolved_structs.len();
                parent.access_struct(node);
                if parent.unresolved_structs.len() <= old {
                    return self.unresolved_structs.retain(|n| n != node);
                }
            }
            if !self.unresolved_structs.contains(node) {
                self.unresolved_structs.push(node.clone())
            }
        } else if self.unresolved_structs.contains(node) {
            self.unresolved_structs.retain(|n| n != node);
        }
    }

    pub fn refresh(&mut self) {
        for node in self.unresolved_functions.clone() {
            self.access_function(&node);
        }
        for node in self.unresolved_structs.clone() {
            self.access_struct(&node);
        }
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
