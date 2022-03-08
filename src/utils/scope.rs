use super::{Error, ErrorType, Node, Token, Type};
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum VarType {
    Variable(Type, Token),
    Function(Token, Vec<Type>),
    Struct(Vec<Type>, Token),
}

/// Scope struct
/// It is used to find undefined variables and functions
#[derive(Debug, Clone)]
pub struct Scope {
    pub signatures: Vec<(Token, Vec<Type>, Type)>,
    pub unresolved_structs: Vec<Node>,
    pub defined: Vec<VarType>,
    pub defined_static: Vec<(Type, Token)>,
    pub args: Option<Vec<(Token, Type)>>,
    pub scopes: Vec<Scope>,
    pub parent: Option<Box<Scope>>,
}

impl Scope {
    pub fn new(parent: Option<&Scope>) -> Self {
        Self {
            unresolved_structs: vec![],
            signatures: vec![],
            defined: vec![],
            scopes: vec![],
            args: None,
            defined_static: vec![],
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
                self.defined.push(VarType::Struct(
                    fields.iter().map(|a| a.1.clone()).collect(),
                    token,
                ));
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

    pub fn register_variable(&mut self, assign_node: Node) {
        if let Node::VarAssign(token, e, _) = assign_node {
            let t = e.get_type();
            self.defined.push(VarType::Variable(t, token));
        } else if let Node::StaticVar(token, e) = assign_node {
            let t = e.get_type();
            self.defined_static.push((t, token));
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
                } else if let Some((t, _)) = self
                    .defined_static
                    .iter()
                    .find(|a| matches!(a, (_, n) if n == token))
                {
                    Ok(t.clone())
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
        } else if let Some((t, _)) = self
            .defined_static
            .iter()
            .find(|a| matches!(a, (_, n) if n == token))
        {
            Ok(t.clone())
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
                if let Some(a) = self.signatures.iter().find(|a| {
                    let (name, args, _) = a;
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
        for node in self.unresolved_structs.clone() {
            self.access_struct(&node);
        }
    }

    pub fn has_static(&self, node: &Node) -> bool {
        let token = if let Node::StaticVar(token, ..) = node {
            token
        } else {
            unreachable!();
        };
        self.defined_static
            .iter()
            .any(|a| matches!(a, (_, n) if n == token))
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
