use super::{Error, ErrorType, Node, Token, TokenType, Type};
use std::fmt;

#[derive(Debug, Clone)]
pub enum VarType {
    Variable(Option<Type>, Token),
    Function(Vec<Type>, Type, Token),
}

impl VarType {
    fn get_name(&self) -> &Token {
        match self {
            VarType::Variable(_, name) => name,
            VarType::Function(_, _, name) => name,
        }
    }
}

/// Scope struct
/// It is used to find undefined variables and functions
#[derive(Debug, Clone)]
pub struct Scope {
    pub unresolved_functions: Vec<Node>,
    pub defined: Vec<VarType>,
    pub args: Option<Vec<Token>>,
    pub scopes: Vec<Scope>,
    pub parent: Option<Box<Scope>>,
    pub error: Option<Error>,
    pub func_error: Option<Error>,
}

impl Scope {
    pub fn new(parent: Option<&Scope>) -> Self {
        Self {
            unresolved_functions: Vec::new(),
            defined: Vec::new(),
            scopes: Vec::new(),
            args: None,
            parent: parent.map(|p| Box::new(p.clone())),
            error: None,
            func_error: None,
        }
    }

    pub fn register_function(&mut self, function: Node) {
        if self.error.is_some() {
            return;
        }
        if let Node::FuncDef(token, args, _, ret, _) = function {
            self.defined.push(VarType::Function(
                args.iter().map(|a| a.1.clone()).collect(),
                ret,
                token,
            ));
        }
    }

    pub fn register_variable(&mut self, assign_node: Node) {
        if self.error.is_some() {
            return;
        }
        if let Node::VarAssign(token, _, t) = assign_node {
            self.defined.push(VarType::Variable(t, token));
        } else {
            unreachable!();
        }
    }

    pub fn access_variable(&mut self, node: &Node) {
        if self.error.is_some() {
            return;
        }
        match &node {
            Node::VarAccess(token) | Node::VarReassign(token, ..) | Node::VarAssign(token, ..) => {
                if !self.defined.iter().any(|a| a.get_name() == token) {
                    if self.parent.is_some() {
                        let parent = self.parent.as_mut().unwrap();
                        parent.access_variable(node);
                        if parent.error.is_none() {
                            return;
                        }
                    }
                    if self.args.is_some() && self.args.as_ref().unwrap().contains(token) {
                        return;
                    }
                    self.error = Some(Error::new(
                        ErrorType::UndefinedVariable,
                        token.position.clone(),
                        format!("Variable {} is not defined", token),
                    ));
                }
            }
            Node::IndexAssign(token, index, ..) | Node::Index(token, index, ..) => {
                if let Some(t) = self.defined.iter().find(|a| a.get_name() == token) {
                    if let VarType::Variable(Some(t), _) = t {
                        if let Type::Array(_, l) = t {
                            if let Node::Number(n) = &**index {
                                let n = if let TokenType::Number(n) = n.token_type {
                                    n
                                } else {
                                    unreachable!();
                                };
                                if n as usize >= *l {
                                    self.error = Some(Error::new(
                                        ErrorType::IndexOutOfBounds,
                                        token.position.clone(),
                                        format!(
                                            "Length of the array is {}, but the index is {}",
                                            l, n
                                        ),
                                    ));
                                }
                            }
                        } else {
                            self.error = Some(Error::new(
                                ErrorType::SyntaxError,
                                token.position.clone(),
                                format!("Variable {} is not an array", token),
                            ));
                        }
                    }
                } else {
                    if self.parent.is_some() {
                        let parent = self.parent.as_mut().unwrap();
                        parent.access_variable(node);
                        if parent.error.is_none() {
                            return;
                        }
                    }
                    if self.args.is_some() && self.args.as_ref().unwrap().contains(token) {
                        return;
                    }
                    self.error = Some(Error::new(
                        ErrorType::UndefinedVariable,
                        token.position.clone(),
                        format!("Variable {} is not defined", token),
                    ));
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn access_function(&mut self, node: &Node) {
        if self.func_error.is_some() {
            return;
        }
        let mut found = false;
        match &node {
            Node::Call(token1, args1, _) => {
                for function in &self.defined {
                    match function {
                        VarType::Function(args2, _, token2) => {
                            if token1 == token2 {
                                if args1.len() != args2.len() {
                                    self.func_error = Some(Error::new(
                                        ErrorType::UndefinedFunction,
                                        token1.position.clone(),
                                        format!(
                                            "Function {} takes {} arguments, but {} given",
                                            token1,
                                            args2.len(),
                                            args1.len()
                                        ),
                                    ));
                                    return;
                                }
                                found = true;
                                break;
                            }
                        }
                        VarType::Variable(_, token2) => {
                            if token1 == token2 {
                                self.error = Some(Error::new(
                                    ErrorType::UndefinedFunction,
                                    token1.position.clone(),
                                    format!("Can only call a function, not a variable {}", token2),
                                ));
                                return;
                            }
                        }
                    }
                }
            }
            _ => unreachable!(),
        }
        if !found {
            if self.parent.is_some() {
                let parent = self.parent.as_mut().unwrap();
                let old = parent.unresolved_functions.len();
                parent.access_function(node);
                self.func_error = parent.func_error.clone();
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

    pub fn refresh(&mut self) {
        for node in self.unresolved_functions.clone() {
            self.access_function(&node);
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
