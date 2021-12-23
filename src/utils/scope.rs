use super::{Error, ErrorType, Node, Token};
use std::fmt;

#[derive(Debug, Clone)]
pub struct Scope {
    pub unresolved_functions: Vec<Node>,
    pub defined_functions: Vec<Node>,
    pub defined_variables: Vec<Token>,
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
            defined_functions: Vec::new(),
            defined_variables: Vec::new(),
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
        self.defined_functions.push(function);
    }

    pub fn register_variable(&mut self, assign_node: Node) {
        if self.error.is_some() {
            return;
        }
        if let Node::VarAssign(token, _) = assign_node {
            return self.defined_variables.push(token);
        }
        unreachable!();
    }

    pub fn access_variable(&mut self, node: Node) {
        if self.error.is_some() {
            return;
        }
        match &node {
            Node::VarAccess(token) | Node::VarReassign(token, _) => {
                if !self.defined_variables.contains(token) {
                    if self.parent.is_some() {
                        let parent = self.parent.as_mut().unwrap();
                        parent.access_variable(node.clone());
                        if parent.error.is_none() {
                            return;
                        }
                    }
                    if self.args.is_some() && self.args.as_ref().unwrap().contains(token) {
                        return;
                    }
                    self.error = Some(Error::new(
                        ErrorType::Parse,
                        token.position,
                        format!("Variable {:?} is not defined", token),
                    ));
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn access_function(&mut self, node: Node, force: bool) {
        if self.func_error.is_some() && !force {
            return;
        }
        let mut found = false;
        match &node {
            Node::Call(call_node, args1) => match &**call_node {
                Node::VarAccess(token1) => {
                    for function in &self.defined_functions {
                        match function {
                            Node::FuncDef(token2, args2, _) => {
                                if token1 == token2 {
                                    if args1.len() != args2.len() {
                                        self.func_error = Some(Error::new(
                                            ErrorType::Parse,
                                            token1.position,
                                            format!(
                                                "Function {:?} takes {} arguments, but {} given",
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
                            _ => unreachable!(),
                        }
                    }
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
        if !found {
            if self.parent.is_some() {
                let parent = self.parent.as_mut().unwrap();
                let old = parent.unresolved_functions.len();
                parent.access_function(node.clone(), force);
                self.error = parent.func_error.clone();
                if parent.unresolved_functions.len() <= old {
                    return self.unresolved_functions.retain(|n| n != &node);
                }
            }
            if !self.unresolved_functions.contains(dbg!(&node)) {
                self.unresolved_functions.push(node)
            }
        } else if self.unresolved_functions.contains(dbg!(&node)) {
            self.unresolved_functions.retain(|n| n != &node);
        }
    }

    pub fn refresh(&mut self) {
        for node in self.unresolved_functions.clone() {
            self.access_function(node, true);
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
