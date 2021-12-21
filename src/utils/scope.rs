use super::{Error, ErrorType, Node, Token};

#[derive(Debug, Clone)]
pub struct Scope {
    pub unresolved_functions: Vec<Node>,
    pub defined_functions: Vec<Node>,
    pub defined_variables: Vec<Token>,
    pub args: Option<Vec<Token>>,
    pub scopes: Vec<Scope>,
    pub parent: Option<Box<Scope>>,
    pub error: Option<Error>,
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
                if dbg!(!self.defined_variables.contains(&token)) {
                    if self.parent.is_some() {
                        let parent = self.parent.as_mut().unwrap();
                        parent.access_variable(node.clone());
                        if parent.error.is_none() {return}
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

    pub fn access_function(&mut self, node: Node) {
        if self.error.is_some() {
            return;
        }
        let mut found = false;
        match &node {
            Node::Call(node, args1) => match &**node {
                Node::VarAccess(token1) => {
                    for function in &self.defined_functions {
                        match function {
                            Node::FuncDef(token2, args2, _) => {
                                if token1 == token2 && args1.len() == args2.len() {
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
            self.unresolved_functions.push(node);
        }
    }

    pub fn refresh(&mut self) {
        for node in self.unresolved_functions.clone() {
            self.access_function(node);
        }
        dbg!(&self.unresolved_functions);
    }
}
