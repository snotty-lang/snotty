use super::{Node, Token};

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub accessed_variables: Vec<Node>,
    pub accessed_functions: Vec<Node>,
    pub defined_functions: Vec<Node>,
    pub defined_variables: Vec<Node>,
    pub scopes: Vec<Scope>,
    parent: Option<Box<Scope>>,
}

impl Scope {
    pub fn new(parent: Option<&Scope>) -> Self {
        Self {
            accessed_variables: Vec::new(),
            accessed_functions: Vec::new(),
            defined_functions: Vec::new(),
            defined_variables: Vec::new(),
            scopes: Vec::new(),
            parent: parent.map(|p| Box::new(p.clone())),
        }
    }

    pub fn has_variable(&self, token: &Token) -> bool {
        for var in &self.defined_variables {
            match var {
                Node::VarAssign(t, _) => {
                    if t == token {
                        return true;
                    }
                }
                _ => unreachable!(),
            }
        }
        if self.parent.is_some() {
            self.parent.as_ref().unwrap().has_variable(token)
        } else {
            false
        }
    }

    pub fn has_function(&self, node: &Node, args: &[Node]) -> bool {
        true
    }
}
