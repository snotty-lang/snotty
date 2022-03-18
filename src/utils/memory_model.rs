use std::collections::HashMap;

use super::Val;

#[derive(Debug, Clone)]
pub struct Memory {
    pub last_memory_index: usize,
}

impl Memory {
    pub fn new() -> Self {
        Memory {
            last_memory_index: 2usize.pow(15),
        }
    }

    pub fn allocate(&mut self, size: usize) -> usize {
        let m = self.last_memory_index;
        self.last_memory_index += size;
        m
    }
}

impl Default for Memory {
    fn default() -> Self {
        Memory::new()
    }
}

#[derive(Clone)]
pub struct Variables {
    pub super_vars: Option<Box<Variables>>,
    pub vars: HashMap<String, Val>,
}

impl Variables {
    pub fn new() -> Self {
        Variables {
            super_vars: None,
            vars: HashMap::new(),
        }
    }

    pub fn new_from_parent(parent: Variables) -> Self {
        Variables {
            super_vars: Some(Box::new(parent)),
            vars: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: String, val: Val) {
        self.vars.insert(key, val);
    }

    pub fn get(&self, key: &str) -> Option<&Val> {
        if let a @ Some(_) = self.vars.get(key) {
            return a;
        }
        if let Some(ref super_vars) = self.super_vars {
            return super_vars.get(key);
        }
        None
    }

    pub fn get_mut(&mut self, key: &str) -> Option<&mut Val> {
        if let a @ Some(_) = self.vars.get_mut(key) {
            return a;
        }
        if let Some(ref mut super_vars) = self.super_vars {
            return super_vars.get_mut(key);
        }
        None
    }
}

impl Default for Variables {
    fn default() -> Self {
        Variables::new()
    }
}
