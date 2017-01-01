use std::collections::HashMap;

use eval::Var;

#[derive(Debug)]
pub struct Env<'a> {
    parent: Option<&'a Env<'a>>,
    names: HashMap<&'a str, Var>,
}

impl<'a> Env<'a> {
    pub fn new() -> Self {
        Env {
            parent: None,
            names: HashMap::new(),
        }
    }

    pub fn with_parent<'b>(parent: &'a Env<'b>) -> Env<'a> {
        Env {
            parent: Some(parent),
            names: HashMap::new(),
        }
    }

    pub fn lookup(&self, name: &str) -> Option<&Var> {
        self.names.get(name).or_else(|| {
            self.parent.and_then(|parent| parent.lookup(name))
        })
    }

    pub fn insert(&'a mut self, name: &'a str, var: Var) {
        self.names.insert(name, var);
    }
}
