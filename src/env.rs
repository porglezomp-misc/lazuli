use std::collections::HashMap;
use std::rc::Rc;

use eval::Var;

#[derive(Debug, Clone)]
pub struct Env<'a> {
    parent: Option<Rc<Env<'a>>>,
    names: HashMap<String, Var<'a>>,
}

impl<'a> Env<'a> {
    pub fn new() -> Self {
        Env {
            parent: None,
            names: HashMap::new(),
        }
    }

    pub fn with_parent(parent: Rc<Env<'a>>) -> Env<'a> {
        Env {
            parent: Some(parent),
            names: HashMap::new(),
        }
    }

    pub fn lookup(&self, name: &str) -> Option<&Var<'a>> {
        match self.names.get(name) {
            None => {
                match self.parent {
                    Some(ref parent) => {
                        parent.lookup(name)
                    }
                    None => None
                }
            }
            some => some
        }
    }

    pub fn insert(&mut self, name: String, var: Var<'a>) {
        self.names.insert(name, var);
    }
}
