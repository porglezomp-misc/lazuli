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
}
