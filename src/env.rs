use std::collections::HashMap;
use std::rc::Rc;

use eval::{Val, Var};

#[derive(Debug, Clone)]
pub struct Env<'a> {
    parent: Option<Rc<Env<'a>>>,
    names: HashMap<String, Var<'a>>,
}

impl<'a> Env<'a> {
    pub fn with_primitives() -> Self {
        use prims::Prim::*;
        let mut names = HashMap::new();
        names.insert("+".into(), Var(Rc::new(Val::Prim(Plus))));
        names.insert("-".into(), Var(Rc::new(Val::Prim(Minus))));
        names.insert("*".into(), Var(Rc::new(Val::Prim(Times))));
        names.insert("/".into(), Var(Rc::new(Val::Prim(Divide))));
        names.insert("==".into(), Var(Rc::new(Val::Prim(Equals))));
        names.insert("!=".into(), Var(Rc::new(Val::Prim(NotEquals))));
        names.insert("<=".into(), Var(Rc::new(Val::Prim(LessEquals))));
        names.insert(">=".into(), Var(Rc::new(Val::Prim(GreaterEquals))));
        names.insert("<".into(), Var(Rc::new(Val::Prim(Less))));
        names.insert(">".into(), Var(Rc::new(Val::Prim(Greater))));
        names.insert("print".into(), Var(Rc::new(Val::Prim(Print))));
        names.insert("tuple".into(), Var(Rc::new(Val::Prim(MakeTuple))));
        names.insert("tuple/nth".into(), Var(Rc::new(Val::Prim(IndexTuple))));
        names.insert("list".into(), Var(Rc::new(Val::Prim(MakeList))));
        Env {
            parent: None,
            names: names,
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
