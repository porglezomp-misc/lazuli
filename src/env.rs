use fnv::FnvHashMap;
use std::rc::Rc;

use eval::{Val, Value};

#[derive(Debug, Clone)]
pub struct Env<'a> {
    parent: Option<Rc<Env<'a>>>,
    names: FnvHashMap<String, Value<'a>>,
}

impl<'a> Env<'a> {
    pub fn with_primitives() -> Self {
        use prims::Prim::*;
        let mut names = FnvHashMap::default();
        names.insert("+".into(), Value::new(Val::Prim(Plus)));
        names.insert("-".into(), Value::new(Val::Prim(Minus)));
        names.insert("*".into(), Value::new(Val::Prim(Times)));
        names.insert("/".into(), Value::new(Val::Prim(Divide)));
        names.insert("==".into(), Value::new(Val::Prim(Equals)));
        names.insert("!=".into(), Value::new(Val::Prim(NotEquals)));
        names.insert("<=".into(), Value::new(Val::Prim(LessEquals)));
        names.insert(">=".into(), Value::new(Val::Prim(GreaterEquals)));
        names.insert("<".into(), Value::new(Val::Prim(Less)));
        names.insert(">".into(), Value::new(Val::Prim(Greater)));
        names.insert("print".into(), Value::new(Val::Prim(Print)));
        names.insert("tuple".into(), Value::new(Val::Prim(MakeTuple)));
        names.insert("tuple/nth".into(), Value::new(Val::Prim(IndexTuple)));
        names.insert("tuple/tag:set".into(), Value::new(Val::Prim(SetTupleTag)));
        names.insert("tuple/tag".into(), Value::new(Val::Prim(GetTupleTag)));
        names.insert("list".into(), Value::new(Val::Prim(MakeList)));
        names.insert("hole".into(), Value::new(Val::Prim(MakeHole)));
        names.insert("hole/fill".into(), Value::new(Val::Prim(FillHole)));
        Env {
            parent: None,
            names: names,
        }
    }

    pub fn with_parent(parent: Rc<Env<'a>>) -> Env<'a> {
        Env {
            parent: Some(parent),
            names: FnvHashMap::default(),
        }
    }

    pub fn lookup(&self, name: &str) -> Option<&Value<'a>> {
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

    pub fn insert(&mut self, name: String, var: Value<'a>) {
        self.names.insert(name, var);
    }
}
