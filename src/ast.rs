use sexp2::Sexp;

use std::borrow::{Cow, ToOwned};
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug)]
pub enum AstError<'a> {
    ArgList(Sexp<'a>),
    ArgListNoName,
    ArgListName(Sexp<'a>),
    ArgListArg(Sexp<'a>),
    InvalidInCall(Sexp<'a>),
    StructName(Sexp<'a>),
    StructMember(Sexp<'a>),
    NameLookup(Cow<'a, str>),
}

fn extend_cow<'a, T: ?Sized>(cow: &Cow<'a, T>) -> Cow<'static, T>
    where T: ToOwned
{
    Cow::Owned(cow.clone().into_owned())
}

impl<'a> AstError<'a> {
    pub fn to_owned(&self) -> AstError<'static> {
        use self::AstError::*;
        match *self {
            ArgList(ref s) => ArgList(s.to_owned()),
            ArgListNoName => ArgListNoName,
            ArgListName(ref s) => ArgListName(s.to_owned()),
            ArgListArg(ref s) => ArgListArg(s.to_owned()),
            InvalidInCall(ref s) => InvalidInCall(s.to_owned()),
            StructName(ref s) => StructName(s.to_owned()),
            StructMember(ref s) => StructMember(s.to_owned()),
            NameLookup(ref s) => NameLookup(extend_cow(s)),
        }
    }
}

type NameId = u64;

#[derive(Debug, Clone)]
pub enum Ast<'a> {
    Call(Box<Ast<'a>>, Vec<Ast<'a>>),
    If(Box<Ast<'a>>, Box<Ast<'a>>, Box<Ast<'a>>),
    Let(Box<Ast<'a>>, Box<Ast<'a>>),
    Set(Box<Ast<'a>>, Box<Ast<'a>>),
    Begin(Vec<Ast<'a>>),
    Loop(Vec<Ast<'a>>),
    Return(Option<Box<Ast<'a>>>),
    Break(Option<Box<Ast<'a>>>),
    Continue,
    Defn(&'a str, Vec<&'a str>, Vec<Ast<'a>>),
    Struct(&'a str, Vec<&'a str>),
    Var(&'a str),
    Name(NameId),
    Literal(&'a Sexp<'a>),
    Nil,
}

#[derive(Debug)]
pub struct Env<'a> {
    parent: Option<&'a Env<'a>>,
    names: HashMap<&'a str, NameId>,
    counter: Rc<RefCell<NameId>>,
}

impl<'a> Env<'a> {
    pub fn new() -> Self {
        Env {
            parent: None,
            names: HashMap::new(),
            counter: Rc::new(RefCell::new(0)),
        }
    }

    pub fn with_parent<'b>(parent: &'a Env<'b>) -> Env<'a> {
        Env {
            parent: Some(parent),
            names: HashMap::new(),
            counter: parent.counter.clone(),
        }
    }

    pub fn lookup<'b>(&self, name: &'b str) -> Result<NameId, AstError<'b>> {
        Err(AstError::NameLookup(name.into()))
    }
}

fn defn_get_args<'a>(arg_list: &'a Sexp<'a>)
-> Result<(&'a str, Vec<&'a str>), AstError<'a>> {
    match *arg_list {
        Sexp::List(ref xs, ..) => {
            let name = match xs.first() {
                Some(&Sexp::Sym(ref s, ..)) => Ok(s),
                Some(s) => Err(AstError::ArgListName(s.clone())),
                None => Err(AstError::ArgListNoName),
            }?;
            let args = xs[1..].iter().map(|arg| {
                match *arg {
                    Sexp::Sym(ref s, ..) => Ok(&s[..]),
                    ref s => Err(AstError::ArgListArg(s.clone())),
                }
            }).collect::<Result<_, _>>()?;
            Ok((name, args))
        }
        ref s => Err(AstError::ArgList(s.clone())),
    }
}

pub fn make_ast<'a>(sexp: &'a Sexp<'a>) -> Result<Ast<'a>, AstError<'a>> {
    match *sexp {
        Sexp::List(ref xs, ..) => match xs.first() {
            Some(x) => match *x {
                Sexp::Sym(ref s, ..) if s == "if" => {
                    // TODO: Not too many args
                    // TODO: Optional second arg?
                    let cond = make_ast(&xs[1])?;
                    let if_true = make_ast(&xs[2])?;
                    let if_false = make_ast(&xs[3])?;
                    Ok(Ast::If(Box::new(cond), Box::new(if_true), Box::new(if_false)))
                }
                Sexp::Sym(ref s, ..) if s == "let!" => {
                    // TODO: Not too many args
                    let var = make_ast(&xs[1])?;
                    let val = make_ast(&xs[2])?;
                    Ok(Ast::Let(Box::new(var), Box::new(val)))
                }
                Sexp::Sym(ref s, ..) if s == "set!" => {
                    // TODO: Not too many args
                    let var = make_ast(&xs[1])?;
                    let val = make_ast(&xs[2])?;
                    Ok(Ast::Set(Box::new(var), Box::new(val)))
                }
                Sexp::Sym(ref s, ..) if s == "begin" => {
                    let body = xs[1..].iter().map(make_ast)
                        .collect::<Result<_, _>>()?;
                    Ok(Ast::Begin(body))
                }
                Sexp::Sym(ref s, ..) if s == "loop" => {
                    let body = xs[1..].iter().map(make_ast)
                        .collect::<Result<_, _>>()?;
                    Ok(Ast::Loop(body))
                }
                Sexp::Sym(ref s, ..) if s == "return" => {
                    // TODO: Assert not too many args
                    let result = if let Some(res) = xs.get(1) {
                        Some(Box::new(make_ast(&res)?))
                    } else {
                        None
                    };
                    Ok(Ast::Return(result))
                }
                Sexp::Sym(ref s, ..) if s == "break" => {
                    // TODO: Assert not too many args
                    let result = if let Some(res) = xs.get(1) {
                        Some(Box::new(make_ast(&res)?))
                    } else {
                        None
                    };
                    Ok(Ast::Break(result))
                }
                Sexp::Sym(ref s, ..) if s == "continue" => {
                    // TODO: Assert not too many args
                    Ok(Ast::Continue)
                }
                Sexp::Sym(ref s, ..) if s == "defn" => {
                    let (name, args) = defn_get_args(&xs[1])?;
                    let body = xs[2..].iter()
                        .map(make_ast)
                        .collect::<Result<_, _>>()?;
                    Ok(Ast::Defn(name, args, body))
                }
                Sexp::Sym(ref s, ..) if s == "struct" => {
                    let name = match xs[1] {
                        Sexp::Sym(ref name, ..) => Ok(name),
                        ref s => {
                            Err(AstError::StructName(s.clone()))
                        }
                    }?;
                    let members = xs[2..].iter().map(|x| {
                        match *x {
                            Sexp::Sym(ref name, ..) => Ok(&name[..]),
                            ref s => Err(AstError::StructMember(s.clone())),
                        }
                    }).collect::<Result<_, _>>()?;
                    Ok(Ast::Struct(name, members))
                }
                Sexp::Sym(ref s, ..) => {
                    let func = Ast::Var(s);
                    let args = xs[1..].iter()
                        .map(make_ast)
                        .collect::<Result<_, _>>()?;
                    Ok(Ast::Call(Box::new(func), args))
                }
                ref s @ Sexp::List(..) => {
                    let func = make_ast(s)?;
                    let args = xs[1..].iter()
                        .map(make_ast)
                        .collect::<Result<_, _>>()?;
                    Ok(Ast::Call(Box::new(func), args))
                }
                ref s => Err(AstError::InvalidInCall(s.clone())),
            },
            None => Ok(Ast::Nil),
        },
        Sexp::Sym(ref s, ..) => Ok(Ast::Var(s)),
        ref s @ Sexp::Str(..) => Ok(Ast::Literal(s)),
        ref s @ Sexp::Char(..) => Ok(Ast::Literal(s)),
        ref s @ Sexp::Int(..) => Ok(Ast::Literal(s)),
        ref s @ Sexp::Float(..) => Ok(Ast::Literal(s)),
    }
}

/*
pub fn resolve_names<'a, 'b>(env: &'a Env<'a>, ast: &Ast<'b>) -> Result<Ast<'b>, AstError<'b>> {
    match *ast {
        Ast::Var(ref name) => env.lookup(&name).map(Ast::Name),
        Ast::Defn(_, _, _) => unimplemented!(),
        Ast::Struct(ref name, ref members) => {
            unimplemented!()
        }
        Ast::Call(ref func, ref args) => {
            let mut new_args = Vec::with_capacity(args.len());
            for arg in args {
                new_args.push(resolve_names(&env, arg)?);
            }
            Ok(Ast::Call(Box::new(resolve_names(env, func)?), new_args))
        }
        Ast::If(ref cond, ref if_true, ref if_false) => {
            let cond = resolve_names(env, cond)?;
            let if_true = resolve_names(env, if_true)?;
            let if_false = resolve_names(env, if_false)?;
            Ok(Ast::If(Box::new(cond), Box::new(if_true), Box::new(if_false)))
        }
        Ast::Let(ref var, ref val) => {
            let var = resolve_names(env, var)?;
            let val = resolve_names(env, val)?;
            // TODO: Add var to the env if appropriate
            Ok(Ast::Let(Box::new(var), Box::new(val)))
        }
        Ast::Set(ref var, ref val) => {
            let var = resolve_names(env, var)?;
            let val = resolve_names(env, val)?;
            Ok(Ast::Set(Box::new(var), Box::new(val)))
        }
        Ast::Begin(ref body) => {
            let mut env = Env::with_parent(env);
            let mut new_body = Vec::with_capacity(body.len());
            for item in body {
                new_body.push(resolve_names(&env, item)?);
            }
            Ok(Ast::Begin(new_body))
        }
        Ast::Loop(ref body) => {
            let mut env = Env::with_parent(env);
            let mut new_body = Vec::with_capacity(body.len());
            for item in body {
                new_body.push(resolve_names(&env, item)?);
            }
            Ok(Ast::Loop(new_body))
        }
        Ast::Return(ref s) => {
            match *s {
                Some(ref x) => Ok(Ast::Return(Some(Box::new(resolve_names(env, x)?)))),
                None => Ok(Ast::Return(None)),
            }
        }
        Ast::Break(ref s) => {
            match *s {
                Some(ref x) => Ok(Ast::Break(Some(Box::new(resolve_names(env, x)?)))),
                None => Ok(Ast::Break(None)),
            }
        }
        Ast::Continue => Ok(Ast::Continue),
        ref lit@Ast::Literal(_) => Ok(lit.clone()),
        Ast::Name(name) => Ok(Ast::Name(name)),
        Ast::Nil => Ok(Ast::Nil),
    }
}
*/
