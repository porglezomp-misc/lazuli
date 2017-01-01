use std::borrow::{Borrow, Cow};
use std::rc::Rc;
use std::fmt::{self, Display};

use ess::Sexp;

use ast::Ast;
use env::Env;
use prims::Prim;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Nil,
    Int,
    Float,
    Char,
    Str,
    Sym,
    Tuple,
    Fn,
}

#[derive(Debug, Clone)]
pub enum EvalError {
    NameLookup(String),
    InvalidFunction(String),
    InvalidName(String),
    BadArity {
        expected: usize,
        got: usize,
    },
    OutOfBounds {
        index: usize,
        length: usize,
    },
    TypeError {
        expected: Type,
        got: Type,
    },
}

#[derive(Debug, Clone)]
pub struct Var<'a>(pub Rc<Val<'a>>);

#[derive(Debug, Clone)]
pub enum Val<'a> {
    Nil,
    Int(i64),
    Float(f64),
    Char(char),
    Str(Cow<'a, str>),
    Sym(Cow<'a, str>),
    Tuple(Vec<Rc<Val<'a>>>),
    Fn(Vec<&'a str>, Ast<'a>),
    Prim(Prim),
    // TODO: Move these up a layer into a higher type so that they can't be confused with values
    Break(Rc<Val<'a>>),
    Return(Rc<Val<'a>>),
    Continue,
}

impl<'a> Display for Val<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Val::Nil => write!(fmt, "()"),
            Val::Int(i) => write!(fmt, "{}", i),
            Val::Float(f) => write!(fmt, "{:.0}", f),
            Val::Char(c) => write!(fmt, "#\\{}", c),
            Val::Str(ref s) => write!(fmt, "{:?}", s),
            Val::Sym(ref s) => write!(fmt, "{}", s),
            Val::Tuple(ref t) => {
                write!(fmt, "(tuple")?;
                for elem in t {
                    write!(fmt, " {}", elem)?;
                }
                write!(fmt, ")")
            }
            Val::Fn(..) => write!(fmt, "#fn<[anonymous]>"),
            Val::Prim(p) => write!(fmt, "#fn<[{:?}]>", p),
            Val::Break(ref val) => write!(fmt, "#break<{}>", val),
            Val::Return(ref val) => write!(fmt, "#return<{}>", val),
            Val::Continue => write!(fmt, "#continue"),
        }
    }
}

impl<'a> PartialEq for Val<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (&Val::Nil, &Val::Nil) => true,
            (&Val::Int(a), &Val::Int(b)) => a == b,
            (&Val::Int(a), &Val::Float(b)) => a as f64 == b && a == b as i64,
            (&Val::Float(a), &Val::Int(b)) => a as i64 == b && a == b as f64,
            (&Val::Float(a), &Val::Float(b)) => a == b,
            (&Val::Str(ref a), &Val::Str(ref b)) => a == b,
            (&Val::Sym(ref a), &Val::Sym(ref b)) => a == b,
            (&Val::Tuple(ref a), &Val::Tuple(ref b)) => a == b,
            (_, _) => false,
        }
    }
}

pub fn eval_literal<'a>(literal: &Sexp<'a>) -> Rc<Val<'a>> {
    Rc::new(match *literal {
        Sexp::Str(ref s, ..) => Val::Str(s.clone()),
        Sexp::Int(i, ..) => Val::Int(i),
        Sexp::Float(f, ..) => Val::Float(f),
        Sexp::Sym(ref s, ..) => Val::Sym(s.clone()),
        Sexp::Char(c, ..) => Val::Char(c),
        Sexp::List(ref v, ..) => {
            v.iter().rev().fold(Val::Nil, |cdr, car| {
                Val::Tuple(vec![eval_literal(car), Rc::new(cdr)])
            })
        }
    })
}

pub fn type_of_term(term: &Val) -> Type {
    match *term {
        Val::Nil => Type::Nil,
        Val::Int(..) => Type::Int,
        Val::Float(..) => Type::Float,
        Val::Char(..) => Type::Char,
        Val::Str(..) => Type::Str,
        Val::Sym(..) => Type::Sym,
        Val::Tuple(..) => Type::Tuple,
        Val::Fn(..) | Val::Prim(..) => Type::Fn,
        Val::Break(..) | Val::Continue | Val::Return(..) => unreachable!(),
    }
}

pub fn eval<'a>(ast: &Ast<'a>, env: Rc<Env<'a>>) -> Result<(Rc<Val<'a>>, Rc<Env<'a>>), EvalError> {
    match *ast {
        Ast::Nil => {
            Ok((Rc::new(Val::Nil), env))
        }
        Ast::Int(i) => {
            Ok((Rc::new(Val::Int(i)), env))
        }
        Ast::Float(f) => {
            Ok((Rc::new(Val::Float(f)), env))
        }
        Ast::Literal(ref val) => {
            Ok((eval_literal(val), env))
        }
        Ast::Var(ref name) => {
            env.lookup(name.borrow()).map(|x| (x.0.clone(), env.clone()))
                .ok_or_else(|| EvalError::NameLookup(name.clone().into_owned()))
        }
        Ast::Struct(name, ref members) => {
            let constructor = Ast::Call(Box::new(Ast::Var("tuple".into())), members.iter().map(|&member| {
                Ast::Var(member.into())
            }).collect());
            let constructor = Rc::new(Val::Fn(members.clone(), constructor));
            let accessors = members.iter().enumerate().map(|(i, &member)| {
                Rc::new(Val::Fn(vec![member],
                                Ast::Call(Box::new(Ast::Var("tuple/nth".into())), vec![
                                    Ast::Int(i as i64),
                                    Ast::Var(member.into()),
                                ])))
            });
            let mut env = (*env).clone();
            env.insert(format!("{}", name).into(), Var(constructor.clone()));
            for (member, accessor) in members.iter().zip(accessors) {
                env.insert(format!("{}/{}", name, member).into(), Var(accessor));
            }
            Ok((constructor, Rc::new(env)))
        }
        Ast::Defn(ref name, ref args, ref code) => {
            let func = Rc::new(Val::Fn(args.clone(), Ast::Begin(code.clone())));
            let mut env = (*env).clone();
            env.insert(name.clone().into_owned(), Var(func.clone()));
            Ok((func, Rc::new(env)))
        }
        Ast::Call(ref func, ref args) => {
            let (func, _) = eval(func, env.clone())?;
            match *func {
                Val::Fn(ref names, ref body) => {
                    if args.len() != names.len() {
                        return Err(EvalError::BadArity{ got: args.len(), expected: names.len() });
                    }
                    let mut call_env = Env::with_parent(env.clone());
                    for (&name, arg) in names.iter().zip(args) {
                        let (arg_val, _) = eval(arg, env.clone())?;
                        call_env.insert(name.into(), Var(arg_val));
                    }
                    Ok((eval(body, Rc::new(call_env))?.0, env))
                }
                Val::Prim(ref prim) => {
                    let args = args.iter().map(|arg| {
                        eval(arg, env.clone()).map(|x| x.0)
                    }).collect::<Result<_, _>>()?;
                    Ok((prim.call(&args)?, env))
                }
                _ => {
                    Err(EvalError::InvalidFunction(format!("{:#?}", func)))
                }
            }
        }
        Ast::Let(ref var, ref val) => {
            let name = match *var.clone() {
                Ast::Var(name) => name.into(),
                ref other => return Err(EvalError::InvalidName(format!("{:#?}", other))),
            };
            let (val, _) = eval(val, env.clone())?;
            let mut env = (*env).clone();
            env.insert(name, Var(val));
            Ok((Rc::new(Val::Nil), Rc::new(env)))
        }
        Ast::If(ref cond, ref if_true, ref if_false) => {
            let (cond, _) = eval(cond, env.clone())?;
            if let Val::Nil = *cond {
                Ok((eval(if_false, env.clone())?.0, env))
            } else {
                Ok((eval(if_true, env.clone())?.0, env))
            }
        }
        Ast::Break(None) => {
            Ok((Rc::new(Val::Break(Rc::new(Val::Nil))), env))
        }
        Ast::Break(Some(ref val)) => {
            let (val, _) = eval(&val, env.clone())?;
            Ok((Rc::new(Val::Break(val)), env))
        }
        Ast::Continue => {
            Ok((Rc::new(Val::Continue), env))
        }
        Ast::Return(None) => {
            Ok((Rc::new(Val::Return(Rc::new(Val::Nil))), env))
        }
        Ast::Return(Some(ref val)) => {
            let (val, _) = eval(&val, env.clone())?;
            Ok((Rc::new(Val::Return(val)), env))
        }
        Ast::Loop(ref body) => {
            loop {
                let mut loop_env = env.clone();
                for expr in body {
                    let (val, new_env) = eval(expr, loop_env)?;
                    loop_env = new_env;
                    if let Val::Break(ref val) = *val {
                        return Ok((val.clone(), env));
                    }
                    if let Val::Continue = *val { break; }
                    if let Val::Return(_) = *val {
                        return Ok((val, env));
                    }
                }
            }
        }
        Ast::Begin(ref body) => {
            let mut last_val = Rc::new(Val::Nil);
            let mut block_env = env.clone();
            for expr in body {
                let (new_val, new_env) = eval(expr, block_env)?;
                block_env = new_env;
                last_val = new_val;
                if let Val::Break(_) = *last_val { break; }
                if let Val::Continue = *last_val { break; }
                if let Val::Return(..) = *last_val {
                    return Ok((last_val, env));
                }
            }
            Ok((last_val, env))
        }
    }
}
