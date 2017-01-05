use std::borrow::{Borrow, Cow};
use std::cell::{Ref, RefCell};
use std::fmt::{self, Display};
use std::rc::Rc;

use ess::Sexp;
use ess::span::ByteSpan;

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
    Hole,
    Fn,
}

#[derive(Debug, Clone)]
pub enum EvalError {
    NameLookup(String),
    InvalidFunction(String),
    InvalidName(String),
    ContinueOutsideLoop,
    BreakOutsideLoop,
    BadArity {
        at: Option<ByteSpan>,
        expected: usize,
        got: usize,
    },
    OutOfBounds {
        at: Option<ByteSpan>,
        index: usize,
        length: usize,
    },
    TypeError {
        at: Option<ByteSpan>,
        expected: Type,
        got: Type,
    },
    If(Box<EvalError>, ByteSpan),
    Call(Box<EvalError>, ByteSpan),
    Let(Box<EvalError>, ByteSpan),
    Break(Box<EvalError>, ByteSpan),
    Return(Box<EvalError>, ByteSpan),
    Continue(Box<EvalError>, ByteSpan),
    Loop(Box<EvalError>, ByteSpan),
    Begin(Box<EvalError>, ByteSpan),
}

impl EvalError {
    pub fn get_cause(&self) -> Option<&EvalError> {
        use self::EvalError::*;
        match *self {
            If(ref e, ..) | Call(ref e, ..) | Let(ref e, ..) |
            Break(ref e, ..) | Return(ref e, ..) | Loop(ref e, ..) |
            Begin(ref e, ..) => Some(e),
            _ => None,
        }
    }

    pub fn get_span(&self) -> Option<ByteSpan> {
        use self::EvalError::*;
        match *self {
            If(.., l) | Call(.., l) | Let(.., l) |
            Break(.., l) | Return(.., l) | Loop(.., l) |
            Begin(.., l) => Some(l),
            OutOfBounds { at: l, ..} | BadArity { at: l, .. }
            | TypeError { at: l, .. } => l,
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Val<'a> {
    Nil,
    Int(i64),
    Float(f64),
    Char(char),
    Str(Cow<'a, str>),
    Sym(Cow<'a, str>),
    Tuple(Vec<Value<'a>>, Option<Cow<'a, str>>),
    Fn(Vec<Cow<'a, str>>, Ast<'a>),
    Prim(Prim),
    Hole,
}

#[derive(Debug, Clone)]
pub struct Value<'a> {
    val: Rc<RefCell<Val<'a>>>,
}

impl<'a> Value<'a> {
    pub fn new(val: Val) -> Value {
        Value {
            val: Rc::new(RefCell::new(val))
        }
    }

    pub fn val(&self) -> Ref<Val<'a>> {
        (*self.val).borrow()
    }

    pub fn fill(&self, val: Val<'a>) -> Result<(), ()> {
        let mut contents = (*self.val).borrow_mut();
        match *contents {
            Val::Hole => {
                *contents = val;
                Ok(())
            }
            _ => Err(())
        }
    }
}


#[derive(Debug, Clone)]
pub enum ControlFlow<'a> {
    Value(Value<'a>),
    Break(Value<'a>),
    Return(Value<'a>),
    Continue(Vec<Value<'a>>),
}

impl<'a> Display for Value<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self.val() {
            Val::Nil => {
                if fmt.alternate() {
                    write!(fmt, "'()")
                } else {
                    write!(fmt, "()")
                }
            }
            Val::Int(i) => write!(fmt, "{}", i),
            Val::Float(f) => write!(fmt, "{}", f),
            Val::Char(c) => write!(fmt, "#\\{}", c),
            Val::Str(ref s) => write!(fmt, "{:?}", s),
            Val::Sym(ref s) => {
                if fmt.alternate() {
                    write!(fmt, "'{}", s)
                } else {
                    write!(fmt, "{}", s)
                }
            }
            Val::Tuple(ref t, ref tag) => {
                if *tag == Some("cons".into()) {
                    if fmt.alternate() {
                        write!(fmt, "'({}", t[0])?;
                    } else {
                        write!(fmt, "({}", t[0])?;
                    }
                    let mut cdr = t[1].clone();
                    loop {
                        let cons = if let Val::Tuple(ref cons, ref tag) = *cdr.val() {
                            if *tag != Some("cons".into()) {
                                break;
                            }
                            cons.clone()
                        } else {
                            break;
                        };
                        write!(fmt, " {}", cons[0])?;
                        cdr = cons[1].clone();
                    }

                    return if *cdr.val() == Val::Nil {
                        write!(fmt, ")")
                    } else {
                        write!(fmt, " . {})", cdr)
                    };
                }
                match *tag {
                    Some(ref tag) => write!(fmt, "(#tuple<{}>", tag)?,
                    None => write!(fmt, "(#tuple")?,
                }
                for elem in t {
                    write!(fmt, " {}", elem)?;
                }
                write!(fmt, ")")
            }
            Val::Fn(..) => write!(fmt, "#fn<[anonymous]>"),
            Val::Prim(p) => write!(fmt, "#fn<[{:?}]>", p),
            Val::Hole => write!(fmt, "#hole"),
        }
    }
}

impl<'a> PartialEq for Value<'a> {
    fn eq(&self, other: &Self) -> bool {
        *self.val() == *other.val()
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
            (&Val::Tuple(ref a, ref tag_a), &Val::Tuple(ref b, ref tag_b)) =>
                tag_a == tag_b && a == b,
            (_, _) => false,
        }
    }
}

pub fn eval_literal<'a>(literal: &Sexp<'a>) -> Value<'a> {
    Value::new(match *literal {
        Sexp::Str(ref s, ..) => Val::Str(s.clone()),
        Sexp::Int(i, ..) => Val::Int(i),
        Sexp::Float(f, ..) => Val::Float(f),
        Sexp::Sym(ref s, ..) => Val::Sym(s.clone()),
        Sexp::Char(c, ..) => Val::Char(c),
        Sexp::List(ref v, ..) => {
            v.iter().rev().fold(Val::Nil, |cdr, car| {
                Val::Tuple(vec![eval_literal(car), Value::new(cdr)],
                           Some(Cow::Borrowed("cons")))
            })
        }
    })
}

pub fn type_of_term(term: &Value) -> Type {
    match *term.val() {
        Val::Nil => Type::Nil,
        Val::Int(..) => Type::Int,
        Val::Float(..) => Type::Float,
        Val::Char(..) => Type::Char,
        Val::Str(..) => Type::Str,
        Val::Sym(..) => Type::Sym,
        Val::Tuple(..) => Type::Tuple,
        Val::Fn(..) | Val::Prim(..) => Type::Fn,
        Val::Hole => Type::Hole,
    }
}

pub fn eval<'a>(ast: &Ast<'a>, env: &mut Rc<Env<'a>>) -> Result<Value<'a>, EvalError> {
    let (val, new_env) = eval_core(ast, env.clone())?;
    *env = new_env;
    match val {
        ControlFlow::Break(..) => Err(EvalError::BreakOutsideLoop),
        ControlFlow::Continue(..) => Err(EvalError::ContinueOutsideLoop),
        ControlFlow::Return(val) | ControlFlow::Value(val) => Ok(val),
    }
}

fn value(val: Val) -> ControlFlow {
    ControlFlow::Value(Value::new(val))
}

fn nil<'a>() -> ControlFlow<'a> {
    value(Val::Nil)
}

macro_rules! try_get_value {
    ($val:expr, $env:expr) => {
        if let ControlFlow::Value(val) = $val {
            val
        } else {
            return Ok(($val, $env));
        }
    }
}

macro_rules! chain_error {
    ($val:expr, $Err:expr, $span:expr) => {
        match $val {
            Ok(v) => v,
            Err(e) => return Err($Err(Box::new(e), $span).into()),
        }
    }
}

pub fn eval_core<'a>(ast: &Ast<'a>, env: Rc<Env<'a>>) -> Result<(ControlFlow<'a>, Rc<Env<'a>>), EvalError> {
    match *ast {
        Ast::Nil => {
            Ok((nil(), env))
        }
        Ast::Int(i) => {
            Ok((value(Val::Int(i)), env))
        }
        Ast::Float(f) => {
            Ok((value(Val::Float(f)), env))
        }
        Ast::Literal(ref val) => {
            Ok((ControlFlow::Value(eval_literal(val)), env))
        }
        Ast::Var(ref name, ..) => {
            env.lookup(name.borrow())
                .map(|x| (ControlFlow::Value(x.clone()), env.clone()))
                .ok_or_else(|| EvalError::NameLookup(name.clone().into_owned()))
        }
        Ast::Struct(ref name, ref members, span) => {
            let constructor = Ast::Call(
                Box::new(Ast::Var("tuple/tag:set".into(), (0, 0))),
                vec![
                    Ast::Call(
                        Box::new(Ast::Var("tuple".into(), (0, 0))),
                        members.iter().map(|member| {
                            Ast::Var(member.clone(), (0, 0))
                        }).collect(), span),
                    Ast::Literal(Cow::Owned(Sexp::Sym(name.clone(), (0, 0)))),
                ],
                span);
            let constructor = Value::new(Val::Fn(members.clone(), constructor));
            let accessors = members.iter().enumerate().map(|(i, member)| {
                Value::new(Val::Fn(vec![member.clone()],
                                   Ast::Call(Box::new(Ast::Var("tuple/nth".into(), (0, 0))), vec![
                                       Ast::Int(i as i64),
                                       Ast::Var(member.clone(), (0, 0)),
                                ], (0, 0))))
            });
            let mut env = (*env).clone();
            env.insert(format!("{}", name).into(), constructor.clone());
            for (member, accessor) in members.iter().zip(accessors) {
                env.insert(format!("{}/{}", name, member).into(), accessor);
            }
            Ok((ControlFlow::Value(constructor), Rc::new(env)))
        }
        Ast::Defn(ref name, ref args, ref code, span) => {
            let func = Value::new(Val::Fn(args.clone(),
                                          Ast::Begin(code.clone(), span)));
            let mut env = (*env).clone();
            env.insert(name.clone().into_owned(), func.clone());
            Ok((ControlFlow::Value(func), Rc::new(env)))
        }
        Ast::Call(ref func, ref args, span) => {
            let (func, _) = chain_error!(
                eval_core(func, env.clone()),
                EvalError::Call,
                span);
            let func = try_get_value!(func, env);
            let ref pat = *func.val();
            match *pat {
                Val::Fn(ref names, ref body) => {
                    if args.len() != names.len() {
                        return Err(EvalError::BadArity {
                            got: args.len(),
                            expected: names.len(),
                            at: Some(span),
                        });
                    }
                    let mut call_env = Env::with_parent(env.clone());
                    for (name, arg) in names.iter().zip(args) {
                        let (arg_val, _) = chain_error!(
                            eval_core(arg, env.clone()), EvalError::Call, span);
                        let arg_val = try_get_value!(arg_val, env);
                        call_env.insert(name.clone().into_owned(), arg_val);
                    }
                    Ok((ControlFlow::Value(eval(body, &mut Rc::new(call_env))?), env))
                }
                Val::Prim(ref prim) => {
                    let mut arg_values = Vec::new();
                    for arg in args {
                        let (arg_val, _) = chain_error!(
                            eval_core(arg, env.clone()), EvalError::Call, span);
                        let arg_val = try_get_value!(arg_val, env);
                        arg_values.push(arg_val);
                    }
                    Ok((ControlFlow::Value(prim.call(&arg_values)?), env))
                }
                _ => {
                    Err(EvalError::InvalidFunction(format!("{:#?}", func)))
                }
            }
        }
        Ast::Let(ref var, ref val, span) => {
            let name = match *var.clone() {
                Ast::Var(name, ..) => name.into(),
                ref other => {
                    return Err(EvalError::InvalidName(
                        format!("{:#?} at {:?}", other, span)
                    ));
                }
            };
            let (val, _) = chain_error!(
                eval_core(val, env.clone()), EvalError::Let, span);
            let val = try_get_value!(val, env);
            let mut env = (*env).clone();
            env.insert(name, val);
            Ok((nil(), Rc::new(env)))
        }
        Ast::If(ref cond, ref if_true, ref if_false, span) => {
            let (cond, _) = chain_error!(
                eval_core(cond, env.clone()), EvalError::If, span);
            let cond = try_get_value!(cond, env);
            if *cond.val() == Val::Nil {
                Ok((eval_core(if_false, env.clone())?.0, env))
            } else {
                Ok((eval_core(if_true, env.clone())?.0, env))
            }
        }
        Ast::Break(None, ..) => {
            Ok((ControlFlow::Break(Value::new(Val::Nil)), env))
        }
        Ast::Break(Some(ref val), span) => {
            let (val, _) = chain_error!(
                eval_core(&val, env.clone()), EvalError::Break, span);
            let val = try_get_value!(val, env);
            Ok((ControlFlow::Break(val), env))
        }
        Ast::Continue(ref args, span) => {
            let mut arg_values = Vec::new();
            for arg in args {
                let (arg_val, _) = chain_error!(
                    eval_core(arg, env.clone()), EvalError::Continue, span);
                let arg_val = try_get_value!(arg_val, env);
                arg_values.push(arg_val);
            }
            Ok((ControlFlow::Continue(arg_values), env))
        }
        Ast::Return(None, ..) => {
            Ok((ControlFlow::Return(Value::new(Val::Nil)), env))
        }
        Ast::Return(Some(ref val), span) => {
            let (val, _) = chain_error!(
                eval_core(&val, env.clone()), EvalError::Return, span);
            let val = try_get_value!(val, env);
            Ok((ControlFlow::Return(val), env))
        }
        Ast::Loop(ref body, ref bindings, span) => {
            let mut loop_default_env = Env::with_parent(env.clone());
            for &(ref var, ref val) in bindings {
                let (val, _) = chain_error!(
                    eval_core(&val, env.clone()), EvalError::Loop, span);
                let val = match val {
                    ControlFlow::Value(val) => val,
                    other => return Ok((other, env)),
                };
                loop_default_env.insert(var.clone().into_owned(), val);
            }
            let mut loop_env = Rc::new(loop_default_env.clone());
            'outer: loop {
                let loop_top_env = loop_env.clone();
                for expr in body {
                    let (val, new_env) = chain_error!(
                        eval_core(expr, loop_env), EvalError::Loop, span);
                    loop_env = new_env;
                    match val {
                        ControlFlow::Break(ref val) => {
                            return Ok((ControlFlow::Value(val.clone()), env));
                        }
                        ControlFlow::Continue(ref new_val) => {
                            for (&(ref var, _), val) in bindings.iter().zip(new_val) {
                                loop_default_env.insert(var.clone().into_owned(), val.clone());
                            }
                            loop_env = Rc::new(loop_default_env.clone());
                            continue 'outer;
                        }
                        ControlFlow::Return(ref val) => {
                            return Ok((ControlFlow::Value(val.clone()), env));
                        }
                        ControlFlow::Value(..) => (),
                    }
                }
                loop_env = loop_top_env;
            }
        }
        Ast::Begin(ref body, span) => {
            let mut last_val = Value::new(Val::Nil);
            let mut block_env = env.clone();
            for expr in body {
                let (new_val, new_env) = chain_error!(
                    eval_core(expr, block_env), EvalError::Begin, span);
                block_env = new_env;
                match new_val {
                    ControlFlow::Break(..) | ControlFlow::Continue(..) |
                    ControlFlow::Return(..) => {
                        return Ok((new_val, env));
                    }
                    ControlFlow::Value(ref val) => {
                        last_val = val.clone();
                    }
                }
            }
            Ok((ControlFlow::Value(last_val), env))
        }
    }
}
