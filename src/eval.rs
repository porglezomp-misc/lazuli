use ess::Sexp;
use std::borrow::{Borrow, Cow};

use std::rc::Rc;
use ast::Ast;
use env::Env;

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
    UnknownArithmetic(String),
    UnknownRelation(String),
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
pub struct Var<'a>(Rc<Val<'a>>);

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
    // TODO: Move these up a layer into a higher type so that they can't be confused with values
    Break(Rc<Val<'a>>),
    Return(Rc<Val<'a>>),
    Continue,
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
        Val::Fn(..) => Type::Fn,
        Val::Break(..) | Val::Continue | Val::Return(..) => unreachable!(),
    }
}

fn unwrap_as_float<'a>(val: &Rc<Val<'a>>) -> f64 {
    match **val {
        Val::Int(i) => i as f64,
        Val::Float(f) => f,
        _ => panic!("Only valid for Val::Int or Val::Float, got {:?}", val),
    }
}

fn unwrap_as_int<'a>(val: &Rc<Val<'a>>) -> i64 {
    match **val {
        Val::Int(i) => i,
        Val::Float(f) => f as i64,
        _ => panic!("Only valid for Val::Int or Val::Float, got {:?}", val),
    }
}

fn eval_arithmetic<'a>(op: &str, args: &Vec<Rc<Val<'a>>>) -> Result<Rc<Val<'a>>, EvalError> {
    let mut use_float = false;
    for expr in args {
        let ty = type_of_term(expr);
        if ty == Type::Float { use_float = true; }
        if ty != Type::Float && ty != Type::Int {
            return Err(EvalError::TypeError {
                expected: Type::Float,
                got: ty,
            });
        }
    }

    Ok(Rc::new(match op {
        "+" if use_float => {
            Val::Float(args.iter().fold(0.0, |a, b| a + unwrap_as_float(b)))
        }
        "+" => {
            Val::Int(args.iter().fold(0, |a, b| a + unwrap_as_int(b)))
        }
        "-" if use_float => {
            if args.len() == 0 {
                Val::Float(0.0)
            } else if args.len() == 1 {
                Val::Float(-unwrap_as_float(&args[0]))
            } else {
                Val::Float(args[1..].iter().fold(
                    unwrap_as_float(&args[0]),
                    |a, b| a - unwrap_as_float(b)
                ))
            }
        }
        "-" => {
            if args.len() == 0 {
                Val::Int(0)
            } else if args.len() == 1 {
                Val::Int(-unwrap_as_int(&args[0]))
            } else {
                Val::Int(args[1..].iter().fold(
                    unwrap_as_int(&args[0]),
                    |a, b| a - unwrap_as_int(b)
                ))
            }
        }
        "*" if use_float => {
            Val::Float(args.iter().fold(1.0, |a, b| a * unwrap_as_float(b)))
        }
        "*" => {
            Val::Int(args.iter().fold(1, |a, b| a * unwrap_as_int(b)))
        }
        "/" => {
            if args.len() == 0 {
                Val::Float(1.0)
            } else if args.len() == 1 {
                Val::Float(1.0 / unwrap_as_float(&args[0]))
            } else {
                Val::Float(args[1..].iter().fold(
                    unwrap_as_float(&args[0]),
                    |a, b| a / unwrap_as_float(b)
                ))
            }
        }
        op => return Err(EvalError::UnknownArithmetic(op.into()))
    }))
}

fn eval_relations<'a>(op: &str, args: &Vec<Rc<Val<'a>>>) -> Result<Rc<Val<'a>>, EvalError> {
    let mut non_num_ty = None;
    for expr in args {
        let ty = type_of_term(expr);
        if ty != Type::Float && ty != Type::Int {
            non_num_ty = Some(ty);
            break;
        }
    }
    let success: bool;
    match op {
        "==" => {
            success = args.iter().zip(&args[1..]).all(|(a, b)| a == b);
        }
        "<=" => {
            if let Some(ty) = non_num_ty {
                return Err(EvalError::TypeError {
                    expected: Type::Float,
                    got: ty,
                });
            }
            success = args.iter().zip(&args[1..]).all(|(a, b)| unwrap_as_float(a) <= unwrap_as_float(b));
        }
        ">=" => {
            if let Some(ty) = non_num_ty {
                return Err(EvalError::TypeError {
                    expected: Type::Float,
                    got: ty,
                });
            }
            success = args.iter().zip(&args[1..]).all(|(a, b)| unwrap_as_float(a) >= unwrap_as_float(b));
        }
        "<" => {
            if let Some(ty) = non_num_ty {
                return Err(EvalError::TypeError {
                    expected: Type::Float,
                    got: ty,
                });
            }
            success = args.iter().zip(&args[1..]).all(|(a, b)| unwrap_as_float(a) < unwrap_as_float(b));
        }
        ">" => {
            if let Some(ty) = non_num_ty {
                return Err(EvalError::TypeError {
                    expected: Type::Float,
                    got: ty,
                });
            }
            success = args.iter().zip(&args[1..]).all(|(a, b)| unwrap_as_float(a) > unwrap_as_float(b));
        }
        "!=" => {
            let mut cond = true;
            'check: for (i, lhs) in args.iter().enumerate() {
                for (j, rhs) in args.iter().enumerate() {
                    if i == j { continue; }
                    if lhs == rhs {
                        cond = false;
                        break 'check;
                    }
                }
            }
            success = cond;
        }
        op => return Err(EvalError::UnknownRelation(op.into()))
    }
    if success {
        Ok(Rc::new(Val::Sym("t".into())))
    } else {
        Ok(Rc::new(Val::Nil))
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
            if let Ast::Var(ref s) = **func {
                if s == "tuple" {
                    if args.len() == 0 {
                        return Ok((Rc::new(Val::Nil), env));
                    }

                    let values = args.iter().map(|arg| {
                        eval(arg, env.clone()).map(|x| x.0)
                    }).collect::<Result<_, _>>()?;
                    return Ok((Rc::new(Val::Tuple(values)), env));
                } else if s == "tuple/nth" {
                    if args.len() != 2 {
                        return Err(EvalError::BadArity { expected: 2, got: args.len() });
                    }

                    let (idx, _) = eval(&args[0], env.clone())?;
                    let idx = match *idx {
                        Val::Int(idx) => idx as usize,
                        ref other => return Err(EvalError::TypeError {
                            expected: Type::Int,
                            got: type_of_term(other),
                        })
                    };
                    let (tup, _) = eval(&args[1], env.clone())?;
                    let tup = match *tup {
                        Val::Tuple(ref items) => items,
                        ref other => return Err(EvalError::TypeError{
                            expected: Type::Tuple,
                            got: type_of_term(other),
                        })
                    };

                    if tup.len() <= idx {
                        return Err(EvalError::OutOfBounds {
                            index: idx,
                            length: tup.len(),
                        })
                    }

                    return Ok((tup[idx].clone(), env))
                } else if s == "print" {
                    let mut last_val = Rc::new(Val::Nil);
                    for expr in args {
                        let (val, _) = eval(expr, env.clone())?;
                        print!("{:?} ", val);
                        last_val = val;
                    }
                    println!();
                    return Ok((last_val, env))
                } else if s == "+" || s == "-" || s == "*" || s == "/" {
                    let args = args.iter().map(|expr| {
                        eval(expr, env.clone()).map(|x| x.0)
                    }).collect::<Result<_, _>>()?;
                    return Ok((eval_arithmetic(s, &args)?, env));
                } else if s == "==" || s == "!=" || s == "<=" || s == ">=" || s == "<" || s == ">" {
                    let args = args.iter().map(|expr| {
                        eval(expr, env.clone()).map(|x| x.0)
                    }).collect::<Result<_, _>>()?;
                    return Ok((eval_relations(s, &args)?, env));
                }
            }

            let (func, _) = eval(func, env.clone())?;
            if let Val::Fn(ref names, ref body) = *func {
                if args.len() != names.len() {
                    return Err(EvalError::BadArity{ got: args.len(), expected: names.len() });
                }
                let mut call_env = Env::with_parent(env.clone());
                for (&name, arg) in names.iter().zip(args) {
                    let (arg_val, _) = eval(arg, env.clone())?;
                    call_env.insert(name.into(), Var(arg_val));
                }
                Ok((eval(body, Rc::new(call_env))?.0, env))
            } else {
                Err(EvalError::InvalidFunction(format!("{:#?}", func)))
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
