extern crate sexp2;

mod error;

use std::io::{self, Read, Write};
use error::Error;
use sexp2::Sexp;

#[derive(Debug)]
pub enum AstError {
    Unimplemented,
    ArgList(Sexp),
    ArgListNoName,
    ArgListName(Sexp),
    ArgListArg(Sexp),
    InvalidInCall(Sexp),
    StructName(Sexp),
    StructMember(Sexp),
    VarName(Sexp),
}

#[derive(Debug)]
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
    Literal(&'a Sexp),
    Nil,
}

fn defn_get_args(arg_list: &Sexp) -> Result<(&str, Vec<&str>), AstError> {
    match *arg_list {
        Sexp::List(ref xs) => {
            let name = match xs.first() {
                Some(&Sexp::Sym(ref s)) => Ok(s),
                Some(s) => Err(AstError::ArgListName(s.clone())),
                None => Err(AstError::ArgListNoName),
            }?;
            let args = xs[1..].iter().map(|arg| {
                match *arg {
                    Sexp::Sym(ref s) => Ok(&s[..]),
                    ref s => Err(AstError::ArgListArg(s.clone())),
                }
            }).collect::<Result<_, _>>()?;
            Ok((name, args))
        }
        ref s => Err(AstError::ArgList(s.clone())),
    }
}

fn make_ast(sexp: &Sexp) -> Result<Ast, AstError> {
    match *sexp {
        Sexp::List(ref xs) => match xs.first() {
            Some(x) => match *x {
                Sexp::Sym(ref s) if s == "if" => {
                    // TODO: Not too many args
                    // TODO: Optional second arg?
                    let cond = make_ast(&xs[1])?;
                    let if_true = make_ast(&xs[2])?;
                    let if_false = make_ast(&xs[3])?;
                    Ok(Ast::If(Box::new(cond), Box::new(if_true), Box::new(if_false)))
                }
                Sexp::Sym(ref s) if s == "let!" => {
                    // TODO: Not too many args
                    let var = make_ast(&xs[1])?;
                    let val = make_ast(&xs[2])?;
                    Ok(Ast::Let(Box::new(var), Box::new(val)))
                }
                Sexp::Sym(ref s) if s == "set!" => {
                    // TODO: Not too many args
                    let var = make_ast(&xs[1])?;
                    let val = make_ast(&xs[2])?;
                    Ok(Ast::Set(Box::new(var), Box::new(val)))
                }
                Sexp::Sym(ref s) if s == "begin" => {
                    let body = xs[1..].iter().map(make_ast)
                        .collect::<Result<_, _>>()?;
                    Ok(Ast::Begin(body))
                }
                Sexp::Sym(ref s) if s == "loop" => {
                    let body = xs[1..].iter().map(make_ast)
                        .collect::<Result<_, _>>()?;
                    Ok(Ast::Loop(body))
                }
                Sexp::Sym(ref s) if s == "return" => {
                    // TODO: Assert not too many args
                    let result = if let Some(res) = xs.get(1) {
                        Some(Box::new(make_ast(&res)?))
                    } else {
                        None
                    };
                    Ok(Ast::Return(result))
                }
                Sexp::Sym(ref s) if s == "break" => {
                    // TODO: Assert not too many args
                    let result = if let Some(res) = xs.get(1) {
                        Some(Box::new(make_ast(&res)?))
                    } else {
                        None
                    };
                    Ok(Ast::Break(result))
                }
                Sexp::Sym(ref s) if s == "continue" => {
                    // TODO: Assert not too many args
                    Ok(Ast::Continue)
                }
                Sexp::Sym(ref s) if s == "defn" => {
                    let (name, args) = defn_get_args(&xs[1])?;
                    let body = xs[2..].iter()
                        .map(make_ast)
                        .collect::<Result<_, _>>()?;
                    Ok(Ast::Defn(name, args, body))
                }
                Sexp::Sym(ref s) if s == "struct" => {
                    let name = match xs[1] {
                        Sexp::Sym(ref name) => Ok(name),
                        ref s => {
                            Err(AstError::StructName(s.clone()))
                        }
                    }?;
                    let members = xs[2..].iter().map(|x| {
                        match *x {
                            Sexp::Sym(ref name) => Ok(&name[..]),
                            ref s => Err(AstError::StructMember(s.clone())),
                        }
                    }).collect::<Result<_, _>>()?;
                    Ok(Ast::Struct(name, members))
                }
                Sexp::Sym(ref s) => {
                    let func = Ast::Var(s);
                    let args = xs[1..].iter()
                        .map(make_ast)
                        .collect::<Result<_, _>>()?;
                    Ok(Ast::Call(Box::new(func), args))
                }
                ref s @ Sexp::List(_) => {
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
        Sexp::Sym(ref s) => Ok(Ast::Var(s)),
        ref s @ Sexp::Str(_) => Ok(Ast::Literal(s)),
        ref s @ Sexp::Char(_) => Ok(Ast::Literal(s)),
        ref s @ Sexp::Int(_) => Ok(Ast::Literal(s)),
        ref s @ Sexp::Float(_) => Ok(Ast::Literal(s)),
    }
}

fn run<'a>() -> Result<(), Error> {
    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf)?;

    let sexpressions = sexp2::parse(&buf)?;
    let ast = sexpressions.iter()
        .map(make_ast)
        .collect::<Result<Vec<_>, _>>()?;

    for item in ast {
        println!("{:#?}", item);
    }

    Ok(())
}

fn main() {
    if let Err(e) = run() {
        let _ = writeln!(io::stderr(), "Error: {:?}", e);
    }
}
