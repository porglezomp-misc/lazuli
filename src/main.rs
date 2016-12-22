extern crate sexp2;

mod error;

use std::io::{self, Read, Write};
use error::Error;
use sexp2::Sexp;

#[derive(Debug)]
pub enum AstError {
    Unimplemented,
    InvalidInCall(Sexp),
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

fn make_ast(sexp: &Sexp) -> Result<Ast, AstError> {
    match *sexp {
        Sexp::List(ref xs) => match xs.first() {
            Some(x) => match *x {
                Sexp::Sym(ref s) => {
                    let func = Ast::Var(s);
                    let args = xs[1..].iter().map(make_ast).collect::<Result<_, _>>()?;
                    Ok(Ast::Call(Box::new(func), args))
                }
                ref s@Sexp::List(_) => {
                    let func = make_ast(s)?;
                    let args = xs[1..].iter().map(make_ast).collect::<Result<_, _>>()?;
                    Ok(Ast::Call(Box::new(func), args))
                }
                ref s => Err(AstError::InvalidInCall(s.clone()))
            },
            None => Ok(Ast::Nil),
        },
        Sexp::Sym(ref s) => Ok(Ast::Var(s)),
        ref s@Sexp::Str(_) => Ok(Ast::Literal(s)),
        ref s@Sexp::Char(_) => Ok(Ast::Literal(s)),
        ref s@Sexp::Int(_) => Ok(Ast::Literal(s)),
        ref s@Sexp::Float(_) => Ok(Ast::Literal(s)),
    }
}

fn run<'a>() -> Result<(), Error> {
    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf)?;

    let sexpressions = sexp2::parse(&buf)?;
    let ast = sexpressions.iter().map(make_ast)
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
