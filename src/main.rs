extern crate sexp2;

mod error;

use std::io::{self, Read, Write};
use error::Error;
use sexp2::Sexp;

#[derive(Debug)]
pub enum AstError {
    Unimplemented
}

#[derive(Debug)]
pub enum Ast {
}

fn make_ast(_sexp: Vec<Sexp>) -> Result<Vec<Ast>, AstError> {
    Err(AstError::Unimplemented)
}

fn run() -> Result<(), Error> {
    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf)?;

    let sexpressions = sexp2::parse(&buf)?;
    let ast = make_ast(sexpressions)?;

    println!("{:#?}", ast);

    Ok(())
}

fn main() {
    if let Err(e) = run() {
        let _ = writeln!(io::stderr(), "Error: {:?}", e);
    }
}
