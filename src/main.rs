extern crate sexp2;

mod ast;
mod error;

use std::io::{self, Read, Write};
use error::Error;

fn run<'a>() -> Result<(), Error> {
    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf)?;

    let sexpressions = sexp2::parse(&buf)?;
    let ast = sexpressions.iter()
        .map(ast::make_ast)
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
