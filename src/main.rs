extern crate ess;

mod ast;
mod error;
mod env;
mod eval;

use std::io::{self, Read, Write};
use error::Error;

fn run() -> Result<(), Error> {
    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf)?;

    let (sexpressions, err) = ess::parse(&buf);
    if let Some(err) = err {
        return Err(err.into());
    }

    let ast = sexpressions.iter()
        .map(ast::make_ast)
        .collect::<Result<Vec<_>, _>>()?;

    // let mut env = ast::Env::new();
    // let mut new_ast = Vec::with_capacity(ast.len());
    // for node in &ast {
    //     let new_node = ast::resolve_names(&env, node)?;
    //     new_ast.push(new_node);
    // }

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
