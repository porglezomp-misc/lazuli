extern crate ess;

mod ast;
mod error;
mod env;
mod eval;
mod prims;

use std::io::{self, Read, Write};
use std::rc::Rc;

use error::Error;

fn run() -> Result<(), Error> {
    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf)?;

    let (exprs, err) = ess::parse(&buf);
    if let Some(err) = err {
        return Err(err.into());
    }

    let mut ast = Vec::new();
    for item in &exprs {
        ast.push(ast::make_ast(item)?.to_owned());
    }

    let (prelude, err) = ess::parse(include_str!("prelude.laz"));
    if let Some(err) = err {
        println!("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
        println!("!!! AN ERROR OCCURRED WHILE LOADING THE PRELUDE !!!");
        println!("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
        return Err(err.into());
    }

    let mut prelude_ast = Vec::new();
    for item in &prelude {
        prelude_ast.push(ast::make_ast(item)?.to_owned());
    }

    let mut env = Rc::new(env::Env::with_primitives());
    for item in &prelude_ast {
        let (_, new_env) = eval::eval(item, env)?;
        env = new_env;
    }

    for item in &ast {
        let (_, new_env) = eval::eval(item, env)?;
        env = new_env;
    }

    Ok(())
}

fn main() {
    if let Err(e) = run() {
        let _ = writeln!(io::stderr(), "Error: {:?}", e);
    }
}
