extern crate ess;

mod ast;
mod error;
mod env;
mod eval;

use std::io::{self, Read, Write};
use std::rc::Rc;

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

    let mut env = Rc::new(env::Env::new());
    for item in &ast {
        let (val, new_env) = eval::eval(item, env)?;
        env = new_env;
        println!("{:#?}", val);
    }

    Ok(())
}

fn main() {
    if let Err(e) = run() {
        let _ = writeln!(io::stderr(), "Error: {:?}", e);
    }
}
