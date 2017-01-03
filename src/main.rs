extern crate ess;
extern crate fnv;

mod ast;
mod error;
mod env;
mod eval;
mod prims;

use ess::parser::ParseError;
use std::io::{self, Read, Write};
use std::rc::Rc;
use std::env::{Args, args};

use error::Error;

fn is_eof_error(err: &ParseError) -> bool {
    match *err {
        ParseError::String(ref e, ..) => **e == ParseError::UnexpectedEof,
        ParseError::List(ref e, ..) => {
            **e == ParseError::UnexpectedEof || is_eof_error(&**e)
        }
        _ => false
    }
}

fn repl() -> Result<(), Error> {
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
        eval::eval(item, &mut env)?;
    }

    let mut buffer = String::new();
    let mut should_continue = false;
    loop {
        if !should_continue {
            buffer.clear();
            print!(">>> ");
        } else {
            print!("    ");
        }
        std::io::stdout().flush()?;
        if std::io::stdin().read_line(&mut buffer)? == 0 {
            return Ok(());
        }
        should_continue = false;
        if buffer.trim() == "" { continue; }
        let buf = buffer.clone();
        let (exprs, err) = ess::parse(&buf);
        if let Some(error) = err {
            if is_eof_error(&error) {
                should_continue = true;
                continue;
            } else {
                println!("Parse Error: {:?}", error);
                buffer.clear();
                continue;
            }
        }

        for expr in exprs {
            let ast = ast::make_ast(&expr)?.to_owned();
            let val = match eval::eval(&ast, &mut env) {
                Ok(val) => val,
                Err(e) => {
                    println!("Eval Error: {:?}", e);
                    break;
                }
            };
            println!("{}", val);
        }
    }
}

fn run(mut args: Args) -> Result<(), Error> {
    if let Some(fname) = args.nth(1) {
        let mut buf = String::new();
        let mut file = std::fs::File::open(fname)?;
        file.read_to_string(&mut buf)?;

        let (items, err) = ess::parse(&buf);
        if let Some(err) = err {
            return Err(err.into());
        }

        let mut ast = Vec::new();
        for item in &items {
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
            eval::eval(item, &mut env)?;
        }

        for item in &ast {
            if let Err(e) = eval::eval(item, &mut env) {
                let mut e = Some(&e);
                while let Some(err) = e {
                    if let Some((start, end)) = err.get_span() {
                        println!("ERROR:\n{}", &buf[start..end])
                    } else {
                        println!("ERROR:\n{:?}", err);
                    }
                    e = err.get_cause();
                }
            }
        }

        Ok(())
    } else {
        repl()
    }
}


fn main() {
    if let Err(e) = run(args()) {
        let _ = writeln!(io::stderr(), "Error: {:?}", e);
    }
}
