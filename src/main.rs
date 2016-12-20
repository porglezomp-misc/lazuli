extern crate sexp2;

use std::io::{self, Read, Write};

mod error;
use error::Error;

fn run() -> Result<(), Error> {
    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf)?;

    let expr = sexp2::parse(&buf)?;
    println!("{:#?}", expr);

    Ok(())
}

fn main() {
    if let Err(e) = run() {
        let _ = writeln!(io::stderr(), "Error: {:?}", e);
    }
}
