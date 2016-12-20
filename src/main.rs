extern crate sexp2;

use std::io::Read;

fn main() {
    let mut buf = String::new();
    if let Err(e) = std::io::stdin().read_to_string(&mut buf) {
        println!("Error! {}", e);
        return;
    }

    match sexp2::parse(&buf) {
        Ok(expr) => println!("{:#?}", expr),
        Err(()) => println!("Error parsing."),
    }
}
