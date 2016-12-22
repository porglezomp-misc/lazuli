use sexp2;
use std::io;
use super::AstError;


#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Parse(sexp2::ParseError),
    Ast(AstError),
}


impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::Io(err)
    }
}

impl From<sexp2::ParseError> for Error {
    fn from(err: sexp2::ParseError) -> Self {
        Error::Parse(err)
    }
}

impl From<AstError> for Error {
    fn from(err: AstError) -> Self {
        Error::Ast(err)
    }
}
