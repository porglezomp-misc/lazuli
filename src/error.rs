use sexp2;
use std::io;

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Parse(sexp2::ParseError),
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::Io(err)
    }
}

impl From<sexp2::ParseError> for Error {
    fn from(err: sexp2::ParseError) -> Self{
        Error::Parse(err)
    }
}
