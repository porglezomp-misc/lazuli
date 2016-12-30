use ess;
use std::io;
use ast::AstError;


#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Parse(ess::ParseError),
    Ast(AstError<'static>),
}


impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::Io(err)
    }
}

impl From<ess::ParseError> for Error {
    fn from(err: ess::ParseError) -> Self {
        Error::Parse(err)
    }
}

impl<'a> From<AstError<'a>> for Error {
    fn from(err: AstError) -> Error {
        Error::Ast(err.to_owned())
    }
}
