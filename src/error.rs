use std::io;

use ess::parser::ParseError;

use ast::AstError;
use eval::EvalError;


#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Parse(ParseError),
    Ast(AstError<'static>),
    Eval(EvalError),
}


impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::Io(err)
    }
}

impl From<ParseError> for Error {
    fn from(err: ParseError) -> Self {
        Error::Parse(err)
    }
}

impl<'a> From<AstError<'a>> for Error {
    fn from(err: AstError) -> Error {
        Error::Ast(err.to_owned())
    }
}

impl From<EvalError> for Error {
    fn from(err: EvalError) -> Error {
        Error::Eval(err)
    }
}
