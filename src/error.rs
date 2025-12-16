use core::fmt;
use std::{io, result};

use crate::language::parse::ParseError;
use rustyline::error::ReadlineError;

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Readline(String),

    UnknownCommand(String),

    ParsingError {
        error: ParseError,
        line: usize,
        column: usize,
    },
}

pub type Result<T> = result::Result<T, Error>;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Io(err) => {
                write!(f, "I/O error: {}", err)
            }
            Error::Readline(err) => write!(f, "{}", err),
            Error::UnknownCommand(name) => write!(f, "Unknown command '{}'", name),
            Error::ParsingError {
                error,
                line,
                column,
            } => write!(f, "error: {:?}\n{}:{}", error, line, column),
        }
    }
}

impl From<ReadlineError> for Error {
    fn from(err: ReadlineError) -> Self {
        Error::Readline(err.to_string())
    }
}
impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::Io(err)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_conversions() {
        //let err = Error::Readline(ReadlineError::Interrupted);
    }
}
