use crate::errors::*;
use crate::text::*;
use std::ops::Range;

// Because this is scoped by the module as lexer::ErrorType, this bare type name should be fine
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ErrorType {
    InvalidInteger,
    InvalidIdent,
}

impl CompilerErrorMessage for ErrorType {
    fn message(&self) -> &'static str {
        match self {
            ErrorType::InvalidInteger => "Could not parse malformed integer token",
            ErrorType::InvalidIdent => "Unrecognized or invalid identifier",
        }
    }
}

// Because this is scoped by the module as lexer::Error, this bare type name should be fine
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Error {
    pub loc: Range<CharLocation>,
    pub error_type: ErrorType,
    pub text: String,
}

impl Locateable<Range<CharLocation>> for Error {
    fn location(&self) -> Range<CharLocation> {
        self.loc.clone()
    }
}

impl CompilerErrorMessage for Error {
    fn message(&self) -> &'static str {
        self.error_type.message()
    }
}

impl Error {
    pub fn invalid_integer(text: String, loc: Range<CharLocation>) -> Error {
        Error {
            loc: loc,
            error_type: ErrorType::InvalidInteger,
            text,
        }
    }

    pub fn invalid_ident(text: String, loc: Range<CharLocation>) -> Error {
        Error {
            loc: loc,
            error_type: ErrorType::InvalidIdent,
            text,
        }
    }
}
