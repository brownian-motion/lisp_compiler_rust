use crate::errors::*;
use crate::text::*;
use std::fmt;
use std::ops::Range;

// Because this is scoped by the module as lexer::ErrorType, this bare type name should be fine
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ErrorType {
    InvalidInteger,
    InvalidIdent,
}

impl CompilerErrorMessage for ErrorType {
    fn message(&self) -> String {
        match self {
            ErrorType::InvalidInteger => "Could not parse malformed integer token",
            ErrorType::InvalidIdent => "Unrecognized or invalid identifier",
        }
        .to_string()
    }
}

impl ErrorCode for ErrorType {
    fn error_code(&self) -> (&'static str, u32) {
        match self {
            ErrorType::InvalidInteger => ("L", 1),
            ErrorType::InvalidIdent => ("L", 2),
        }
    }
}

// Because this is scoped by the module as lexer::Error, this bare type name should be fine
#[derive(Eq, PartialEq, Clone)]
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
    fn message(&self) -> String {
        self.error_type.message()
    }
}

impl ErrorCode for Error {
    fn error_code(&self) -> (&'static str, u32) {
        self.error_type.error_code()
    }
}

impl CompilerError for Error {}

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

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", format_error(self))
    }
}
