use crate::errors::*;
use crate::lexer;
use crate::text::*;
use std::fmt;
use std::ops::Range;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ErrorType {
    UnexpectedEof,
    UnmatchedRightParen,
    LexerError(lexer::ErrorType),
    UnexpectedStackState,
}

#[derive(PartialEq, Eq, Clone)]
pub struct Error {
    pub loc: Range<CharLocation>,
    pub error_type: ErrorType,
}

impl Locateable<Range<CharLocation>> for Error {
    fn location(&self) -> Range<CharLocation> {
        self.loc.clone()
    }
}

impl ErrorType {
    pub fn at(self, loc: Range<CharLocation>) -> Error {
        Error {
            loc,
            error_type: self,
        }
    }
}

impl From<lexer::ErrorType> for ErrorType {
    fn from(lexer_error_type: lexer::ErrorType) -> ErrorType {
        ErrorType::LexerError(lexer_error_type)
    }
}

impl From<lexer::Error> for Error {
    fn from(error: lexer::Error) -> Error {
        Error {
            loc: error.loc,
            error_type: error.error_type.into(),
        }
    }
}

impl CompilerErrorMessage for ErrorType {
    fn message(&self) -> String {
        use ErrorType::*;
        match self {
            UnexpectedEof => "Unexpected end of input (unmatched left parenthesis)".to_string(),
            UnmatchedRightParen => "Unmatched right parenthesis".to_string(),
            UnexpectedStackState => "Unexpected Stack state!!".to_string(), // TODO: add info ABOUT that stack state to the error message
            LexerError(lexer_error) => lexer_error.message(),
        }
    }
}

impl ErrorCode for ErrorType {
    fn error_code(&self) -> (&'static str, u32) {
        use ErrorType::*;
        match self {
            UnexpectedEof => ("P", 1),
            UnmatchedRightParen => ("P", 2),
            UnexpectedStackState => ("P", 3),
            LexerError(lexer_error) => lexer_error.error_code(),
        }
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

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", format_error(self))
    }
}
