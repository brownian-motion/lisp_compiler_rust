use crate::errors::*;
use crate::lexer;
use crate::text::*;
use std::ops::Range;

#[derive(Debug, PartialEq, Eq)]
pub enum ErrorType {
    UnexpectedEof,
    UnmatchedRightParen,
    LexerError(lexer::ErrorType),
    UnexpectedStackState,
}

#[derive(Debug, PartialEq, Eq)]
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
    fn message(&self) -> &'static str {
        use ErrorType::*;
        match self {
            UnexpectedEof => "Unexpected end of input (unmatched left parenthesis)",
            UnmatchedRightParen => "Unmatched right parenthesis",
            EmptySExpr => {
                "Empty S-sxpression (must have values between left and right parenthesis)"
            }
            UnexpectedStackState => "Unexpected Stack state!!",
            LexerError(lexer_error) => lexer_error.message(),
        }
    }
}

impl CompilerErrorMessage for Error {
    fn message(&self) -> &'static str {
        self.error_type.message()
    }
}
