use crate::text::*;
use std::fmt;
use std::fmt::Debug;

use std::ops::Range;

pub trait CompilerErrorMessage {
    fn message(&self) -> String;
}

pub trait ErrorCode {
    fn error_code(&self) -> (&'static str, u32);
}

pub trait CompilerError:
    Locateable<Range<CharLocation>> + CompilerErrorMessage + ErrorCode
{
}

pub fn format_error<E: CompilerError>(error: &E) -> String {
    let (code_type, code_num) = error.error_code();
    format!(
        "Error {}{} at {:?}: {:?}",
        code_type,
        code_num,
        error.location(),
        error.message()
    )
}
