use crate::text::*;
use std::fmt::Debug;
use std::ops::Range;

pub trait CompilerErrorMessage: Debug {
    fn message(&self) -> &'static str;
}

pub trait CompilerError: Locateable<Range<CharLocation>> + CompilerErrorMessage {}
