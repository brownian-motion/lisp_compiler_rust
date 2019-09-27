use crate::text::*;
use std::ops::Range;

pub trait CompilerError: Locateable<Range<CharLocation>> {
    fn message(&self) -> &'static str;
}
