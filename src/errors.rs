use crate::text::*;
use std::ops::Range;

trait CompilerError: Locateable<Range<CharLocation>> {
    fn location() -> Range<CharLocation>;
}
