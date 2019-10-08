use std::cmp::{Ord, Ordering};
use std::fmt;
use std::ops::Range;

// Represents the location of a single char in a file, starting from line 1 col 1.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd)]
pub struct CharLocation {
    pub line: u32, // starts at 1
    pub col: u32,  // starts at 1
}

impl fmt::Display for CharLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(l{}, c{})", self.line, self.col)
    }
}

impl fmt::Debug for CharLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(l{:?}, c{:?})", self.line, self.col)
    }
}

impl Ord for CharLocation {
    fn cmp(&self, other: &Self) -> Ordering {
        self.line.cmp(&other.line).then(self.col.cmp(&other.col))
    }
}

impl CharLocation {
    pub fn new(line: u32, col: u32) -> Self {
        CharLocation { line, col }
    }
}

pub trait Locateable<T> {
    fn location(&self) -> T;
}

impl Into<Range<CharLocation>> for CharLocation {
    fn into(self) -> Range<CharLocation> {
        self..CharLocation {
            line: self.line,
            col: self.col + 1,
        }
    }
}

impl From<(u32, u32)> for CharLocation {
    fn from(loc: (u32, u32)) -> CharLocation {
        CharLocation::new(loc.0, loc.1)
    }
}

pub trait Locater<T, L> {
    fn locate(&self, obj: T) -> L;
}
