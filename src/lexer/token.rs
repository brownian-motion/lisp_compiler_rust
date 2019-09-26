use std::cmp::{Ord, Ordering};
use std::fmt;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd)]
pub struct TextLocation {
    pub row: u32,
    pub col: u32,
}

impl fmt::Display for TextLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(l{}, c{})", self.row, self.col)
    }
}

impl fmt::Debug for TextLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(l{:?}, c{:?})", self.row, self.col)
    }
}

impl Ord for TextLocation {
    fn cmp(&self, other: &Self) -> Ordering {
        self.row.cmp(&other.row).then(self.col.cmp(&other.col))
    }
}

#[derive(Eq, PartialEq, Debug)]
pub enum TokenText {
    LeftParen,
    RightParen,
    Identifier(String),
    Integer(i32),
}

pub trait Locateable {
    fn get_location(&self) -> TextLocation;
}

impl fmt::Display for TokenText {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenText::LeftParen => write!(f, "("),
            TokenText::RightParen => write!(f, ")"),
            TokenText::Identifier(text) => write!(f, "{}", text),
            TokenText::Integer(val) => write!(f, "{}", val),
        }
    }
}

// impl fmt::Debug for TokenText {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match self {
//             TokenText::LeftParen => write!(f, "("),
//             TokenText::RightParen => write!(f, ")"),
//             TokenText::Identifier(text) => write!(f, "{:?}", text),
//             TokenText::Integer(val) => write!(f, "{:?}", val),
//         }
//     }
// }

#[derive(Eq, PartialEq)]
pub struct Token {
    pub text: TokenText,
    pub loc: TextLocation,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{:?} at {:?}]", self.text, self.loc)
    }
}
