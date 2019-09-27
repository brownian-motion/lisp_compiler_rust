use crate::text::*;

use std::fmt;
use std::ops::Range;

#[derive(Eq, PartialEq, Debug)]
pub enum TokenText {
    LeftParen,
    RightParen,
    Identifier(String),
    Integer(i32),
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
    pub loc: Range<CharLocation>,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{:?} at {:?}]", self.text, self.loc)
    }
}
