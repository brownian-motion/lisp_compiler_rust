use std::cmp::{Ord, Ordering};
use std::fmt;
use std::iter::Peekable;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd)]
struct TextLocation {
    row: u32,
    col: u32,
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

enum LexerState {
    Initial,
    MidIdentifier(Token),
}

#[derive(Eq, PartialEq)]
enum TokenText {
    LeftParen,
    RightParen,
    Identifier(String),
}

pub trait Locateable {
    fn get_location(&self) -> TextLocation;
}

impl fmt::Display for TokenText {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenText::LeftParen => "(",
                TokenText::RightParen => ")",
                TokenText::Identifier(text) => text,
            }
        )
    }
}

impl fmt::Debug for TokenText {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{:?}",
            match self {
                TokenText::LeftParen => "(",
                TokenText::RightParen => ")",
                TokenText::Identifier(text) => text,
            }
        )
    }
}

#[derive(Eq, PartialEq)]
struct Token {
    text: TokenText,
    loc: TextLocation,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{:?} at {:?}]", self.text, self.loc)
    }
}

struct Lexer<I: Iterator<Item = char>> {
    loc: TextLocation,
    chars: Peekable<I>,
}

impl<I: Iterator<Item = char>> Lexer<I> {
    pub fn new(iter: I) -> Self {
        Lexer {
            loc: TextLocation { row: 1, col: 1 },
            chars: iter.peekable(),
        }
    }
}

struct LexerError {
    message: &'static str,
    loc: TextLocation,
}

impl<I: Iterator<Item = char>> Lexer<I> {
    fn advance_char(&mut self) -> Option<char> {
        match self.chars.next() {
            Some(c) => {
                if c == '\n' {
                    self.loc.row += 1;
                    self.loc.col = 1;
                } else {
                    self.loc.col += 1;
                };
                Some(c)
            }
            None => None,
        }
    }
}

fn is_identifier_char(c: char) -> bool {
    !c.is_whitespace() && !c.is_control() && c != '(' && c != ')'
}

impl<I: Iterator<Item = char>> Iterator for Lexer<I> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        while self.chars.peek().filter(|&&c| c.is_whitespace()).is_some() {
            self.advance_char();
        }
        let initial_loc = self.loc;
        match self.advance_char() {
            None => None,
            Some('(') => Some(Token {
                text: TokenText::LeftParen,
                loc: initial_loc,
            }),
            Some(')') => Some(Token {
                text: TokenText::RightParen,
                loc: initial_loc,
            }),
            Some(c) => {
                let mut token_text = String::new();
                token_text.push(c);
                loop {
                    if None == self.chars.peek().filter(|&&c| is_identifier_char(c)) {
                        return Some(Token {
                            text: TokenText::Identifier(token_text),
                            loc: initial_loc,
                        });
                    }
                    token_text.push(self.advance_char().unwrap())
                }
            }
        }
    }
}

mod test {
    use super::*;

    #[test]
    fn test_empty_string_lexes_to_none() {
        let text = "";
        let mut lexer = Lexer::new(text.chars());
        assert_eq!(None, lexer.next());
    }

    #[test]
    fn test_empty_tuple_lexes_to_left_paren_then_right_paren() {
        let text = "()";
        let mut lexer = Lexer::new(text.chars());
        assert_eq!(
            Some(Token {
                text: TokenText::LeftParen,
                loc: TextLocation { row: 1, col: 1 }
            }),
            lexer.next()
        );
        assert_eq!(
            Some(Token {
                text: TokenText::RightParen,
                loc: TextLocation { row: 1, col: 2 }
            }),
            lexer.next()
        );
        assert_eq!(None, lexer.next());
    }

    #[test]
    fn test_single_value_list_lexes_to_three_tokens() {
        let text = "(a)";
        let mut lexer = Lexer::new(text.chars());
        assert_eq!(
            Some(Token {
                text: TokenText::LeftParen,
                loc: TextLocation { row: 1, col: 1 }
            }),
            lexer.next()
        );
        assert_eq!(
            Some(Token {
                text: TokenText::Identifier("a".to_string()),
                loc: TextLocation { row: 1, col: 2 }
            }),
            lexer.next()
        );
        assert_eq!(
            Some(Token {
                text: TokenText::RightParen,
                loc: TextLocation { row: 1, col: 3 }
            }),
            lexer.next()
        );
        assert_eq!(None, lexer.next());
    }

    #[test]
    fn test_skips_over_spaces() {
        let text = " ( a  ) ";
        let mut lexer = Lexer::new(text.chars());
        assert_eq!(
            Some(Token {
                text: TokenText::LeftParen,
                loc: TextLocation { row: 1, col: 2 }
            }),
            lexer.next()
        );
        assert_eq!(
            Some(Token {
                text: TokenText::Identifier("a".to_string()),
                loc: TextLocation { row: 1, col: 4 }
            }),
            lexer.next()
        );
        assert_eq!(
            Some(Token {
                text: TokenText::RightParen,
                loc: TextLocation { row: 1, col: 7 }
            }),
            lexer.next()
        );
        assert_eq!(None, lexer.next());
    }

    #[test]
    fn test_newline_advances_to_new_row() {
        let text = "(a\n)";
        let mut lexer = Lexer::new(text.chars());
        assert_eq!(
            Some(Token {
                text: TokenText::LeftParen,
                loc: TextLocation { row: 1, col: 1 }
            }),
            lexer.next()
        );
        assert_eq!(
            Some(Token {
                text: TokenText::Identifier("a".to_string()),
                loc: TextLocation { row: 1, col: 2 }
            }),
            lexer.next()
        );
        assert_eq!(
            Some(Token {
                text: TokenText::RightParen,
                loc: TextLocation { row: 2, col: 1 }
            }),
            lexer.next()
        );
        assert_eq!(None, lexer.next());
    }
}
