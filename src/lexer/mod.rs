use crate::text::*;
use std::iter::Peekable;
use std::ops::Range;

pub mod errors;
pub mod token;
pub mod traits;
pub use errors::*;
pub use token::*;
pub use traits::*;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum LexerState {
    // Initial,
    MidIdentifier,
    Integer,
    LeadingMinus,
    // Decimal,
    InvalidIdent,
    InvalidInteger,
}

#[derive(Debug)]
pub struct Lexer<I: Iterator<Item = char>> {
    loc: CharLocation,
    chars: Peekable<I>,
}

impl<I: Iterator<Item = char>> Lexer<I> {
    pub fn new(iter: I) -> Self {
        Lexer {
            loc: CharLocation::new(1, 1),
            chars: iter.peekable(),
        }
    }
}

impl<T> Lexable<T> for T
where
    T: Iterator<Item = char>,
{
    fn lex(self) -> Lexer<T> {
        Lexer::new(self)
    }
}

impl<I: Iterator<Item = char>> Lexer<I> {
    fn advance_char(&mut self) -> Option<char> {
        match self.chars.next() {
            Some(c) => {
                if c == '\n' {
                    self.loc.line += 1;
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

impl Token {
    fn integer(text: String, loc: Range<CharLocation>) -> Result<Token, Error> {
        match text.parse::<i32>() {
            Ok(val) => Ok(Token {
                text: TokenText::Integer(val),
                loc: loc,
            }),
            Err(_) => Err(Error {
                loc: loc,
                error_type: ErrorType::InvalidInteger,
                text: text,
            }),
        }
    }

    fn i32(val: i32, loc: Range<CharLocation>) -> Token {
        Token {
            text: TokenText::Integer(val),
            loc: loc,
        }
    }

    fn identifier(text: String, loc: Range<CharLocation>) -> Token {
        Token {
            text: TokenText::Identifier(text),
            loc: loc,
        }
    }

    fn rparen(loc: CharLocation) -> Token {
        Token {
            text: TokenText::RightParen,
            loc: loc.into(),
        }
    }

    fn lparen(loc: CharLocation) -> Token {
        Token {
            text: TokenText::LeftParen,
            loc: loc.into(),
        }
    }
}

impl<I: Iterator<Item = char>> Lexer<I> {
    fn integer_token(&self, text: String, start_loc: CharLocation) -> Result<Token, Error> {
        Token::integer(text, start_loc..self.loc)
    }

    fn identifier_token(&self, text: String, start_loc: CharLocation) -> Token {
        Token::identifier(text, start_loc..self.loc)
    }

    fn invalid_integer(&self, text: String, start_loc: CharLocation) -> Error {
        Error::invalid_integer(text, start_loc..self.loc)
    }

    fn invalid_ident(&self, text: String, loc: CharLocation) -> Error {
        Error::invalid_ident(text, loc..self.loc)
    }
}

impl<I: Iterator<Item = char>> Iterator for Lexer<I> {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Result<Token, Error>> {
        while self.chars.peek().filter(|&&c| c.is_whitespace()).is_some() {
            self.advance_char();
        }
        let initial_loc = self.loc;
        match self.advance_char() {
            None => None,
            Some('(') => Some(Ok(Token::lparen(initial_loc))),
            Some(')') => Some(Ok(Token::rparen(initial_loc))),
            Some(c) => {
                let mut token_text = String::new();
                let mut state = match c {
                    c if c.is_digit(10) => LexerState::Integer,
                    c if c == '-' => LexerState::LeadingMinus,
                    c if is_identifier_char(c) => LexerState::MidIdentifier,
                    _ => LexerState::InvalidIdent,
                };
                token_text.push(c);
                loop {
                    match state {
                        LexerState::MidIdentifier => {
                            if None == self.chars.peek().filter(|&&c| is_identifier_char(c)) {
                                return Some(Ok(self.identifier_token(token_text, initial_loc)));
                            }
                            token_text.push(self.advance_char().unwrap())
                        }
                        LexerState::LeadingMinus => match self.chars.peek() {
                            None => {
                                return Some(Ok(self.identifier_token(token_text, initial_loc)))
                            }
                            Some(c) if c.is_whitespace() || *c == '(' || *c == ')' => {
                                // this refers to the special builtin -
                                return Some(Ok(self.identifier_token(token_text, initial_loc)));
                            }
                            Some(c) => {
                                state = if c.is_digit(10) {
                                    LexerState::Integer
                                } else if is_identifier_char(*c) {
                                    LexerState::MidIdentifier
                                } else {
                                    LexerState::InvalidIdent
                                };
                                token_text.push(self.advance_char().unwrap());
                            }
                        },
                        LexerState::Integer => match self.chars.peek() {
                            Some(c) if c.is_digit(10) => {
                                token_text.push(self.advance_char().unwrap())
                            }
                            None => return Some(self.integer_token(token_text, initial_loc)),
                            Some(c) if c.is_whitespace() || *c == '(' || *c == ')' => {
                                return Some(self.integer_token(token_text, initial_loc))
                            }
                            Some(_) => {
                                token_text.push(self.advance_char().unwrap());
                                state = LexerState::InvalidInteger
                            }
                        },
                        LexerState::InvalidIdent | LexerState::InvalidInteger => {
                            match self.chars.peek() {
                                Some(c) if (!c.is_whitespace() && *c != '(' && *c != ')') => {
                                    token_text.push(self.advance_char().unwrap())
                                }
                                _ => {
                                    return Some(Err(if state == LexerState::InvalidIdent {
                                        self.invalid_ident(token_text, initial_loc)
                                    } else {
                                        self.invalid_integer(token_text, initial_loc)
                                    }))
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

mod test {
    use super::*;

    fn assert_lexes_to(text: &str, expected_tokens: Vec<Result<Token, Error>>) {
        let actual_tokens: Vec<_> = text.chars().lex().collect();
        assert_eq!(expected_tokens, actual_tokens);
    }

    #[test]
    fn test_empty_string_lexes_to_none() {
        assert_lexes_to("", vec![]);
    }

    #[test]
    fn test_empty_tuple_lexes_to_left_paren_then_right_paren() {
        assert_lexes_to(
            "()",
            vec![
                Ok(Token::lparen(CharLocation::new(1, 1))),
                Ok(Token::rparen(CharLocation::new(1, 2))),
            ],
        )
    }

    #[test]
    fn test_single_value_list_lexes_to_three_tokens() {
        assert_lexes_to(
            "(a)",
            vec![
                Ok(Token::lparen(CharLocation::new(1, 1))),
                Ok(Token::identifier(
                    "a".to_string(),
                    CharLocation::new(1, 2).into(),
                )),
                Ok(Token::rparen(CharLocation::new(1, 3))),
            ],
        );
    }

    #[test]
    fn test_skips_over_spaces() {
        assert_lexes_to(
            " ( a  ) ",
            vec![
                Ok(Token::lparen(CharLocation::new(1, 2))),
                Ok(Token::identifier(
                    "a".to_string(),
                    CharLocation::new(1, 4).into(),
                )),
                Ok(Token::rparen(CharLocation::new(1, 7))),
            ],
        );
    }

    #[test]
    fn test_newline_advances_to_new_row() {
        assert_lexes_to(
            "(a\n)",
            vec![
                Ok(Token::lparen(CharLocation::new(1, 1))),
                Ok(Token::identifier(
                    "a".to_string(),
                    CharLocation::new(1, 2).into(),
                )),
                Ok(Token::rparen(CharLocation::new(2, 1))),
            ],
        );
    }

    #[test]
    fn test_crlf_advances_to_new_row() {
        assert_lexes_to(
            "(a\r\n)",
            vec![
                Ok(Token::lparen(CharLocation::new(1, 1))),
                Ok(Token::identifier(
                    "a".to_string(),
                    CharLocation::new(1, 2).into(),
                )),
                Ok(Token::rparen(CharLocation::new(2, 1))),
            ],
        );
    }

    #[test]
    fn test_integer_literal_followed_by_eof() {
        assert_lexes_to(
            "123",
            vec![Ok(Token {
                text: TokenText::Integer(123),
                loc: (1, 1).into()..(1, 4).into(),
            })],
        );
    }

    #[test]
    fn test_integer_literal_followed_by_space() {
        assert_lexes_to(
            " 123 ",
            vec![Ok(Token {
                text: TokenText::Integer(123),
                loc: (1, 2).into()..(1, 5).into(),
            })],
        );
    }

    #[test]
    fn test_integer_literal_followed_by_paren() {
        assert_lexes_to(
            "(123)",
            vec![
                Ok(Token::lparen(CharLocation::new(1, 1))),
                Ok(Token::i32(123, (1, 2).into()..(1, 5).into())),
                Ok(Token::rparen(CharLocation::new(1, 5))),
            ],
        );
    }

    #[test]
    fn test_int_arithmetic() {
        assert_lexes_to(
            "(+ 1 (- 2 3))",
            vec![
                Ok(Token::lparen(CharLocation::new(1, 1))),
                Ok(Token::identifier(
                    "+".to_string(),
                    CharLocation::new(1, 2).into(),
                )),
                Ok(Token::i32(1, CharLocation::new(1, 4).into())),
                Ok(Token::lparen(CharLocation::new(1, 6))),
                Ok(Token::identifier(
                    "-".to_string(),
                    CharLocation::new(1, 7).into(),
                )),
                Ok(Token::i32(2, CharLocation::new(1, 9).into())),
                Ok(Token::i32(3, CharLocation::new(1, 11).into())),
                Ok(Token::rparen(CharLocation::new(1, 12))),
                Ok(Token::rparen(CharLocation::new(1, 13))),
            ],
        );
    }

    #[test]
    fn test_int_arithmetic_multiline() {
        assert_lexes_to(
            "(\n +\n 1\n (\n  -\n  2\n  3\n )\n)",
            vec![
                Ok(Token::lparen(CharLocation::new(1, 1))),
                Ok(Token::identifier(
                    "+".to_string(),
                    CharLocation::new(2, 2).into(),
                )),
                Ok(Token::i32(1, CharLocation::new(3, 2).into())),
                Ok(Token::lparen(CharLocation::new(4, 2))),
                Ok(Token::identifier(
                    "-".to_string(),
                    CharLocation::new(5, 3).into(),
                )),
                Ok(Token::i32(2, CharLocation::new(6, 3).into())),
                Ok(Token::i32(3, CharLocation::new(7, 3).into())),
                Ok(Token::rparen(CharLocation::new(8, 2))),
                Ok(Token::rparen(CharLocation::new(9, 1))),
            ],
        );
    }

    #[test]
    fn test_negative_int() {
        let text = "(+ 1 -1)";
        let actual_tokens: Vec<_> = Lexer::new(text.chars()).collect();
        let expected_tokens: Vec<_> = vec![
            Ok(Token::lparen(CharLocation::new(1, 1))),
            Ok(Token::identifier(
                "+".to_string(),
                CharLocation::new(1, 2).into(),
            )),
            Ok(Token::i32(1, CharLocation::new(1, 4).into())),
            Ok(Token::i32(-1, (1, 6).into()..(1, 8).into())),
            Ok(Token::rparen(CharLocation::new(1, 8))),
        ];

        assert_eq!(expected_tokens, actual_tokens);
    }

    #[test]
    fn test_invalid_int() {
        assert_lexes_to(
            "123abc",
            vec![Err(Error::invalid_integer(
                "123abc".to_string(),
                (1, 1).into()..(1, 7).into(),
            ))],
        );
    }

    #[test]
    fn test_invalid_int_excludes_trailing_spaces() {
        assert_lexes_to(
            " 123abc ",
            vec![Err(Error::invalid_integer(
                "123abc".to_string(),
                (1, 2).into()..(1, 8).into(),
            ))],
        );
    }
}
