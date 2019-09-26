use std::iter::Peekable;

mod token;
use token::*;

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

#[derive(Debug, Eq, PartialEq, Clone)]
struct LexerError {
    message: &'static str,
    loc: TextLocation,
    state: LexerState,
    text: String,
}

impl LexerError {
    fn invalid_integer(text: String, loc: TextLocation) -> LexerError {
        LexerError {
            message: "Could not parse malformed integer token",
            loc: loc,
            state: LexerState::InvalidInteger,
            text: text,
        }
    }

    fn invalid_ident(text: String, loc: TextLocation) -> LexerError {
        LexerError {
            message: "Unrecognized or invalid token",
            loc: loc,
            state: LexerState::InvalidIdent,
            text: text,
        }
    }
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

impl Token {
    fn integer(text: String, loc: TextLocation) -> Result<Token, LexerError> {
        match text.parse::<i32>() {
            Ok(val) => Ok(Token {
                text: TokenText::Integer(val),
                loc: loc,
            }),
            Err(_) => Err(LexerError {
                message: "Could not parse malformed integer token",
                loc: loc,
                state: LexerState::InvalidInteger,
                text: text,
            }),
        }
    }

    fn identifier(text: String, loc: TextLocation) -> Token {
        Token {
            text: TokenText::Identifier(text),
            loc: loc,
        }
    }

    fn rparen(loc: TextLocation) -> Token {
        Token {
            text: TokenText::RightParen,
            loc: loc,
        }
    }

    fn lparen(loc: TextLocation) -> Token {
        Token {
            text: TokenText::LeftParen,
            loc: loc,
        }
    }
}

impl<I: Iterator<Item = char>> Iterator for Lexer<I> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Result<Token, LexerError>> {
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
                                return Some(Ok(Token::identifier(token_text, initial_loc)));
                            }
                            token_text.push(self.advance_char().unwrap())
                        }
                        LexerState::LeadingMinus => match self.chars.peek() {
                            None => return Some(Ok(Token::identifier(token_text, initial_loc))),
                            Some(c) if c.is_whitespace() || *c == '(' || *c == ')' => {
                                // this refers to the special builtin -
                                return Some(Ok(Token::identifier(token_text, initial_loc)));
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
                            None => return Some(Token::integer(token_text, initial_loc)),
                            Some(c) if c.is_whitespace() || *c == '(' || *c == ')' => {
                                return Some(Token::integer(token_text, initial_loc))
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
                                        LexerError::invalid_ident(token_text, initial_loc)
                                    } else {
                                        LexerError::invalid_integer(token_text, initial_loc)
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
            Some(Ok(Token::lparen(TextLocation { row: 1, col: 1 }))),
            lexer.next()
        );
        assert_eq!(
            Some(Ok(Token::rparen(TextLocation { row: 1, col: 2 }))),
            lexer.next()
        );
        assert_eq!(None, lexer.next());
    }

    #[test]
    fn test_single_value_list_lexes_to_three_tokens() {
        let text = "(a)";
        let mut lexer = Lexer::new(text.chars());
        assert_eq!(
            Some(Ok(Token::lparen(TextLocation { row: 1, col: 1 }))),
            lexer.next()
        );
        assert_eq!(
            Some(Ok(Token::identifier(
                "a".to_string(),
                TextLocation { row: 1, col: 2 }
            ))),
            lexer.next()
        );
        assert_eq!(
            Some(Ok(Token::rparen(TextLocation { row: 1, col: 3 }))),
            lexer.next()
        );
        assert_eq!(None, lexer.next());
    }

    #[test]
    fn test_skips_over_spaces() {
        let text = " ( a  ) ";
        let mut lexer = Lexer::new(text.chars());
        assert_eq!(
            Some(Ok(Token::lparen(TextLocation { row: 1, col: 2 }))),
            lexer.next()
        );
        assert_eq!(
            Some(Ok(Token::identifier(
                "a".to_string(),
                TextLocation { row: 1, col: 4 }
            ))),
            lexer.next()
        );
        assert_eq!(
            Some(Ok(Token::rparen(TextLocation { row: 1, col: 7 }))),
            lexer.next()
        );
        assert_eq!(None, lexer.next());
    }

    #[test]
    fn test_newline_advances_to_new_row() {
        let text = "(a\n)";
        let mut lexer = Lexer::new(text.chars());
        assert_eq!(
            Some(Ok(Token::lparen(TextLocation { row: 1, col: 1 }))),
            lexer.next()
        );
        assert_eq!(
            Some(Ok(Token::identifier(
                "a".to_string(),
                TextLocation { row: 1, col: 2 }
            ))),
            lexer.next()
        );
        assert_eq!(
            Some(Ok(Token::rparen(TextLocation { row: 2, col: 1 }))),
            lexer.next()
        );
        assert_eq!(None, lexer.next());
    }

    #[test]
    fn test_integer_literal_followed_by_eof() {
        let text = "123";
        let mut lexer = Lexer::new(text.chars());
        assert_eq!(
            Some(Ok(Token {
                text: TokenText::Integer(123),
                loc: TextLocation { row: 1, col: 1 },
            })),
            lexer.next()
        );
        assert_eq!(None, lexer.next());
    }

    #[test]
    fn test_integer_literal_followed_by_space() {
        let text = " 123 ";
        let mut lexer = Lexer::new(text.chars());
        assert_eq!(
            Some(Ok(Token {
                text: TokenText::Integer(123),
                loc: TextLocation { row: 1, col: 2 },
            })),
            lexer.next()
        );
        assert_eq!(None, lexer.next());
    }

    #[test]
    fn test_integer_literal_followed_by_paren() {
        let text = "(123)";
        let mut lexer = Lexer::new(text.chars());
        assert_eq!(
            Some(Ok(Token::lparen(TextLocation { row: 1, col: 1 }))),
            lexer.next()
        );
        assert_eq!(
            Some(Ok(Token {
                text: TokenText::Integer(123),
                loc: TextLocation { row: 1, col: 2 },
            })),
            lexer.next()
        );
        assert_eq!(
            Some(Ok(Token::rparen(TextLocation { row: 1, col: 5 }))),
            lexer.next()
        );
        assert_eq!(None, lexer.next());
    }

    #[test]
    fn test_int_arithmetic() {
        let text = "(+ 1 (- 2 3))";
        let mut lexer = Lexer::new(text.chars());
        assert_eq!(
            Some(Ok(Token::lparen(TextLocation { row: 1, col: 1 }))),
            lexer.next()
        );
        assert_eq!(
            Some(Ok(Token::identifier(
                "+".to_string(),
                TextLocation { row: 1, col: 2 }
            ))),
            lexer.next()
        );
        assert_eq!(
            Some(Ok(Token {
                text: TokenText::Integer(1),
                loc: TextLocation { row: 1, col: 4 },
            })),
            lexer.next()
        );
        assert_eq!(
            Some(Ok(Token::lparen(TextLocation { row: 1, col: 6 }))),
            lexer.next()
        );
        assert_eq!(
            Some(Ok(Token::identifier(
                "-".to_string(),
                TextLocation { row: 1, col: 7 }
            ))),
            lexer.next()
        );
        assert_eq!(
            Some(Ok(Token {
                text: TokenText::Integer(2),
                loc: TextLocation { row: 1, col: 9 },
            })),
            lexer.next()
        );
        assert_eq!(
            Some(Ok(Token {
                text: TokenText::Integer(3),
                loc: TextLocation { row: 1, col: 11 },
            })),
            lexer.next()
        );
        assert_eq!(
            Some(Ok(Token::rparen(TextLocation { row: 1, col: 12 }))),
            lexer.next()
        );
        assert_eq!(
            Some(Ok(Token::rparen(TextLocation { row: 1, col: 13 }))),
            lexer.next()
        );
        assert_eq!(None, lexer.next());
    }

    #[test]
    fn test_negative_int() {
        let text = "(+ 1 -1)";
        let mut lexer = Lexer::new(text.chars());
        assert_eq!(
            Some(Ok(Token::lparen(TextLocation { row: 1, col: 1 }))),
            lexer.next()
        );
        assert_eq!(
            Some(Ok(Token::identifier(
                "+".to_string(),
                TextLocation { row: 1, col: 2 }
            ))),
            lexer.next()
        );
        assert_eq!(
            Some(Ok(Token {
                text: TokenText::Integer(1),
                loc: TextLocation { row: 1, col: 4 },
            })),
            lexer.next()
        );
        assert_eq!(
            Some(Ok(Token {
                text: TokenText::Integer(-1),
                loc: TextLocation { row: 1, col: 6 },
            })),
            lexer.next()
        );
        assert_eq!(
            Some(Ok(Token::rparen(TextLocation { row: 1, col: 8 }))),
            lexer.next()
        );
        assert_eq!(None, lexer.next());
    }

    #[test]
    fn test_invalid_int() {
        let text = "123abc";
        let mut lexer = Lexer::new(text.chars());
        assert_eq!(
            Some(Err(LexerError::invalid_integer(
                "123abc".to_string(),
                TextLocation { row: 1, col: 1 }
            ))),
            lexer.next()
        );
        assert_eq!(None, lexer.next());
    }

    #[test]
    fn test_invalid_int_excludes_trailing_spaces() {
        let text = " 123abc ";
        let mut lexer = Lexer::new(text.chars());
        assert_eq!(
            Some(Err(LexerError::invalid_integer(
                "123abc".to_string(),
                TextLocation { row: 1, col: 2 }
            ))),
            lexer.next()
        );
        assert_eq!(None, lexer.next());
    }
}
