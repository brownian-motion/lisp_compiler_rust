use super::Lexer;

pub trait Lexable<I: Iterator<Item = char>> {
    fn lex(self) -> Lexer<I>;
}
