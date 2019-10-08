use crate::lexer::token::*;
use crate::memory::*;
use crate::text::*;
use std::ops::Range;

pub enum Expr {
    Terminal(Token),
    SExpr(ArenaIdx<SExpr>),
}

impl From<Token> for Expr {
    fn from(token: Token) -> Self {
        Expr::Terminal(token)
    }
}

pub enum Es {
    Empty(CharLocation),
    Cons(ArenaIdx<Expr>, ArenaIdx<Es>),
}

pub struct SExpr {
    pub loc: Range<CharLocation>, // needed to include the left and right parens, which aren't part of the tree any more
    pub e: ArenaIdx<Expr>,
    pub es: ArenaIdx<Es>,
}

impl Locateable<Range<CharLocation>> for SExpr {
    fn location(&self) -> Range<CharLocation> {
        self.loc.clone()
    }
}
