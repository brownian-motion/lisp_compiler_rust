use crate::lexer::token::*;
use crate::lexer::{Lexable, Lexer};
use crate::memory::*;
use crate::text::*;
use std::clone::Clone;

use std::fmt;
use std::fmt::Debug;
use std::iter::Peekable;

use std::ops::*;

pub mod elems;
pub mod errors;
pub mod traits;
pub mod tree;
use crate::parser::elems::*;
use errors::*;
use traits::*;
use tree::*;

// used so that we can fit all of these different types into the same stack
enum ParserStackItem {
    E(ArenaIdx<Expr>),
    Es(ArenaIdx<Es>),
    SExpr(ArenaIdx<SExpr>),
    LeftParen(CharLocation),
    RightParen(CharLocation),
}

impl ParserStackItem {
    fn expr<I: Iterator<Item = char>>(self, parser: &Parser<I>) -> Result<ArenaIdx<Expr>, Error> {
        if let ParserStackItem::E(e) = self {
            Ok(e)
        } else {
            Err(ErrorType::UnexpectedStackState.at(parser.locate(&self)))
        }
    }
    fn es<I: Iterator<Item = char>>(self, parser: &Parser<I>) -> Result<ArenaIdx<Es>, Error> {
        if let ParserStackItem::Es(es) = self {
            Ok(es)
        } else {
            Err(ErrorType::UnexpectedStackState.at(parser.locate(&self)))
        }
    }
    fn sexpr<I: Iterator<Item = char>>(self, parser: &Parser<I>) -> Result<ArenaIdx<SExpr>, Error> {
        if let ParserStackItem::SExpr(se) = self {
            Ok(se)
        } else {
            Err(ErrorType::UnexpectedStackState.at(parser.locate(&self)))
        }
    }
    fn lparen<I: Iterator<Item = char>>(self, parser: &Parser<I>) -> Result<CharLocation, Error> {
        if let ParserStackItem::LeftParen(loc) = self {
            Ok(loc)
        } else {
            Err(ErrorType::UnexpectedStackState.at(parser.locate(&self)))
        }
    }
    fn rparen<I: Iterator<Item = char>>(self, parser: &Parser<I>) -> Result<CharLocation, Error> {
        if let ParserStackItem::RightParen(loc) = self {
            Ok(loc)
        } else {
            Err(ErrorType::UnexpectedStackState.at(parser.locate(&self)))
        }
    }
}

impl<I> Locater<&ParserStackItem, Range<CharLocation>> for Parser<I>
where
    I: Iterator<Item = char>,
{
    fn locate(&self, item: &ParserStackItem) -> Range<CharLocation> {
        match item {
            ParserStackItem::E(e) => self.tree.locate(e),
            ParserStackItem::Es(es) => self.tree.locate(es),
            ParserStackItem::SExpr(sexpr) => self.tree.locate(sexpr),
            ParserStackItem::LeftParen(loc) => loc.clone().into(),
            ParserStackItem::RightParen(loc) => loc.clone().into(),
        }
    }
}

struct Parser<I: Iterator<Item = char>> {
    lexer: Peekable<Lexer<I>>,
    stack: Vec<ParserStackItem>,
    tree: ParseTree,
}

impl<I> Parser<I>
where
    I: Iterator<Item = char>,
{
    fn cursor_location(&self) -> CharLocation {
        self.stack
            .last()
            .map(|item| self.locate(item).end)
            .unwrap_or_else(|| (1, 1).into())
    }
}

impl<I> Pushable<Expr> for Parser<I>
where
    I: Iterator<Item = char>,
{
    fn push(&mut self, expr: Expr) {
        self.stack
            .push(ParserStackItem::E(self.tree.exprs.add(expr)))
    }
}
impl<I> Pushable<Es> for Parser<I>
where
    I: Iterator<Item = char>,
{
    fn push(&mut self, es: Es) {
        self.stack.push(ParserStackItem::Es(self.tree.es.add(es)))
    }
}
impl<I> Pushable<SExpr> for Parser<I>
where
    I: Iterator<Item = char>,
{
    fn push(&mut self, sexpr: SExpr) {
        self.stack
            .push(ParserStackItem::SExpr(self.tree.sexprs.add(sexpr)))
    }
}

impl<I: Iterator<Item = char>> Locater<&[ParserStackItem], Range<CharLocation>> for Parser<I> {
    fn locate(&self, slice: &[ParserStackItem]) -> Range<CharLocation> {
        match (slice.first(), slice.last()) {
            (None, _) | (_, None) => (1, 1).into()..(1, 1).into(),
            (Some(left), Some(right)) => self.locate(left).start..self.locate(right).end,
        }
    }
}

impl<I> Parser<I>
where
    I: Iterator<Item = char>,
{
    fn new(lexer: Lexer<I>) -> Parser<I> {
        Parser {
            stack: Vec::new(),
            lexer: lexer.peekable(),
            tree: ParseTree::new(),
        }
    }

    fn shift(&mut self) -> Result<(), Error> {
        match self.lexer.next() {
            None => Err(ErrorType::UnexpectedEof.at(self.cursor_location().into())),
            Some(Ok(token)) => {
                match token.text {
                    TokenText::LeftParen => {
                        self.stack.push(ParserStackItem::LeftParen(token.loc.start))
                    }
                    TokenText::RightParen => self
                        .stack
                        .push(ParserStackItem::RightParen(token.loc.start)),
                    _ => self
                        .stack
                        .push(ParserStackItem::E(self.tree.exprs.add(token.into()))),
                }
                Ok(())
            }
            Some(Err(e)) => Err(e.into()),
        }
    }

    fn reduce_nothing_to_es(&mut self) -> Result<(), Error> {
        let es = Es::Empty(self.cursor_location());
        self.stack.push(ParserStackItem::Es(self.tree.es.add(es)));
        Ok(())
    }

    fn reduce_sexpr_to_e(&mut self) -> Result<(), Error> {
        let sexpr = self.stack.pop().expect("Expected sexpr!").sexpr(&self)?;

        let e = Expr::SExpr(sexpr);
        self.stack.push(ParserStackItem::E(self.tree.exprs.add(e)));
        Ok(())
    }

    fn reduce_to_sexpr(&mut self) -> Result<(), Error> {
        let rparen_loc = self.stack.pop().expect("Expected rparen!").rparen(&self)?;
        let es = self.stack.pop().expect("Expected es!").es(&self)?;
        let e = self.stack.pop().expect("Expected e!").expr(&self)?;
        let lparen_loc = self.stack.pop().expect("Expected lparen!").lparen(&self)?;

        let sexpr = SExpr {
            e,
            es,
            loc: lparen_loc..rparen_loc,
        };

        self.stack
            .push(ParserStackItem::SExpr(self.tree.sexprs.add(sexpr)));
        Ok(())
    }

    fn reduce_e_es_to_es(&mut self) -> Result<(), Error> {
        let es = self.stack.pop().expect("Expected es!").es(&self)?;
        let e = self.stack.pop().expect("Expected e!").expr(&self)?;

        let es = Es::Cons(e, es);

        self.push(es);
        Ok(())
    }
}

impl<I> Parser<I>
where
    I: Iterator<Item = char>,
{
    fn finish(mut self) -> Result<ParseTree, Vec<Error>> {
        use ErrorType::UnexpectedStackState;
        let mut errors = Vec::new();
        for item in self.stack.into_iter().rev() {
            match item {
                ParserStackItem::SExpr(idx) => {
                    self.tree.program.push(idx);
                }

                ParserStackItem::E(idx) => {
                    errors.push(UnexpectedStackState.at(self.tree.locate(&idx)));
                }

                ParserStackItem::Es(idx) => {
                    errors.push(UnexpectedStackState.at(self.tree.locate(&idx)));
                }

                ParserStackItem::LeftParen(loc) | ParserStackItem::RightParen(loc) => {
                    errors.push(UnexpectedStackState.at(loc.into()));
                }
            }
        }
        if errors.len() == 0 {
            Ok(self.tree)
        } else {
            Err(errors)
        }
    }
}

impl<I> Debug for Parser<I>
where
    I: Iterator<Item = char> + Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f)?;
        writeln!(f, "===Tree===")?;
        writeln!(f, "{:?}", self.tree)?;
        writeln!(f, "===Stack===")?;
        self.pretty_print_stack(f)?;
        writeln!(f, "===Lookahead===")?;
        writeln!(f, "{:?}", self.lexer)?;
        Ok(())
    }
}

impl<I> Into<Vec<Error>> for Parser<I>
where
    I: Iterator<Item = char>,
{
    fn into(self) -> Vec<Error> {
        self.lexer.filter_map(Result::err).map(Into::into).collect()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParserOperation {
    Shift,
    ReduceToSExpr,
    ReduceEEsToEs,
    ReduceNothingToEs,
    ReduceSExprToE,
    EmitError(Error),
    Finish,
}

impl<I> Parser<I>
where
    I: Iterator<Item = char>,
{
    fn get_next_operation(&mut self) -> ParserOperation {
        use ErrorType::*;
        use ParserOperation::*;
        use ParserStackItem::*;

        let lookahead = self.lexer.peek();

        let stack_top: Option<&ParserStackItem> = self.stack.last();
        match stack_top {
            None => Shift,
            Some(LeftParen(_)) => Shift,
            Some(&RightParen(loc)) => {
                if let Some(Es(_)) = self.stack.get(self.stack.len() - 2) {
                    if let Some(E(_)) = self.stack.get(self.stack.len() - 3) {
                        if let Some(LeftParen(_)) = self.stack.get(self.stack.len() - 4) {
                            // if the stack is [ ... ( E Es ) ]  then reduce to SExpr
                            ReduceToSExpr
                        } else {
                            // if the stack is [ ... ? E Es ) ] or [ E Es ) ], then there's an unmatched right paren
                            EmitError(UnmatchedRightParen.at(loc.into()))
                        }
                    } else {
                        // if the stack is [ ... ? Es ) ] or [ Es ) ], then there's an unmatched right paren
                        EmitError(UnmatchedRightParen.at(loc.into()))
                    }
                } else {
                    // if the stack is [ ... ? ) ] or [ ) ], then there's an unmatched right paren
                    EmitError(UnmatchedRightParen.at(loc.into()))
                }
            }
            Some(E(e)) => match lookahead {
                None => EmitError(UnexpectedEof.at(self.tree.locate(e))),

                Some(Err(lex_error)) => EmitError(lex_error.clone().into()),
                Some(Ok(token)) if token.text == TokenText::RightParen => ReduceNothingToEs,
                Some(Ok(_)) => Shift,
            },
            Some(Es(_)) => match lookahead {
                None => EmitError(UnexpectedEof.at(self.cursor_location().into())),
                Some(Ok(token)) if token.text == TokenText::RightParen => {
                    if let Some(E(_)) = self.stack.get(self.stack.len() - 2) {
                        if let Some(LeftParen(_)) = self.stack.get(self.stack.len() - 3) {
                            Shift
                        } else {
                            ReduceEEsToEs
                        }
                    } else {
                        EmitError(UnmatchedRightParen.at(self.cursor_location().into()))
                    }
                }
                Some(Ok(token)) => EmitError(UnexpectedStackState.at(token.location())),
                Some(Err(error)) => EmitError(error.clone().into()),
            },
            Some(SExpr(_)) => {
                match lookahead {
                    // you can have multiple s-expressions in a program (def a 1) (print! (+ a 1))
                    Some(Ok(token)) if token.text == TokenText::LeftParen => Shift,
                    // s-expressions are nested as expressions (print! (+ a 1))
                    Some(Ok(_)) => ReduceSExprToE,
                    Some(Err(error)) => EmitError(error.clone().into()),
                    // This assumes, possibly incorrectly, that all of the stack is now s-expressions.
                    // We will need to validate that when returning the resulting parse tree.
                    None => Finish,
                }
            }
        }
    }
}

#[test]
fn test_get_next_parser_op_shift_onto_empty_stack() {
    for text in vec!["(", "a", "1", ")", "(+ 1 2 3)"] {
        let mut parser = Parser {
            lexer: text.chars().lex().peekable(),
            stack: Vec::new(),
            tree: ParseTree::new(),
        };

        let op = parser.get_next_operation();
        assert_eq!(ParserOperation::Shift, op);
    }
}

impl<I> Parseable<ParseTree, Vec<Error>> for Parser<I>
where
    I: Iterator<Item = char>,
{
    fn parse(mut self) -> Result<ParseTree, Vec<Error>> {
        use ParserOperation::*;
        loop {
            let op = self.get_next_operation();
            let op_result = match op {
                Finish => return self.finish(),
                Shift => self.shift(),
                ReduceToSExpr => self.reduce_to_sexpr(),
                ReduceEEsToEs => self.reduce_e_es_to_es(),
                ReduceNothingToEs => self.reduce_nothing_to_es(),
                ReduceSExprToE => self.reduce_sexpr_to_e(),
                EmitError(e) => Err(e),
            };
            match op_result {
                Ok(_) => (),
                Err(e) => {
                    let mut errors = vec![e];
                    let mut more_errors = self.into();
                    errors.append(&mut more_errors);
                    errors.sort_by_key(|e| e.location().start);
                    return Err(errors);
                }
            }
        }
    }
}

impl<I> Parseable<ParseTree, Vec<Error>> for Lexer<I>
where
    I: Iterator<Item = char>,
{
    fn parse(self) -> Result<ParseTree, Vec<Error>> {
        Parser::new(self).parse()
    }
}

impl<I> Parser<I>
where
    I: Iterator<Item = char>,
{
    fn pretty_print_stack(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for item in self.stack.iter() {
            match item {
                ParserStackItem::E(idx) => {
                    let traversal = PreOrderTraversal::new(
                        vec![(0, ParseTreeTraversalIndex::Expr(idx.clone()))],
                        &self.tree,
                    );
                    self.tree.pretty_print(f, traversal)?;
                }
                ParserStackItem::Es(idx) => {
                    let traversal = PreOrderTraversal::new(
                        vec![(0, ParseTreeTraversalIndex::Es(idx.clone()))],
                        &self.tree,
                    );

                    self.tree.pretty_print(f, traversal)?;
                }
                ParserStackItem::SExpr(idx) => {
                    let traversal = PreOrderTraversal::new(
                        vec![(0, ParseTreeTraversalIndex::SExpr(idx.clone()))],
                        &self.tree,
                    );
                    self.tree.pretty_print(f, traversal)?;
                }
                ParserStackItem::LeftParen(_) => writeln!(f, "{}", "(")?,
                ParserStackItem::RightParen(_) => writeln!(f, "{}", ")")?,
            }
        }
        Ok(())
    }
}
