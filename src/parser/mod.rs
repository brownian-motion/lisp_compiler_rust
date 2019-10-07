use crate::errors::*;
use crate::lexer;
use crate::lexer::token::*;
use crate::lexer::{Lexable, Lexer};
use crate::memory::*;
use crate::text::*;
use std::clone::Clone;

use std::fmt;
use std::fmt::Debug;
use std::iter::Peekable;

use std::ops::*;

enum Expr {
    Terminal(Token),
    SExpr(ArenaIdx<SExpr>),
}

impl From<Token> for Expr {
    fn from(token: Token) -> Self {
        Expr::Terminal(token)
    }
}

enum Es {
    Empty(CharLocation),
    Cons(ArenaIdx<Expr>, ArenaIdx<Es>),
}

struct SExpr {
    loc: Range<CharLocation>, // needed to include the left and right parens, which aren't part of the tree any more
    e: ArenaIdx<Expr>,
    es: ArenaIdx<Es>,
}

impl Locateable<Range<CharLocation>> for SExpr {
    fn location(&self) -> Range<CharLocation> {
        self.loc.clone()
    }
}

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

struct ParseTree {
    exprs: MemoryArena<Expr>,
    es: MemoryArena<Es>,
    sexprs: MemoryArena<SExpr>,
    program: Vec<ArenaIdx<SExpr>>,
}

enum ParseTreeTraversalIndex {
    Expr(ArenaIdx<Expr>),
    Es(ArenaIdx<Es>),
    SExpr(ArenaIdx<SExpr>),
}

struct PreOrderTraversal<'a> {
    next_elems: Vec<(u32, ParseTreeTraversalIndex)>,
    tree: &'a ParseTree,
}

impl<'a> Iterator for PreOrderTraversal<'a> {
    type Item = (u32, ParseTreeTraversalIndex);

    fn next(&mut self) -> Option<(u32, ParseTreeTraversalIndex)> {
        match self.next_elems.pop() {
            None => None,
            Some((depth, item)) => {
                match &item {
                    ParseTreeTraversalIndex::SExpr(idx) => {
                        let sexpr = &self.tree.sexprs[idx];
                        self.next_elems
                            .push((depth + 1, ParseTreeTraversalIndex::Es(sexpr.es.clone())));
                        self.next_elems
                            .push((depth + 1, ParseTreeTraversalIndex::Expr(sexpr.e.clone())));
                    }
                    ParseTreeTraversalIndex::Expr(idx) => {
                        let expr = &self.tree.exprs[idx];
                        if let Expr::SExpr(sexpr) = expr {
                            self.next_elems
                                .push((depth + 1, ParseTreeTraversalIndex::SExpr(sexpr.clone())));
                        }
                    }
                    ParseTreeTraversalIndex::Es(idx) => {
                        let es = &self.tree.es[idx];
                        if let Es::Cons(e, es) = es {
                            self.next_elems
                                .push((depth + 1, ParseTreeTraversalIndex::Es(es.clone())));
                            self.next_elems
                                .push((depth + 1, ParseTreeTraversalIndex::Expr(e.clone())));
                        }
                    }
                }
                Some((depth, item))
            }
        }
    }
}

impl ParseTree {
    fn new() -> ParseTree {
        ParseTree {
            exprs: MemoryArena::new(),
            es: MemoryArena::new(),
            sexprs: MemoryArena::new(),
            program: Vec::new(),
        }
    }

    fn preorder_traversal<'a>(&'a self) -> PreOrderTraversal<'a> {
        let stack: Vec<_> = self
            .program
            .iter()
            .map(|idx| (0, ParseTreeTraversalIndex::SExpr(idx.clone())))
            .collect();
        PreOrderTraversal {
            next_elems: stack,
            tree: &self,
        }
    }

    fn pretty_print<I: Iterator<Item = (u32, ParseTreeTraversalIndex)>>(
        &self,
        f: &mut fmt::Formatter<'_>,
        traversal: I,
    ) -> fmt::Result {
        for (depth, item) in traversal {
            write!(f, "{}", "|   ".repeat(depth as usize))?;
            match item {
                ParseTreeTraversalIndex::SExpr(_) => writeln!(f, "SExpr"),
                ParseTreeTraversalIndex::Expr(idx) => {
                    if let Expr::Terminal(token) = &self.exprs[idx] {
                        writeln!(f, "Expr: {:?}", token)
                    } else {
                        writeln!(f, "{}", "Expr")
                    }
                }
                ParseTreeTraversalIndex::Es(idx) => {
                    let es = &self.es[idx];
                    if let Es::Empty(_) = es {
                        writeln!(f, "{}", "<empty>")
                    } else {
                        writeln!(f, "Es")
                    }
                }
            }?;
        }
        Ok(())
    }
}

impl Debug for ParseTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pretty_print(f, self.preorder_traversal())
    }
}

#[test]
fn test_pretty_print_simple_sexpr() {
    let e = Expr::Terminal("a".chars().lex().next().unwrap().unwrap());
    let es = Es::Empty((1, 2).into());
    let mut exprs_arena = MemoryArena::new();
    let mut es_arena = MemoryArena::new();
    let mut sexprs_arena = MemoryArena::new();
    let e = exprs_arena.add(e);
    let es = es_arena.add(es);
    let sexpr = SExpr {
        loc: (1, 1).into()..(1, 2).into(),
        e,
        es,
    };
    let sexpr = sexprs_arena.add(sexpr);
    let parse_tree = ParseTree {
        exprs: exprs_arena,
        es: es_arena,
        sexprs: sexprs_arena,
        program: vec![sexpr],
    };

    let expected = "SExpr\n|   Expr: [Identifier(\"a\") at (l1, c1)..(l1, c2)]\n|   <empty>\n";
    let actual = format!("{:?}", parse_tree);
    assert_eq!(expected, actual);
}

#[test]
fn test_pretty_print_nested_sexpr() {
    // (+ (- 1 2) a)
    let plus = Expr::Terminal("+".chars().lex().next().unwrap().unwrap());
    let minus = Expr::Terminal("-".chars().lex().next().unwrap().unwrap());
    let one = Expr::Terminal("1".chars().lex().next().unwrap().unwrap());
    let two = Expr::Terminal("2".chars().lex().next().unwrap().unwrap());
    let a = Expr::Terminal("a".chars().lex().next().unwrap().unwrap());

    let mut exprs_arena = MemoryArena::new();
    let mut es_arena = MemoryArena::new();
    let mut sexprs_arena = MemoryArena::new();

    let plus = exprs_arena.add(plus);
    let minus = exprs_arena.add(minus);
    let one = exprs_arena.add(one);
    let two = exprs_arena.add(two);
    let a = exprs_arena.add(a);

    let es = es_arena.add(Es::Empty((1, 1).into())); // ... )
    let es = es_arena.add(Es::Cons(two, es)); // ... 2 )
    let es = es_arena.add(Es::Cons(one, es)); // ... 1 2 )

    // (- 1 2)
    let sexpr = sexprs_arena.add(SExpr {
        loc: (1, 1).into()..(1, 2).into(),
        e: minus,
        es,
    });
    let e = exprs_arena.add(Expr::SExpr(sexpr));

    let es = es_arena.add(Es::Empty((1, 1).into())); // ... )
    let es = es_arena.add(Es::Cons(a, es)); // ... a )
    let es = es_arena.add(Es::Cons(e, es)); // ... (- 1 2) a )

    // (+ (- 1 2) a )
    let sexpr = sexprs_arena.add(SExpr {
        loc: (1, 1).into()..(1, 2).into(),
        e: plus,
        es,
    });

    let parse_tree = ParseTree {
        exprs: exprs_arena,
        es: es_arena,
        sexprs: sexprs_arena,
        program: vec![sexpr],
    };

    let expected = "SExpr\n\
                    |   Expr: [Identifier(\"+\") at (l1, c1)..(l1, c2)]\n\
                    |   Es\n\
                    |   |   Expr\n\
                    |   |   |   SExpr\n\
                    |   |   |   |   Expr: [Identifier(\"-\") at (l1, c1)..(l1, c2)]\n\
                    |   |   |   |   Es\n\
                    |   |   |   |   |   Expr: [Integer(1) at (l1, c1)..(l1, c2)]\n\
                    |   |   |   |   |   Es\n\
                    |   |   |   |   |   |   Expr: [Integer(2) at (l1, c1)..(l1, c2)]\n\
                    |   |   |   |   |   |   <empty>\n\
                    |   |   Es\n\
                    |   |   |   Expr: [Identifier(\"a\") at (l1, c1)..(l1, c2)]\n\
                    |   |   |   <empty>\n";
    let actual = format!("{:?}", parse_tree);
    assert_eq!(expected, actual);
}

#[test]
fn test_pretty_print_parsed_simple_sexpr() {
    // (+ (- 1 2) a)
    let parse_tree = "(+ 1 2)".chars().lex().parse().unwrap();

    let expected = "SExpr\n\
                    |   Expr: [Identifier(\"+\") at (l1, c2)..(l1, c3)]\n\
                    |   Es\n\
                    |   |   Expr: [Integer(1) at (l1, c4)..(l1, c5)]\n\
                    |   |   Es\n\
                    |   |   |   Expr: [Integer(2) at (l1, c6)..(l1, c7)]\n\
                    |   |   |   <empty>\n";
    let actual = format!("{:?}", parse_tree);
    assert_eq!(expected, actual);
}

#[test]
fn test_pretty_print_parsed_nested_sexpr() {
    // (+ (- 1 2) a)
    let parse_tree = "(+ (- 1 2) a)".chars().lex().parse().unwrap();

    let expected = "SExpr\n\
                    |   Expr: [Identifier(\"+\") at (l1, c2)..(l1, c3)]\n\
                    |   Es\n\
                    |   |   Expr\n\
                    |   |   |   SExpr\n\
                    |   |   |   |   Expr: [Identifier(\"-\") at (l1, c5)..(l1, c6)]\n\
                    |   |   |   |   Es\n\
                    |   |   |   |   |   Expr: [Integer(1) at (l1, c7)..(l1, c8)]\n\
                    |   |   |   |   |   Es\n\
                    |   |   |   |   |   |   Expr: [Integer(2) at (l1, c9)..(l1, c10)]\n\
                    |   |   |   |   |   |   <empty>\n\
                    |   |   Es\n\
                    |   |   |   Expr: [Identifier(\"a\") at (l1, c12)..(l1, c13)]\n\
                    |   |   |   <empty>\n";
    let actual = format!("{:?}", parse_tree);
    assert_eq!(expected, actual);
}

trait Locater<T, L> {
    fn locate(&self, obj: T) -> L;
}

impl Locater<&ArenaIdx<Expr>, Range<CharLocation>> for ParseTree {
    fn locate(&self, idx: &ArenaIdx<Expr>) -> Range<CharLocation> {
        match &self.exprs[idx] {
            Expr::Terminal(token) => token.location(),
            Expr::SExpr(idx) => self.locate(idx),
        }
    }
}

impl Locater<&ArenaIdx<SExpr>, Range<CharLocation>> for ParseTree {
    fn locate(&self, idx: &ArenaIdx<SExpr>) -> Range<CharLocation> {
        self.sexprs[idx].loc.clone()
    }
}

impl Locater<&ArenaIdx<Es>, Range<CharLocation>> for ParseTree {
    fn locate(&self, idx: &ArenaIdx<Es>) -> Range<CharLocation> {
        match &self.es[idx] {
            Es::Empty(loc) => loc.clone().into(), // TODO: should this be size 0 or 1?
            Es::Cons(e, es) => self.locate(e).start..self.locate(es).end,
        }
    }
}
impl Locater<ArenaIdx<Es>, Range<CharLocation>> for ParseTree {
    fn locate(&self, idx: ArenaIdx<Es>) -> Range<CharLocation> {
        match &self.es[idx] {
            Es::Empty(loc) => loc.clone().into(), // TODO: should this be size 0 or 1?
            Es::Cons(e, es) => self.locate(e).start..self.locate(es).end,
        }
    }
}

trait Parseable<T, E> {
    fn parse(self) -> Result<T, E>;
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

trait Pushable<T> {
    fn push(&mut self, elem: T);
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

#[derive(Debug, PartialEq, Eq)]
pub enum ErrorType {
    UnexpectedEof,
    UnmatchedRightParen,
    EmptySExpr,
    LexerError(lexer::ErrorType),
    UnexpectedStackState,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    loc: Range<CharLocation>,
    error_type: ErrorType,
}

impl Locateable<Range<CharLocation>> for Error {
    fn location(&self) -> Range<CharLocation> {
        self.loc.clone()
    }
}

impl ErrorType {
    fn at(self, loc: Range<CharLocation>) -> Error {
        Error {
            loc,
            error_type: self,
        }
    }
}

impl From<lexer::ErrorType> for ErrorType {
    fn from(lexer_error_type: lexer::ErrorType) -> ErrorType {
        ErrorType::LexerError(lexer_error_type)
    }
}

impl From<lexer::Error> for Error {
    fn from(error: lexer::Error) -> Error {
        Error {
            loc: error.loc,
            error_type: error.error_type.into(),
        }
    }
}

impl CompilerErrorMessage for ErrorType {
    fn message(&self) -> &'static str {
        use ErrorType::*;
        match self {
            UnexpectedEof => "Unexpected end of input (unmatched left parenthesis)",
            UnmatchedRightParen => "Unmatched right parenthesis",
            EmptySExpr => {
                "Empty S-sxpression (must have values between left and right parenthesis)"
            }
            UnexpectedStackState => "Unexpected Stack state!!",
            LexerError(lexer_error) => lexer_error.message(),
        }
    }
}

impl CompilerErrorMessage for Error {
    fn message(&self) -> &'static str {
        self.error_type.message()
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
        for item in self.stack {
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
                    let traversal = PreOrderTraversal {
                        next_elems: vec![(0, ParseTreeTraversalIndex::Expr(idx.clone()))],
                        tree: &self.tree,
                    };
                    self.tree.pretty_print(f, traversal)?;
                }
                ParserStackItem::Es(idx) => {
                    let traversal = PreOrderTraversal {
                        next_elems: vec![(0, ParseTreeTraversalIndex::Es(idx.clone()))],
                        tree: &self.tree,
                    };

                    self.tree.pretty_print(f, traversal)?;
                }
                ParserStackItem::SExpr(idx) => {
                    let traversal = PreOrderTraversal {
                        next_elems: vec![(0, ParseTreeTraversalIndex::SExpr(idx.clone()))],
                        tree: &self.tree,
                    };
                    self.tree.pretty_print(f, traversal)?;
                }
                ParserStackItem::LeftParen(_) => writeln!(f, "{}", "(")?,
                ParserStackItem::RightParen(_) => writeln!(f, "{}", ")")?,
            }
        }
        Ok(())
    }
}
