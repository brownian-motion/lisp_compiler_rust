use crate::lexer::Lexable;
use crate::memory::*;
use crate::text::CharLocation;
use crate::text::*;

use std::clone::Clone;

use std::fmt;
use std::fmt::Debug;
use std::ops::Range;

use super::elems::*;

use super::traits::*;

pub struct ParseTree {
    pub exprs: MemoryArena<Expr>,
    pub es: MemoryArena<Es>,
    pub sexprs: MemoryArena<SExpr>,
    pub program: Vec<ArenaIdx<SExpr>>,
}

pub enum ParseTreeTraversalIndex {
    Expr(ArenaIdx<Expr>),
    Es(ArenaIdx<Es>),
    SExpr(ArenaIdx<SExpr>),
}

pub struct PreOrderTraversal<'a> {
    next_elems: Vec<(u32, ParseTreeTraversalIndex)>,
    tree: &'a ParseTree,
}

impl<'a> PreOrderTraversal<'a> {
    pub fn new(start_elems: Vec<(u32, ParseTreeTraversalIndex)>, tree: &'a ParseTree) -> Self {
        PreOrderTraversal {
            next_elems: start_elems,
            tree,
        }
    }
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
    pub fn new() -> ParseTree {
        ParseTree {
            exprs: MemoryArena::new(),
            es: MemoryArena::new(),
            sexprs: MemoryArena::new(),
            program: Vec::new(),
        }
    }

    pub fn preorder_traversal<'a>(&'a self) -> PreOrderTraversal<'a> {
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

    pub fn pretty_print<I: Iterator<Item = (u32, ParseTreeTraversalIndex)>>(
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
