use crate::errors::*;
use crate::lexer::token::*;
use crate::lexer::traits::*;
use crate::memory::*;
use crate::parser::elems::*;
use crate::parser::traits::*;
use crate::parser::tree::*;
use std::fmt;
use std::fmt::Debug;

use crate::text::*;
use std::collections::HashMap;
use std::ops::Range;

#[derive(Debug)]
pub struct ConsCell {
    pub car: Value,
    pub cdr: Option<ArenaIdx<ConsCell>>,
    pub loc: Range<CharLocation>,
}

#[derive(Debug)]
pub enum Value {
    Identifier(ArenaIdx<String>, Range<CharLocation>),
    Integer(i32, Range<CharLocation>),
    SExpr(ArenaIdx<ConsCell>),
    Nil,
}

pub struct AST {
    identifier_names: MemoryArena<String>,
    cells: MemoryArena<ConsCell>,
    program: Vec<ArenaIdx<ConsCell>>,
}

impl From<ParseTree> for AST {
    fn from(parse_tree: ParseTree) -> Self {
        use ParseTreeTraversalIndex as Index;
        let mut ident_names: MemoryArena<String> = MemoryArena::new();
        let mut cells: MemoryArena<ConsCell> = MemoryArena::new();

        let mut cons_idx_by_sexpr_idx: HashMap<ArenaIdx<SExpr>, ArenaIdx<ConsCell>> =
            HashMap::new();
        let mut cars_by_expr_idx: HashMap<ArenaIdx<Expr>, Value> = HashMap::new();
        let mut cdrs_by_es_idx: HashMap<ArenaIdx<Es>, ArenaIdx<ConsCell>> = HashMap::new();

        let traversal = parse_tree
            .preorder_traversal()
            .collect::<Vec<_>>()
            .into_iter()
            .rev();

        for item in traversal {
            match item.1 {
                Index::Es(es_idx) => match &parse_tree.es[&es_idx] {
                    Es::Empty(_) => {
                        // no point in storing a whole bunch of pointers to nil in the cdr hash map!
                        // we can just leave these indexes uninserted, which will yield None anyways when fetched from the cdr lookup map
                    }
                    Es::Cons(car_idx, cdr_idx) => {
                        // we will use the lookup maps above to determine the cdr for this cell,
                        // and assume that we have already traversed the es above by iterating backwards

                        let value: Value = match cars_by_expr_idx.remove(&car_idx) {
                        	Some(val) => val,
        					None => panic!("We should have already traversed ever Expr in the CAR field of a Parse Tree ES node at {:?}", parse_tree.locate(es_idx)),
                        };
                        let cdr: Option<ArenaIdx<ConsCell>> =
                            cdrs_by_es_idx.get(&cdr_idx).map(|idx| idx.clone());
                        let loc = parse_tree.locate(&es_idx);
                        let cons_idx = cells.add(ConsCell {
                            car: value,
                            cdr,
                            loc,
                        });
                        let cell = &cells[&cons_idx];
                        cdrs_by_es_idx.insert(es_idx.clone(), cons_idx);
                    }
                },
                Index::SExpr(sexpr_idx) => {
                    let sexpr = &parse_tree.sexprs[&sexpr_idx];
                    let value = match cars_by_expr_idx.remove(&sexpr.e){
                    	Some(val) => val,
                    	None => panic!("{}\n\tError at {:?}, unknown CAR at {:?}\n\tcars_by_expr_idx: {:?}\n\tcdrs_by_es_idx: {:?}",
                    		"We should have already traversed the CAR field of any SExpr before encountering it!",
                    		 parse_tree.locate(&sexpr_idx), parse_tree.locate(&sexpr.e), cars_by_expr_idx, cdrs_by_es_idx),
                    };
                    let cdr = cdrs_by_es_idx.remove(&sexpr.es);
                    let loc = parse_tree.locate(&sexpr_idx);
                    let cons_idx = cells.add(ConsCell {
                        car: value,
                        cdr,
                        loc,
                    });
                    let cell = &cells[&cons_idx];
                    cons_idx_by_sexpr_idx.insert(sexpr_idx, cons_idx);
                }
                Index::Expr(expr_idx) => {
                    let value = match &parse_tree.exprs[&expr_idx] {
                        Expr::Terminal(token) => {
                            match &token.text {
        						TokenText::Identifier(text) => {
							        // TODO: de-duplicate the identifier names using a HashMap's .or_insert_with() method
        							let text_idx = ident_names.add(text.clone());
        							Value::Identifier(text_idx, token.location())
        						},
        						TokenText::Integer(val) => Value::Integer(*val, token.location()),
        						_ => panic!("There shouldn't be any parenthesis tokens left in the parse tree, but one was encountered!"),
    						}
                        }
                        Expr::SExpr(sexpr_idx) => match cons_idx_by_sexpr_idx.remove(&sexpr_idx) {
                            Some(val) => Value::SExpr(val),
                            None => panic!("{}\n\tError at {:?}\n\tcons_idx_by_sexpr_idx: {:?}",
    								"We should have already traversed every SExpr in the CAR field of a Parse Tree Expr node.",
    								parse_tree.locate(sexpr_idx), cons_idx_by_sexpr_idx),
                        },
                    };
                    cars_by_expr_idx.insert(expr_idx, value);
                }
            }
        }

        let program: Vec<ArenaIdx<ConsCell>> = parse_tree.program.iter().rev()
        .map(|sexpr_idx| match cons_idx_by_sexpr_idx.remove(sexpr_idx) {
        	Some(val) => val,
        	None => panic!("{}\n\tcons_idx_by_sexpr_idx: {:?}",
        	"We should have encountered every top-level SExpr exactly once, and never referenced it again until now",
        	cons_idx_by_sexpr_idx),
        })
        .collect();

        return AST {
            identifier_names: ident_names,
            cells: cells,
            program,
        };
    }
}

struct PreOrderAstTraversal<'a> {
    ast: &'a AST,
    next_nodes: Vec<(u32, ArenaIdx<ConsCell>)>,
}

impl Iterator for PreOrderAstTraversal<'_> {
    type Item = (u32, ArenaIdx<ConsCell>);

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_nodes.pop() {
            None => None,
            Some((depth, idx)) => {
                let cell = &self.ast.cells[&idx];
                if let Some(cdr_idx) = &cell.cdr {
                    self.next_nodes.push((depth, cdr_idx.clone()));
                }
                if let Value::SExpr(car_idx) = &cell.car {
                    self.next_nodes.push((depth + 1, car_idx.clone()));
                }
                Some((depth, idx.clone()))
            }
        }
    }
}

impl AST {
    fn preorder_traversal<'a>(&'a self) -> PreOrderAstTraversal<'a> {
        PreOrderAstTraversal {
            // [ (first sexpr), (second sexpr), (third sexpr), ... ]
            next_nodes: self
                .program
                .iter()
                .rev() // because we need this as a STACK, with the first item at the end
                .map(|idx| (0, idx.clone()))
                .collect(),
            ast: self,
        }
    }
}

#[test]
fn test_preorder_traversal_tail_heavy_sexpr_program() {
    use Value::*;
    let actual_ast: AST = "(+ 1 (+ 2 3))".chars().lex().parse().unwrap().into();

    let traversal = actual_ast.preorder_traversal();

    let actual_vals: Vec<_> = traversal
        .map(|(depth, cell_idx)| (depth, &actual_ast.cells[cell_idx].car))
        .collect();

    if let (depth, Identifier(idx, _)) = &actual_vals[0] {
        let name = &actual_ast.identifier_names[idx];
        assert_eq!(
            name, "+",
            "Expected identifier \"+\" at [0], found {:?}",
            name
        );
        assert_eq!(*depth, 0, "Expected depth of 0 at [0], found {}", depth);
    } else {
        panic!(
            "Expected CAR of identifier + at [0], found {:?}",
            actual_vals[0]
        );
    }

    if let (depth, Integer(1, _)) = actual_vals[1] {
        assert_eq!(depth, 0, "Expected depth of 0 at [1], found {}", depth);
    } else {
        panic!(
            "Expected CAR of integer 1 at [1], found {:?}",
            actual_vals[0]
        );
    }

    if let (depth, SExpr(_)) = actual_vals[2] {
        assert_eq!(depth, 0, "Expected depth of 0 at [2], found {}", depth);
    } else {
        panic!(
            "Expected CAR of cons cell at [2], found {:?}",
            actual_vals[0]
        );
    }

    if let (depth, Identifier(idx, _)) = &actual_vals[3] {
        assert_eq!(actual_ast.identifier_names[idx], "+");
        assert_eq!(1, *depth, "Expected depth of 1 at [3], found {}", *depth);
    } else {
        panic!(
            "Expected CAR of identifier \"+\" at [3], found {:?}",
            actual_vals[0]
        );
    }

    if let (depth, Integer(2, _)) = actual_vals[4] {
        assert_eq!(depth, 1, "Expected depth of 1 at [4], found {}", depth);
    } else {
        panic!(
            "Expected CAR of integer 2 at [4], found {:?}",
            actual_vals[0]
        );
    }

    if let (depth, Integer(3, _)) = actual_vals[5] {
        assert_eq!(depth, 1, "Expected depth of 1 at [5], found {}", depth);
    } else {
        panic!(
            "Expected CAR of integer 3 at [5], found {:?}",
            actual_vals[0]
        );
    }
}

#[test]
fn test_preorder_traversal_two_sexpr_program() {
    use Value::*;
    let parse_tree = "(+ 1 (+ 2 3))\n(- (- 4 5) 6)"
        .chars()
        .lex()
        .parse()
        .unwrap();
    println!("{:?}", parse_tree);
    let actual_ast: AST = parse_tree.into();
    println!("\nAST:\n{:?}", actual_ast);
    println!(
        "Traversal: {:?}",
        actual_ast
            .preorder_traversal()
            .map(|(_d, c)| c)
            .collect::<Vec<_>>()
    );

    let actual_vals: Vec<_> = actual_ast
        .preorder_traversal()
        .map(|(depth, cell_idx)| (depth, &actual_ast.cells[cell_idx].car))
        .collect();

    if let (depth, Identifier(idx, _)) = &actual_vals[0] {
        let name = &actual_ast.identifier_names[idx];
        assert_eq!(
            name, "+",
            "Expected CAR of identifier \"+\" at [0], found identifier {:?} (cell idx {:?})",
            name, idx
        );
        assert_eq!(0, *depth, "Expected depth of 0 at [0], found {}", depth);
    } else {
        panic!(
            "Expected CAR of identifier + at [0], found {:?}",
            actual_vals[0]
        );
    }

    if let (depth, Integer(1, _)) = actual_vals[1] {
        assert_eq!(0, depth, "Expected depth of 0 at [1], found {}", depth);
    } else {
        panic!("Expected integer 1 at [1], found {:?}", actual_vals[1]);
    }

    if let (depth, SExpr(_)) = actual_vals[2] {
        assert_eq!(0, depth, "Expected depth of 0 at [2], found {}", depth);
    } else {
        panic!(
            "Expected cons cell as the value at [2], found {:?}",
            actual_vals[2]
        );
    }

    if let (depth, Identifier(idx, _)) = &actual_vals[3] {
        assert_eq!(actual_ast.identifier_names[idx], "+");
        assert_eq!(1, *depth, "Expected depth of 1 at [3], found {}", depth);
    } else {
        panic!("Expected identifier + at [3], found {:?}", actual_vals[3]);
    }

    if let (depth, Integer(2, _)) = actual_vals[4] {
        assert_eq!(1, depth, "Expected depth of 1 at [4], found {}", depth);
    } else {
        panic!("Expected integer 2 at [4], found {:?}", actual_vals[4]);
    }

    if let (depth, Integer(3, _)) = actual_vals[5] {
        assert_eq!(1, depth, "Expected depth of 1 at [5], found {}", depth);
    } else {
        panic!("Expected integer 3 at [5], found {:?}", actual_vals[5]);
    }

    if let (depth, Identifier(idx, _)) = &actual_vals[6] {
        let name = &actual_ast.identifier_names[idx];
        assert_eq!(
            name, "-",
            "Expected CAR of identifier \"-\" at [6], found {:?}",
            name
        );
        assert_eq!(0, *depth, "Expected depth of 0 at [6], found {}", depth);
    } else {
        panic!("Expected identifier - at [6], found {:?}", actual_vals[6]);
    }

    if let (depth, SExpr(_)) = actual_vals[7] {
        assert_eq!(0, depth, "Expected depth of 0 at [7], found {}", depth);
    } else {
        panic!(
            "Expected cons cell as the value at [7], found {:?}",
            actual_vals[7]
        );
    }

    if let (depth, Identifier(idx, _)) = &actual_vals[8] {
        let name = &actual_ast.identifier_names[idx];
        assert_eq!(
            name, "-",
            "Expected identifier \"-\" at [8], found {:?}",
            name
        );
        assert_eq!(1, *depth, "Expected depth of 1 at [8], found {}", depth);
    } else {
        panic!("Expected identifier - at [8], found {:?}", actual_vals[8]);
    }

    if let (depth, Integer(4, _)) = actual_vals[9] {
        assert_eq!(1, depth, "Expected depth of 1 at [9], found {}", depth);
    } else {
        panic!("Expected integer 4 at [9], found {:?}", actual_vals[9]);
    }

    if let (depth, Integer(5, _)) = actual_vals[10] {
        assert_eq!(1, depth, "Expected depth of 1 at [10], found {}", depth);
    } else {
        panic!("Expected integer 5 at [10], found {:?}", actual_vals[10]);
    }

    if let (depth, Integer(6, _)) = actual_vals[11] {
        assert_eq!(0, depth, "Expected depth of 0 at [11], found {}", depth);
    } else {
        panic!("Expected integer 6 at [11], found {:?}", actual_vals[11]);
    }
}

#[test]
fn test_preorder_traversal_front_heavy_sexpr_program() {
    use Value::*;
    let parse_tree = "(- (- 1 2) 3)".chars().lex().parse().unwrap();
    println!("{:?}", parse_tree);
    let actual_ast: AST = parse_tree.into();
    println!("\nAST: {:?}", actual_ast);
    let traversal = actual_ast.preorder_traversal();

    let actual_vals: Vec<_> = traversal
        .map(|(_depth, cell_idx)| &actual_ast.cells[cell_idx].car)
        .collect();

    if let Identifier(idx, _) = &actual_vals[0] {
        assert_eq!(actual_ast.identifier_names[idx], "-");
    } else {
        panic!("Expected identifier - at [0], found {:?}", actual_vals[0]);
    }

    if let SExpr(_) = actual_vals[1] {
    } else {
        panic!(
            "Expected cons cell as the value at [1], found {:?}",
            actual_vals[1]
        );
    }

    if let Identifier(idx, _) = &actual_vals[2] {
        assert_eq!(actual_ast.identifier_names[idx], "-");
    } else {
        panic!("Expected identifier - at [2], found {:?}", actual_vals[2]);
    }

    if let Integer(1, _) = actual_vals[3] {
    } else {
        panic!("Expected integer 1 at [3], found {:?}", actual_vals[3]);
    }

    if let Integer(2, _) = actual_vals[4] {
    } else {
        panic!("Expected integer 2 at [4], found {:?}", actual_vals[4]);
    }

    if let Integer(3, _) = actual_vals[5] {
    } else {
        panic!("Expected integer 3 at [5], found {:?}", actual_vals[5]);
    }
}

impl PartialEq for AST {
    fn eq(&self, other: &AST) -> bool {
        use Value::*;
        if self.program.len() != other.program.len() {
            return false;
        }

        let mut to_compare: Vec<(ArenaIdx<_>, ArenaIdx<_>)> = self
            .program
            .iter()
            .map(ArenaIdx::clone)
            .zip(other.program.iter().map(ArenaIdx::clone))
            .collect();
        loop {
            if let Some((left_idx, right_idx)) = to_compare.pop() {
                let (left, right) = (&self.cells[left_idx], &other.cells[right_idx]);
                match (&left.car, &right.car) {
                    (Nil, Nil) => {}
                    (Integer(a, _), Integer(b, _)) if a == b => {}
                    (Identifier(a, _), Identifier(b, _))
                        if self.identifier_names[a] == other.identifier_names[b] => {}
                    (SExpr(a), SExpr(b)) => to_compare.push((a.clone(), b.clone())),
                    _ => {
                        return false;
                    }
                }
                match (&left.cdr, &right.cdr) {
                    (None, None) => {}
                    (Some(a), Some(b)) => {
                        to_compare.push((a.clone(), b.clone()));
                    }
                    _ => {
                        return false;
                    }
                }
            } else {
                return true;
            }
        }
    }
}

impl Eq for AST {}

#[derive(Debug)]
enum AstDebugItem {
    LeftParen,
    RightParen,
    Cell(ArenaIdx<ConsCell>),
}

impl Debug for AST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut print_stack: Vec<(usize, AstDebugItem)> = self
            .program
            .iter()
            .map(ArenaIdx::clone)
            .flat_map(|cell| {
                vec![
                    (0, AstDebugItem::LeftParen),
                    (1, AstDebugItem::Cell(cell)),
                    (0, AstDebugItem::RightParen),
                ]
                .into_iter()
            })
            .rev()
            .collect();
        loop {
            match print_stack.pop() {
                None => return Ok(()),
                Some((depth, item)) => {
                    let padding = " ".repeat(depth);
                    match item {
                        AstDebugItem::LeftParen => {
                            writeln!(f, "{}(", padding)?;
                        }
                        AstDebugItem::RightParen => {
                            writeln!(f, "{})", padding)?;
                        }
                        AstDebugItem::Cell(idx) => {
                            let cell = &self.cells[idx];
                            if let Some(cdr_idx) = &cell.cdr {
                                print_stack.push((depth, AstDebugItem::Cell(cdr_idx.clone())));
                            }
                            match &cell.car {
                                Value::Integer(val, _) => {
                                    writeln!(f, "{}{}", padding, val)?;
                                }
                                Value::Identifier(ident, _) => {
                                    writeln!(f, "{}{}", padding, self.identifier_names[ident])?;
                                }
                                Value::SExpr(cons_idx) => {
                                    print_stack.push((depth, AstDebugItem::RightParen));
                                    print_stack
                                        .push((depth + 1, AstDebugItem::Cell(cons_idx.clone())));
                                    print_stack.push((depth, AstDebugItem::LeftParen));
                                }
                                Value::Nil => {
                                    writeln!(f, "{}{}", padding, "nil")?;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn fmt_errors<E: CompilerError>(errors: Vec<E>) -> String {
    format!(
        "Errors: {:?}",
        errors.iter().map(format_error).collect::<Vec<_>>()
    )
}

fn unwrap<T, E: CompilerError>(val: Result<T, Vec<E>>) -> T {
    match val {
        Ok(t) => t,
        Err(errors) => panic!(fmt_errors(errors)),
    }
}

#[test]
fn test_simple_sexpr_ast_parsing() {
    let parse_tree = match "( + 1 2 )".chars().lex().parse() {
        Ok(tree) => tree,
        Err(errors) => panic!(fmt_errors(errors)),
    };
    let actual_ast: AST = parse_tree.into();
    let mut identifier_names = MemoryArena::new();
    let plus = identifier_names.add("+".to_string());
    let plus = Value::Identifier(plus, (1, 3).into()..(1, 4).into());
    let one = Value::Integer(1, (1, 5).into()..(1, 6).into());
    let two = Value::Integer(2, (1, 7).into()..(1, 8).into());
    let mut cells = MemoryArena::new();
    let tail = cells.add(ConsCell {
        car: two,
        cdr: None,
        loc: (1, 7).into()..(1, 8).into(),
    });
    let tail = cells.add(ConsCell {
        car: one,
        cdr: Some(tail),
        loc: (1, 5).into()..(1, 8).into(),
    });
    let list = cells.add(ConsCell {
        car: plus,
        cdr: Some(tail),
        loc: (1, 3).into()..(1, 8).into(),
    });
    let expected_ast = AST {
        identifier_names,
        cells,
        program: vec![list],
    };
    assert_eq!(expected_ast, actual_ast);
}

#[test]
fn test_two_sexpr_ast_parsing() {
    let parse_tree = match "( + 1 2 )\n( - (* 3 4) 5)".chars().lex().parse() {
        Ok(tree) => tree,
        Err(errors) => panic!(fmt_errors(errors)),
    };
    let actual_ast: AST = parse_tree.into();

    let mut identifier_names = MemoryArena::new();
    let plus = identifier_names.add("+".to_string());
    let minus = identifier_names.add("-".to_string());
    let times = identifier_names.add("*".to_string());
    let identifier_names = identifier_names; // no longer mut

    let plus = Value::Identifier(plus, (1, 3).into()..(1, 4).into());
    let one = Value::Integer(1, (1, 5).into()..(1, 6).into());
    let two = Value::Integer(2, (1, 7).into()..(1, 8).into());
    let minus = Value::Identifier(minus, (2, 3).into()..(2, 4).into());
    let times = Value::Identifier(times, (2, 6).into()..(2, 7).into());
    let three = Value::Integer(3, (2, 9).into()..(2, 10).into());
    let four = Value::Integer(4, (2, 11).into()..(2, 12).into());
    let five = Value::Integer(5, (2, 14).into()..(2, 15).into());

    let mut cells = MemoryArena::new();
    let tail = cells.add(ConsCell {
        car: two,
        cdr: None,
        loc: (1, 7).into()..(1, 8).into(),
    });
    let tail = cells.add(ConsCell {
        car: one,
        cdr: Some(tail),
        loc: (1, 5).into()..(1, 8).into(),
    });
    let first_list = cells.add(ConsCell {
        car: plus,
        cdr: Some(tail),
        loc: (1, 3).into()..(1, 8).into(),
    });

    let tail = cells.add(ConsCell {
        car: five,
        cdr: None,
        loc: (2, 14).into()..(2, 15).into(),
    });
    let nested_tail = cells.add(ConsCell {
        car: four,
        cdr: None,
        loc: (2, 11).into()..(2, 12).into(),
    });
    let nested_tail = cells.add(ConsCell {
        car: three,
        cdr: Some(nested_tail),
        loc: (2, 9).into()..(2, 12).into(),
    });
    let nested_list = cells.add(ConsCell {
        car: times,
        cdr: Some(nested_tail),
        loc: (2, 5).into()..(2, 12).into(),
    });
    let tail = cells.add(ConsCell {
        car: Value::SExpr(nested_list),
        cdr: Some(tail),
        loc: (2, 5).into()..(2, 15).into(),
    });
    let second_list = cells.add(ConsCell {
        car: minus,
        cdr: Some(tail),
        loc: (2, 1).into()..(2, 16).into(),
    });

    let expected_ast = AST {
        identifier_names,
        cells,
        program: vec![first_list, second_list],
    };
    assert_eq!(expected_ast, actual_ast);
}

#[test]
fn test_simple_sexpr_ast_equal_to_self() {
    let parse_tree = unwrap("( + 1 2 )".chars().lex().parse());
    let actual_ast: AST = parse_tree.into();
    assert_eq!(actual_ast, actual_ast);
}

#[test]
fn test_simple_sexpr_can_parse_to_ast_without_panicking() {
    let _ast: AST = unwrap("( + 1 2 )".chars().lex().parse()).into();
}

#[test]
fn test_nested_sexpr_with_newline_can_parse_to_ast_without_panicking() {
    let _ast: AST = unwrap("(+\t 1 (\n- 2\r\n 3))".chars().lex().parse()).into();
}

#[test]
fn test_simple_sexpr_ast_equal_to_same_program_with_space_prefix() {
    let left: AST = unwrap(" ( + 1 2 )".chars().lex().parse()).into();
    let right: AST = unwrap("( + 1 2 )".chars().lex().parse()).into();
    assert_eq!(left, right);
}

#[test]
fn test_debug_fmt_ast_simple_sexpr() {
    let ast: AST = match "( + 1 2 )".chars().lex().parse() {
        Ok(tree) => tree,
        Err(errors) => panic!(fmt_errors(errors)),
    }
    .into();
    let actual_fmt = format!("{:?}", ast);
    let expected_fmt = "(\n +\n 1\n 2\n)\n";
    assert_eq!(expected_fmt, actual_fmt);
}

#[test]
fn test_debug_fmt_ast_tail_heavy_nested_sexpr() {
    let ast: AST = match "( + 1 (- 2 3))".chars().lex().parse() {
        Ok(tree) => tree,
        Err(errors) => panic!(fmt_errors(errors)),
    }
    .into();
    let actual_fmt = format!("{:?}", ast);
    let expected_fmt = "(\n +\n 1\n (\n  -\n  2\n  3\n )\n)\n";
    assert_eq!(expected_fmt, actual_fmt);
}
