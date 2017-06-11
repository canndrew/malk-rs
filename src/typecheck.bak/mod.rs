use core::{Term, TermKind, Type, TypeKind, Form, FormKind, Debug};
use lexer::Span;

pub use self::context::{Context, ContextNode};
pub use self::expr::{typecheck_expr, typeget_expr, typecheck_type, typeget_type};
pub use self::typed::{typecheck_typed, typeget_typed};
pub use self::variable::{typecheck_variable, typeget_variable};
pub use self::struct_term::{typecheck_struct_term, typeget_struct_term};
pub use self::struct_type::{typecheck_struct_type, typeget_struct_type};
pub use self::pattern::{typecheck_open_pattern, typeget_open_pattern, close_pattern};
pub use self::func_term::{typecheck_func_term, typeget_func_term};
/*
pub use self::context::{Ctx, CtxNode, CtxNodeKind};
/*
*/
pub use self::numeric::typecheck_numeric;
pub use self::pattern::{typecheck_pattern, typeget_pattern};
pub use self::func_type::typecheck_func_type;
pub use self::func_app::typecheck_func_app;
/*
pub use self::let_expr::typecheck_let_expr;
*/
*/

pub mod debug;
mod expr;
mod typed;
mod context;
mod variable;
mod struct_term;
mod struct_type;
mod pattern;
mod func_term;
/*
/*
*/
mod struct_type;
mod numeric;
mod func_type;
mod func_app;
//mod let_expr;
*/

#[derive(Debug)]
pub enum TypeCheckError {
    TypeExpected {
        span: Span,
        got: Term,
        got_ty: Type,
    },
    TypeMismatch {
        span: Span,
        expected: Type,
        got: Type,
    },
    NoSuchVariable {
        name: String,
    },
    UnexpectedStruct {
        span: Span,
        expected: Type,
    },
    BindUnknownType {
        span: Span,
    },
    UnexpectedType {
        span: Span,
        expected: Type,
    },
    UnexpectedFunction {
        span: Span,
        expected: Type,
    },
    /*
    InvalidNumeric {
        numeric: String,
        span: Span,
    },
    ExpectedFunctionType {
        span: Span,
    },
    */
}

/*
#[derive(Debug)]
pub struct TypeMismatch {
    pub span: Span,
    pub expected: Type,
    pub got: Type,
}
*/

pub fn into_term(ty: Type, level: u32, ctx: Context) -> (Term, Type) {
    let c = d!({ ctx.core_ctx() });
    let term = ty.as_term();
    let ty = Type::form(Form::ty(level, d!({ None }), c));
    (term, ty)
}

pub fn into_universe(ty: Type, span: Span) -> Result<u32, TypeCheckError> {
    match *ty.kind() {
        TypeKind::Form(ref f) => {
            match *f.kind() {
                FormKind::Type { level } => return Ok(level),
                _ => (),
            }
        },
        _ => (),
    };
    Err(TypeCheckError::UnexpectedType {
        span: span,
        expected: ty,
    })
}

pub fn into_type(ty: Term, universe: Type, span: Span) -> Result<Type, TypeCheckError> {
    match *universe.kind() {
        TypeKind::Form(ref f) => {
            match *f.kind() {
                FormKind::Type { .. } => {
                    return match *ty.kind() {
                        TermKind::Form(ref f) => Ok(Type::form(f.clone())),
                        TermKind::Elim(ref e) => Ok(Type::elim(e.clone())),
                        TermKind::Intro(..) => panic!("this should have been filtered"),
                    }
                }
                _ => (),
            }
        },
        _ => (),
    };
    Err(TypeCheckError::TypeExpected {
        span: span,
        got: ty,
        got_ty: universe,
    })
}

pub fn types_match(ty: &Type, expected: &Type, span: Span) -> Result<(), TypeCheckError> {
    if !ty.subtype_of(expected) {
        return Err(TypeCheckError::TypeMismatch {
            span: span,
            expected: expected.clone(),
            got: ty.clone(),
        });
    }
    Ok(())
}

/*
#[cfg(test)]
mod test {
    use super::*;

    use std::collections::HashMap;

    use core::{Term, TermKind, World, bump_index};
    use lexer::{lex, TextPos, Span};

    use parse::{Expr, Ident, SYMBOL_TABLE, parse_expr};

    #[test]
    fn all_forms() {
        fn check(expr: &Expr, term_type: &Term, ctx_elems: &[(&str, Term)]) -> Term {
            let world = World {
                intrinsics: HashMap::new(),
            };
            let mut elems = Vec::new();
            for &(name, ref term) in ctx_elems {
                let ident = Ident {
                    text: String::from(name),
                    span: Span {
                        start: TextPos::start(),
                        end: TextPos::start(),
                    },
                };
                elems.push((ident, term.clone()));
            }
            let ctx = Ctx::Empty;
            check_acc(expr, term_type, ctx, &elems[..], &world)
        }

        fn check_acc<'c, 'p: 'c>(expr: &'p Expr, term_type: &Term, ctx: Ctx<'c, 'p>, ctx_elems: &[(Ident, Term)], world: &World) -> Term {
            match ctx_elems.split_first() {
                Some((&(ref name, ref var_type), rest)) => {
                    let ctx_node_kind = CtxNodeKind::Var {
                        var_name: name,
                        var_type: var_type.clone(),
                    };
                    let ctx_node = CtxNode {
                        tail: ctx,
                        kind: ctx_node_kind,
                    };
                    let sub_ctx = Ctx::Cons(&ctx_node);
                    check_acc(expr, term_type, sub_ctx, rest, world)
                },
                None => {
                    unwrap!(typecheck_expr(expr, term_type, ctx, world))
                }
            }
        }

        let test_cases = &[
            (
                "0lev",
                &[][..],
                Term::new(TermKind::LevelZero),
                Term::new(TermKind::Level),
            ),
            (
                "2lev",
                &[][..],
                Term::new(TermKind::LevelSucc {
                    pred: Term::new(TermKind::LevelSucc {
                        pred: Term::new(TermKind::LevelZero),
                    }),
                }),
                Term::new(TermKind::Level),
            ),
            (
                "Type 0lev",
                &[][..],
                Term::new(TermKind::Type {
                    level: Term::new(TermKind::LevelZero),
                }),
                Term::new(TermKind::Type {
                    level: Term::new(TermKind::LevelSucc {
                        pred: Term::new(TermKind::LevelZero),
                    }),
                }),
            ),
            /*
            (
                "{}",
                &[][..],
                Term::new(TermKind::UnitTerm),
                Term::new(TermKind::UnitType),
            ),
            (
                "{; x}",
                &[
                    ("", Term::new(TermKind::Type { level: Term::new(TermKind::LevelZero) })),
                    ("x", Term::new(TermKind::Var(0))),
                ][..],
                Term::new(TermKind::Var(0)),
                Term::new(TermKind::Var(1)),
            ),
            (
                "{x; y}",
                &[
                    ("", Term::new(TermKind::Type { level: Term::new(TermKind::LevelZero) })),
                    ("", Term::new(TermKind::Type { level: Term::new(TermKind::LevelZero) })),
                    ("x", Term::new(TermKind::Var(1))),
                    ("y", Term::new(TermKind::Var(1))),
                ][..],
                Term::new(TermKind::PairTerm {
                    head: Term::new(TermKind::Var(1)),
                    tail: Term::new(TermKind::Var(1)),
                }),
                Term::new(TermKind::PairType {
                    head_type: Term::new(TermKind::Var(3)),
                    tail_type: Term::new(TermKind::Var(3)),
                }),
            ),
            (
                "{x}",
                &[
                    ("", Term::new(TermKind::Type { level: Term::new(TermKind::LevelZero) })),
                    ("x", Term::new(TermKind::Var(0))),
                ][..],
                Term::new(TermKind::PairTerm {
                    head: Term::new(TermKind::Var(0)),
                    tail: Term::new(TermKind::UnitTerm),
                }),
                Term::new(TermKind::PairType {
                    head_type: Term::new(TermKind::Var(1)),
                    tail_type: Term::new(TermKind::UnitType),
                }),
            ),
            (
                "{x,}",
                &[
                    ("", Term::new(TermKind::Type { level: Term::new(TermKind::LevelZero) })),
                    ("x", Term::new(TermKind::Var(0))),
                ][..],
                Term::new(TermKind::PairTerm {
                    head: Term::new(TermKind::Var(0)),
                    tail: Term::new(TermKind::UnitTerm),
                }),
                Term::new(TermKind::PairType {
                    head_type: Term::new(TermKind::Var(1)),
                    tail_type: Term::new(TermKind::UnitType),
                }),
            ),
            (
                "{x, y}",
                &[
                    ("", Term::new(TermKind::Type { level: Term::new(TermKind::LevelZero) })),
                    ("", Term::new(TermKind::Type { level: Term::new(TermKind::LevelZero) })),
                    ("x", Term::new(TermKind::Var(1))),
                    ("y", Term::new(TermKind::Var(1))),
                ][..],
                Term::new(TermKind::PairTerm {
                    head: Term::new(TermKind::Var(1)),
                    tail: Term::new(TermKind::PairTerm {
                        head: Term::new(TermKind::Var(1)),
                        tail: Term::new(TermKind::UnitTerm),
                    }),
                }),
                Term::new(TermKind::PairType {
                    head_type: Term::new(TermKind::Var(3)),
                    tail_type: Term::new(TermKind::PairType {
                        head_type: Term::new(TermKind::Var(3)),
                        tail_type: Term::new(TermKind::UnitType),
                    }),
                }),
            ),
            */
            (
                "x: Ty",
                &[
                    ("Ty", Term::new(TermKind::Type { level: Term::new(TermKind::LevelZero) })),
                    ("x", Term::new(TermKind::Var(0))),
                ][..],
                Term::new(TermKind::Var(0)),
                Term::new(TermKind::Var(1)),
            ),
            (
                "x",
                &[
                    ("", Term::new(TermKind::Type { level: Term::new(TermKind::LevelZero) })),
                    ("x", Term::new(TermKind::Var(0))),
                ][..],
                Term::new(TermKind::Var(0)),
                Term::new(TermKind::Var(1)),
            ),
            (
                "(x: Ty) -> Ty",
                &[
                    ("Ty", Term::new(TermKind::Type { level: Term::new(TermKind::LevelZero) })),
                ][..],
                Term::new(TermKind::FuncType {
                    arg_type: Term::new(TermKind::Var(0)),
                    res_type: Term::new(TermKind::Var(1)),
                }),
                Term::new(TermKind::Type {
                    level: Term::new(TermKind::LevelSucc { pred: Term::new(TermKind::LevelZero) }),
                }),
            ),
            (
                "{x: Ty; y: Ty} -> Ty",
                &[
                    ("Ty", Term::new(TermKind::Type { level: Term::new(TermKind::LevelZero) })),
                ][..],
                Term::new(TermKind::FuncType {
                    arg_type: Term::new(TermKind::PairType {
                        head_type: Term::new(TermKind::Var(0)),
                        tail_type: Term::new(TermKind::Var(1)),
                    }),
                    res_type: Term::new(TermKind::Var(1)),
                }),
                Term::new(TermKind::Type {
                    level: Term::new(TermKind::LevelSucc { pred: Term::new(TermKind::LevelZero) }),
                }),
            ),
            (
                "{{Foo: (Type 0lev); x: Foo}; {y: Foo; z: Ty}} -> Ty",
                &[
                    ("Ty", Term::new(TermKind::Type { level: Term::new(TermKind::LevelZero) })),
                ][..],
                Term::new(TermKind::FuncType {
                    arg_type: Term::new(TermKind::PairType {
                        head_type: Term::new(TermKind::PairType {
                            head_type: Term::new(TermKind::Type {
                                level: Term::new(TermKind::LevelZero),
                            }),
                            tail_type: Term::new(TermKind::Var(0)),
                        }),
                        tail_type: Term::new(TermKind::PairElim {
                            pair: Term::new(TermKind::Var(0)),
                            res: Term::new(TermKind::PairType {
                                head_type: Term::new(TermKind::Var(1)),
                                tail_type: Term::new(TermKind::Var(4)),
                            }),
                            head_type: Term::new(TermKind::Type {
                                level: Term::new(TermKind::LevelZero),
                            }),
                            tail_type: Term::new(TermKind::Var(0)),
                        }),
                    }),
                    res_type: Term::new(TermKind::Var(1)),
                }),
                Term::new(TermKind::Type {
                    level: Term::new(TermKind::LevelSucc { pred: Term::new(TermKind::LevelZero) }),
                }),
            ),
            (
                "{{Foo: (Type 0lev); x: Foo}; {y: Foo; z: Ty}} => z",
                &[
                    ("Ty", Term::new(TermKind::Type { level: Term::new(TermKind::LevelZero) })),
                ][..],
                Term::new(TermKind::FuncTerm {
                    body: Term::new(TermKind::PairElim {
                        pair: Term::new(TermKind::Var(0)),
                        head_type: Term::new(TermKind::PairType {
                            head_type: Term::new(TermKind::Type {
                                level: Term::new(TermKind::LevelZero),
                            }),
                            tail_type: Term::new(TermKind::Var(0)),
                        }),
                        tail_type: Term::new(TermKind::PairElim {
                            pair: Term::new(TermKind::Var(0)),
                            head_type: Term::new(TermKind::Type {
                                level: Term::new(TermKind::LevelZero),
                            }),
                            tail_type: Term::new(TermKind::Var(0)),
                            res: Term::new(TermKind::PairType {
                                head_type: Term::new(TermKind::Var(1)),
                                tail_type: Term::new(TermKind::Var(5)),
                            }),
                        }),
                        res: Term::new(TermKind::PairElim {
                            pair: Term::new(TermKind::Var(1)),
                            head_type: Term::new(TermKind::Type {
                                level: Term::new(TermKind::LevelZero),
                            }),
                            tail_type: Term::new(TermKind::Var(0)),
                            res: Term::new(TermKind::PairElim {
                                pair: Term::new(TermKind::Var(2)),
                                head_type: Term::new(TermKind::Var(1)),
                                tail_type: Term::new(TermKind::Var(6)),
                                res: Term::new(TermKind::Var(0)),
                            }),
                        }),
                    }),
                }),
                Term::new(TermKind::FuncType {
                    arg_type: Term::new(TermKind::PairType {
                        head_type: Term::new(TermKind::PairType {
                            head_type: Term::new(TermKind::Type {
                                level: Term::new(TermKind::LevelZero),
                            }),
                            tail_type: Term::new(TermKind::Var(0)),
                        }),
                        tail_type: Term::new(TermKind::PairElim {
                            pair: Term::new(TermKind::Var(0)),
                            res: Term::new(TermKind::PairType {
                                head_type: Term::new(TermKind::Var(1)),
                                tail_type: Term::new(TermKind::Var(4)),
                            }),
                            head_type: Term::new(TermKind::Type {
                                level: Term::new(TermKind::LevelZero),
                            }),
                            tail_type: Term::new(TermKind::Var(0)),
                        }),
                    }),
                    res_type: Term::new(TermKind::Var(1)),
                }),
            ),
            /*
            (
                "({x: Ty}: #{Ty}) -> Ty",
                &[
                    ("Ty", Term::new(TermKind::Type { level: Term::new(TermKind::LevelZero) })),
                ][..],
                Term::new(TermKind::FuncType {
                    arg_type: Term::new(TermKind::PairType {
                        head_type: Term::new(TermKind::Var(0)),
                        tail_type: Term::new(TermKind::UnitType),
                    }),
                    res_type: Term::new(TermKind::Var(1)),
                }),
                Term::new(TermKind::Type {
                    level: Term::new(TermKind::LevelSucc { pred: Term::new(TermKind::LevelZero) }),
                }),
            ),
            (
                "(x: Ty) => x",
                &[
                    ("Ty", Term::new(TermKind::Type { level: Term::new(TermKind::LevelZero) })),
                ][..],
                Term::new(TermKind::FuncTerm {
                    body: Term::new(TermKind::Var(0)),
                }),
                Term::new(TermKind::FuncType {
                    arg_type: Term::new(TermKind::Var(0)),
                    res_type: Term::new(TermKind::Var(1)),
                }),
            ),
            (
                "{a: A, b: B, c: C} => {c, b, a}",
                &[
                    ("A", Term::new(TermKind::Type { level: Term::new(TermKind::LevelZero) })),
                    ("B", Term::new(TermKind::Type { level: Term::new(TermKind::LevelZero) })),
                    ("C", Term::new(TermKind::Type { level: Term::new(TermKind::LevelZero) })),
                ][..],
                Term::new(TermKind::FuncTerm {
                    body: Term::new(TermKind::PairTerm {
                        head: Term::new(TermKind::Var(1)),
                        tail: Term::new(TermKind::PairTerm {
                            head: Term::new(TermKind::Var(4)),
                            tail: Term::new(TermKind::PairTerm {
                                head: Term::new(TermKind::Var(7)),
                                tail: Term::new(TermKind::UnitTerm),
                            }),
                        }),
                    }),
                }),
                Term::new(TermKind::FuncType {
                    arg_type: Term::new(TermKind::PairType {
                        head_type: Term::new(TermKind::Var(2)),
                        tail_type: Term::new(TermKind::PairType {
                            head_type: Term::new(TermKind::Var(2)),
                            tail_type: Term::new(TermKind::PairType {
                                head_type: Term::new(TermKind::Var(2)),
                                tail_type: Term::new(TermKind::UnitType),
                            }),
                        }),
                    }),
                    res_type: Term::new(TermKind::PairType {
                        head_type: Term::new(TermKind::Var(1)),
                        tail_type: Term::new(TermKind::PairType {
                            head_type: Term::new(TermKind::Var(3)),
                            tail_type: Term::new(TermKind::PairType {
                                head_type: Term::new(TermKind::Var(5)),
                                tail_type: Term::new(TermKind::UnitType),
                            }),
                        }),
                    }),
                }),
            ),
            (
                "let x: #{} = {}; x",
                &[
                    ("Ty", Term::new(TermKind::Type { level: Term::new(TermKind::LevelZero) })),
                ][..],
                Term::new(TermKind::UnitTerm),
                Term::new(TermKind::UnitType),
            ),
            */
        ];
        for &(src, ctx_elems, ref term, ref term_type) in test_cases {
            println!("");
            println!("# Checking {}", src);
            println!("");
            let lexed = unwrap!(lex(src, SYMBOL_TABLE));
            let expr = unwrap!(parse_expr(lexed.borrow()));

            let checked_term = check(&expr, term_type, ctx_elems);
            if *term != checked_term {
                println!("term    == {:#?}", term);
                println!("checked == {:#?}", checked_term);
                panic!("mismatch!");
            }
            assert_eq!(*term, checked_term);
        };
    }
}
*/

