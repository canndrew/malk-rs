use std::fmt;

use core::Term;
use lexer::{Span, TokensRef, TokenKind};
use parse::{ParseError, StructTerm, parse_struct_term_inner,
                        StructType, parse_composite_type_inner,
                        EnumType,
                        EnumTerm, parse_enum_term_inner,
                        FuncTerm,
                        FuncType,
                        SingularFuncTerm,
                        SingularFuncType,
                        FuncApp,
                        Typed, 
                        TypeLiteral,
                        LevelLiteral,
                        Parens,
                        LetExpr, parse_let_expr,
                        reparse_pattern_from_expr,
                        Ident};

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Parens(Box<Expr>),
    LetExpr(Box<LetExpr>),
    StructTerm(Box<StructTerm>),
    StructType(Box<StructType>),
    EnumType(Box<EnumType>),
    EnumTerm(Box<EnumTerm>),
    FuncTerm(Box<FuncTerm>),
    FuncType(Box<FuncType>),
    FuncApp(Box<FuncApp>),
    Variable(Ident),
    TypeLiteral(TypeLiteral),
    LevelLiteral(LevelLiteral),
    Numeric(Ident),
    Typed(Box<Typed>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
    pub term: Option<Term>,
}

// TODO: dunno why it can't just infer this.
unsafe impl Send for Expr {}

fn parse_single_expr<'t, 's: 't>(tr: TokensRef<'t, 's>) -> Result<(Expr, TokensRef<'t, 's>), ParseError> {
    let mut iter = tr.tokens.iter().enumerate();
    let ret;
    loop {
        ret = match iter.next() {
            None => return Err(ParseError::ExpectedExpression { pos: tr.end }),
            Some((index, t)) => {
                match t.kind {
                    TokenKind::Whitespace(_) => continue,
                    TokenKind::Ident("let") => {
                        let let_expr = parse_let_expr(tr.range_from(1))?;
                        let expr = Expr {
                            kind: ExprKind::LetExpr(Box::new(let_expr)),
                            span: tr.span(),
                            term: None,
                        };
                        (expr, TokensRef {
                            tokens: &[],
                            end: tr.end,
                        })
                    },
                    TokenKind::Ident("Level") => {
                        let mut rank = 0;
                        loop {
                            match iter.next() {
                                Some((_, t)) => match t.kind {
                                    TokenKind::Symbol("+") => {
                                        rank += 1;
                                    },
                                    _ => break,
                                },
                                _ => break,
                            }
                        }
                        let span = tr.range(index, index + rank + 1).span();
                        let level_literal = LevelLiteral {
                            rank: rank,
                            span: span,
                        };
                        let expr = Expr {
                            kind: ExprKind::LevelLiteral(level_literal),
                            span: span,
                            term: None,
                        };
                        (expr, tr.range_from(index + rank + 1))
                    },
                    TokenKind::Ident("Type") => {
                        let mut rank = 0;
                        loop {
                            match iter.next() {
                                Some((_, t)) => match t.kind {
                                    TokenKind::Symbol("+") => {
                                        rank += 1;
                                    },
                                    _ => break,
                                },
                                _ => break,
                            }
                        }
                        let span = tr.range(index, index + rank + 1).span();
                        let type_literal = TypeLiteral {
                            rank: rank,
                            span: span,
                        };
                        let expr = Expr {
                            kind: ExprKind::TypeLiteral(type_literal),
                            span: span,
                            term: None,
                        };
                        (expr, tr.range_from(index + rank + 1))
                    },
                    TokenKind::Ident(s) => {
                        let span = tr.range(index, index + 1).span();
                        let ident = Ident {
                            text: String::from(s),
                            span: span,
                        };
                        let expr = Expr {
                            kind: ExprKind::Variable(ident),
                            span: span,
                            term: None,
                        };
                        (expr, tr.range_from(index + 1))
                    },
                    TokenKind::Numeric(s) => {
                        let span = tr.range(index, index + 1).span();
                        let ident = Ident {
                            text: String::from(s),
                            span: span,
                        };
                        let expr = Expr {
                            kind: ExprKind::Numeric(ident),
                            span: span,
                            term: None,
                        };
                        (expr, tr.range_from(index + 1))
                    },
                    TokenKind::Bracket(bracket_kind, ref sub_tokens) => {
                        let span = tr.range(index, index + 1).span();
                        let rest = tr.range_from(index + 1);
                        let expr = match bracket_kind {
                            '(' => {
                                let sub_expr = parse_expr(sub_tokens.borrow())?;
                                let span = tr.range(index, index + 1).span();
                                Expr {
                                    kind: ExprKind::Parens(Box::new(sub_expr)),
                                    span: span,
                                    term: None,
                                }
                            },
                            '{' => {
                                let (head_elems, tail) = parse_struct_term_inner(sub_tokens.borrow())?;
                                let struct_term = StructTerm {
                                    head_elems: head_elems,
                                    tail: tail,
                                    span: span,
                                };
                                let kind = ExprKind::StructTerm(Box::new(struct_term));
                                Expr {
                                    kind: kind,
                                    span: span,
                                    term: None,
                                }
                            },
                            '[' => {
                                parse_enum_term_or_function_inner(sub_tokens.borrow())?
                            },
                            _ => {
                                return Err(ParseError::UnexpectedToken { pos: t.start });
                            },
                        };
                        (expr, rest)
                    },
                    TokenKind::Symbol("#") => {
                        match iter.next() {
                            None => return Err(ParseError::ExpectedToken { pos: tr.end }),
                            Some((_, t)) => {
                                match t.kind {
                                    TokenKind::Bracket(bracket_kind, ref sub_tokens) => {
                                        let span = tr.range(index, index + 2).span();
                                        let rest = tr.range_from(index + 2);
                                        let expr = match bracket_kind {
                                            '{' => {
                                                let (head_elems, tail) = parse_composite_type_inner(sub_tokens.borrow())?;
                                                let struct_type = StructType {
                                                    head_elems: head_elems,
                                                    tail: tail,
                                                    span: span,
                                                };
                                                let kind = ExprKind::StructType(Box::new(struct_type));
                                                Expr {
                                                    kind: kind,
                                                    span: span,
                                                    term: None,
                                                }
                                            },
                                            '[' => {
                                                let (head_elems, tail) = parse_composite_type_inner(sub_tokens.borrow())?;
                                                let enum_type = EnumType {
                                                    head_elems: head_elems,
                                                    tail: tail,
                                                    span: span,
                                                };
                                                let kind = ExprKind::EnumType(Box::new(enum_type));
                                                Expr {
                                                    kind: kind,
                                                    span: span,
                                                    term: None,
                                                }
                                            },
                                            _ => {
                                                return Err(ParseError::UnexpectedToken { pos: t.start });
                                            },
                                        };
                                        (expr, rest)
                                    },
                                    _ => {
                                        return Err(ParseError::UnexpectedToken { pos: t.start });
                                    },
                                }
                            },
                        }
                    },
                    _ => {
                        return Err(ParseError::UnexpectedToken { pos: t.start });
                    },
                }
            },
        };
        break;
    };
    Ok(ret)
}

pub fn parse_expr<'t, 's: 't>(ts: TokensRef<'t, 's>) -> Result<Expr, ParseError> {
    println!("parse_expr({:?})", ts);
    let (mut expr, mut ts) = parse_single_expr(ts)?;
    loop {
        println!("still in parse_expr({:?})", ts);
        match ts.tokens.get(0) {
            None => return Ok(expr),
            Some(t) => {
                match t.kind {
                    TokenKind::Whitespace(_) => {
                        ts = ts.range_from(1);
                        continue;
                    },
                    TokenKind::Symbol("->") => {
                        let (rhs, new_ts) = parse_single_expr(ts.range_from(2))?;
                        ts = new_ts;
                        let lhs = reparse_pattern_from_expr(&expr)?;
                        let span = Span {
                            start: lhs.span.start,
                            end: rhs.span.end,
                        };
                        let singular_func_type = SingularFuncType {
                            pattern: Box::new(lhs),
                            body_type: Box::new(rhs),
                            span: span,
                        };
                        let func_type = FuncType::Singular(singular_func_type);
                        let kind = ExprKind::FuncType(Box::new(func_type));
                        expr = Expr {
                            kind: kind,
                            span: span,
                            term: None,
                        };
                    },
                    TokenKind::Symbol("=>") => {
                        let (rhs, new_ts) = parse_single_expr(ts.range_from(2))?;
                        ts = new_ts;
                        let lhs = reparse_pattern_from_expr(&expr)?;
                        let span = Span {
                            start: lhs.span.start,
                            end: rhs.span.end,
                        };
                        let singular_func_term = SingularFuncTerm {
                            pattern: Box::new(lhs),
                            body: Box::new(rhs),
                            span: span,
                        };
                        let func_term = FuncTerm::Singular(singular_func_term);
                        let kind = ExprKind::FuncTerm(Box::new(func_term));
                        expr = Expr {
                            kind: kind,
                            span: span,
                            term: None,
                        };
                    },
                    TokenKind::Symbol(":") => {
                        let (rhs, new_ts) = parse_single_expr(ts.range_from(1))?;
                        ts = new_ts;
                        let span = Span {
                            start: expr.span.start,
                            end: rhs.span.end,
                        };
                        let typed = Typed {
                            typed_term: expr,
                            typed_type: rhs,
                            span: span,
                        };
                        let kind = ExprKind::Typed(Box::new(typed));
                        expr = Expr {
                            kind: kind,
                            span: span,
                            term: None,
                        };
                    },
                    _ => {
                        let (arg_expr, new_ts) = parse_single_expr(ts)?;
                        ts = new_ts;
                        let span = Span {
                            start: expr.span.start,
                            end: arg_expr.span.end,
                        };
                        let func_app = FuncApp {
                            func: expr,
                            arg: arg_expr,
                            span: span,
                        };
                        let kind = ExprKind::FuncApp(Box::new(func_app));
                        expr = Expr {
                            kind: kind,
                            span: span,
                            term: None,
                        };
                    },
                }
            }
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            ExprKind::Parens(ref sub_expr) => {
                write!(f, "({})", sub_expr)
            },
            ExprKind::StructTerm(ref struct_term) => {
                write!(f, "{}", struct_term)
            },
            ExprKind::StructType(ref struct_type) => {
                write!(f, "{}", struct_type)
            },
            ExprKind::EnumType(ref enum_type) => {
                write!(f, "{}", enum_type)
            },
            ExprKind::EnumTerm(ref enum_term) => {
                write!(f, "{}", enum_term)
            },
            ExprKind::Variable(ref ident) |
            ExprKind::Numeric(ref ident) => {
                write!(f, "{}", ident)
            },
            ExprKind::FuncTerm(ref func_term) => {
                write!(f, "{}", func_term)
            }
            ExprKind::FuncType(ref func_type) => {
                write!(f, "{}", func_type)
            }
            ExprKind::FuncApp(ref func_app) => {
                write!(f, "{}", func_app)
            },
            ExprKind::Typed(ref typed) => {
                write!(f, "{}", typed)
            },
            ExprKind::TypeLiteral(ref type_literal) => {
                write!(f, "{}", type_literal)
            },
            ExprKind::LevelLiteral(ref type_literal) => {
                write!(f, "{}", type_literal)
            },
            ExprKind::LetExpr(ref let_expr) => {
                write!(f, "{}", let_expr)
            },
        }
    }
}

fn parse_enum_term_or_function_inner<'t, 's: 't>(ts: TokensRef<'t, 's>) -> Result<Expr, ParseError> {
    /*
    let mut is_function = true;
    for token in ts.tokens.iter() {
        match token.kind {
            TokenKind::Symbol("=") => {
                is_function = false
                break;
            },
            TokenKind::Symbol("=>") => {
                is_function = true;
                break;
            },
            TokenKind::WhiteSpace(_) => (),
            _ => is_function = false,
        }
    }

    if is_function {
        let enum_func_term = parse_enum_func_term_inner(ts.borrow())?;
        let func_term = FuncTerm::Enum(enum_func_term);
        let kind = ExprKind::FuncTerm(Box::new(func_term));
        Ok(Expr {
            kind: kind,
            span: span,
            term: None,
        })
    }
    else {
        let enum_term_kind = parse_enum_term_inner(ts.borrow())?;
        let enum_term = EnumTerm {
            kind: enum_term_kind,
            span: span,
        };
        let kind = ExprKind::EnumTerm(Box::new(enum_term));
        Ok(Expr {
            kind: kind,
            span: span,
            term: None,
        })
    }
    */
    unimplemented!()
}

