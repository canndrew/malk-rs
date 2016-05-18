use std::fmt;

use lexer::{Span, Token, TokenKind};
use parse::{ParseError, StructTerm, parse_struct_term_inner, pprint_struct_term,
                        StructType, parse_composite_type_inner, pprint_struct_type,
                        EnumType, pprint_enum_type,
                        EnumTerm, parse_enum_term_inner, pprint_enum_term};

pub enum ExprKind {
    Parens(Box<Expr>),
    StructTerm(Box<StructTerm>),
    StructType(Box<StructType>),
    EnumType(Box<EnumType>),
    EnumTerm(Box<EnumTerm>),
}

pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

pub fn parse_expr(tokens: &[Token]) -> Result<Expr, ParseError> {
    let mut iter = tokens.iter().filter(|t| !t.is_whitespace());
    match iter.next() {
        None => return Err(ParseError::UnexpectedEndOfInput),
        Some(t) => {
            match t.kind {
                TokenKind::Parens(ref sub_tokens) => {
                    let sub_expr = try!(parse_expr(&sub_tokens[..]));
                    let expr = Expr {
                        kind: ExprKind::Parens(Box::new(sub_expr)),
                        span: t.span,
                    };
                    Ok(expr)
                },
                TokenKind::CurlyBraces(ref sub_tokens) => {
                    let (head_elems, tail) = try!(parse_struct_term_inner(&sub_tokens[..]));
                    let struct_term = StructTerm {
                        head_elems: head_elems,
                        tail: tail,
                        span: t.span,
                    };
                    let kind = ExprKind::StructTerm(Box::new(struct_term));
                    let expr = Expr {
                        kind: kind,
                        span: t.span,
                    };
                    Ok(expr)
                },
                TokenKind::SquareBraces(ref sub_tokens) => {
                    let enum_term_kind = try!(parse_enum_term_inner(&sub_tokens[..]));
                    let enum_term = EnumTerm {
                        kind: enum_term_kind,
                        span: t.span,
                    };
                    let kind = ExprKind::EnumTerm(Box::new(enum_term));
                    let expr = Expr {
                        kind: kind,
                        span: t.span,
                    };
                    Ok(expr)
                },
                TokenKind::Hash => {
                    match iter.next() {
                        None => return Err(ParseError::UnexpectedEndOfInput),
                        Some(t) => {
                            match t.kind {
                                TokenKind::CurlyBraces(ref sub_tokens) => {
                                    let (head_elems, tail) = try!(parse_composite_type_inner(&sub_tokens[..]));
                                    let struct_type = StructType {
                                        head_elems: head_elems,
                                        tail: tail,
                                        span: t.span,
                                    };
                                    let kind = ExprKind::StructType(Box::new(struct_type));
                                    let expr = Expr {
                                        kind: kind,
                                        span: t.span,
                                    };
                                    Ok(expr)
                                },
                                TokenKind::SquareBraces(ref sub_tokens) => {
                                    let (head_elems, tail) = try!(parse_composite_type_inner(&sub_tokens[..]));
                                    let enum_type = EnumType {
                                        head_elems: head_elems,
                                        tail: tail,
                                        span: t.span,
                                    };
                                    let kind = ExprKind::EnumType(Box::new(enum_type));
                                    let expr = Expr {
                                        kind: kind,
                                        span: t.span,
                                    };
                                    Ok(expr)
                                },
                                _ => {
                                    return Err(ParseError::UnexpectedToken);
                                },
                            }
                        },
                    }
                },
                _ => {
                    return Err(ParseError::UnexpectedToken);
                },
            }
        },
    }
}

pub fn pprint_expr(expr: &Expr, f: &mut fmt::Formatter) -> fmt::Result {
    match expr.kind {
        ExprKind::Parens(ref sub_expr) => {
            try!(write!(f, "("));
            try!(pprint_expr(&sub_expr, f));
            write!(f, ")")
        },
        ExprKind::StructTerm(ref struct_term) => {
            pprint_struct_term(struct_term, f)
        },
        ExprKind::StructType(ref struct_type) => {
            pprint_struct_type(struct_type, f)
        },
        ExprKind::EnumType(ref enum_type) => {
            pprint_enum_type(enum_type, f)
        },
        ExprKind::EnumTerm(ref enum_term) => {
            pprint_enum_term(enum_term, f)
        },
    }
}

