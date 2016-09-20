use std::{fmt, slice};

use lexer::{Span, Token, TokenKind};
use parse::{ParseError, StructTerm, parse_struct_term_inner, pprint_struct_term,
                        StructType, parse_composite_type_inner, pprint_struct_type,
                        EnumType, pprint_enum_type,
                        EnumTerm, parse_enum_term_inner, pprint_enum_term,
                        FuncTerm, pprint_func_term,
                        reparse_pattern_from_expr,
                        Ident, pprint_ident};

pub enum ExprKind {
    Parens(Box<Expr>),
    StructTerm(Box<StructTerm>),
    StructType(Box<StructType>),
    EnumType(Box<EnumType>),
    EnumTerm(Box<EnumTerm>),
    FuncTerm(Box<FuncTerm>),
    Variable(Ident),
}

pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

fn parse_single_expr(iter: &mut slice::Iter<Token>) -> Result<Expr, ParseError> {
    let expr;
    loop {
        expr = match iter.next() {
            None => return Err(ParseError::UnexpectedEndOfInput),
            Some(t) => {
                match t.kind {
                    TokenKind::Whitespace => continue,
                    TokenKind::Word(ref s) => {
                        let ident = Ident {
                            text: s.clone(),
                            span: t.span,
                        };
                        let expr = Expr {
                            kind: ExprKind::Variable(ident),
                            span: t.span,
                        };
                        expr
                    },
                    TokenKind::Parens(ref sub_tokens) => {
                        let sub_expr = try!(parse_expr(&sub_tokens[..]));
                        let expr = Expr {
                            kind: ExprKind::Parens(Box::new(sub_expr)),
                            span: t.span,
                        };
                        expr
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
                        expr
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
                        expr
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
                                        expr
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
                                        expr
                                    },
                                    _ => {
                                        return Err(ParseError::UnexpectedToken);
                                    },
                                }
                            },
                        }
                    },
                    /*
                    TokenKind::At => {
                        match iter.next() {
                            None => return Err(ParseError::UnexpectedEndOfInput),
                            Some(t) => {
                                match t.kind {
                                    TokenKind::
                                }
                            },
                        }
                    },
                    */
                    _ => {
                        return Err(ParseError::UnexpectedToken);
                    },
                }
            },
        };
        break;
    };
    Ok(expr)
}

pub fn parse_expr(tokens: &[Token]) -> Result<Expr, ParseError> {
    let mut iter = tokens.iter();
    let mut expr = try!(parse_single_expr(&mut iter));

    loop {
        match iter.next() {
            None => return Ok(expr),
            Some(t) => {
                match t.kind {
                    TokenKind::Whitespace => continue,
                    TokenKind::Equals => {
                        match iter.next() {
                            None => return Err(ParseError::UnexpectedEndOfInput),
                            Some(t) => {
                                match t.kind {
                                    TokenKind::GreaterThan => {
                                        let rhs = try!(parse_single_expr(&mut iter));
                                        let lhs = try!(reparse_pattern_from_expr(&expr));
                                        let span = Span {
                                            start: lhs.span.start,
                                            end: rhs.span.end,
                                        };
                                        let func_term = FuncTerm {
                                            pattern: Box::new(lhs),
                                            body: Box::new(rhs),
                                            span: span,
                                        };
                                        let kind = ExprKind::FuncTerm(Box::new(func_term));
                                        expr = Expr {
                                            kind: kind,
                                            span: span,
                                        };
                                    },
                                    _ => {
                                        return Err(ParseError::UnexpectedToken);
                                    },
                                }
                            },
                        }
                    },
                    _ => return Err(ParseError::UnexpectedToken),
                }
            }
        }
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
        ExprKind::Variable(ref ident) => {
            pprint_ident(ident, f)
        },
        ExprKind::FuncTerm(ref func_term) => {
            pprint_func_term(func_term, f)
        }
    }
}

