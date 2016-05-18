use std::fmt;

use lexer::{Span, Token, TokenKind};
use parse::{ParseError,
            Expr, parse_expr, pprint_expr,
            CompositeTermElem, parse_composite_term_elem, pprint_composite_term_elem};

pub struct EnumTerm {
    pub kind: EnumTermKind,
    pub span: Span,
}

pub enum EnumTermKind {
    Head(CompositeTermElem),
    Tail(Expr),
}

pub fn parse_enum_term_inner(tokens: &[Token]) -> Result<EnumTermKind, ParseError> {
    debug!("parse_enum_term_inner({:?})", tokens);
    match tokens.iter().skip_while(|t| t.is_whitespace()).enumerate().next() {
        None => return Err(ParseError::ExpectedEnumContents),
        Some((i, t)) => {
            match t.kind {
                TokenKind::Semicolon => {
                    let expr = try!(parse_expr(&tokens[(i + 1)..]));
                    return Ok(EnumTermKind::Tail(expr));
                },
                _ => (),
            }
        }
    }

    let elem = try!(parse_composite_term_elem(tokens));
    Ok(EnumTermKind::Head(elem))
}

pub fn pprint_enum_term(enum_term: &EnumTerm, f: &mut fmt::Formatter) -> fmt::Result {
    try!(write!(f, "["));
    match enum_term.kind {
        EnumTermKind::Head(ref head) => {
            try!(pprint_composite_term_elem(head, f));
        },
        EnumTermKind::Tail(ref tail) => {
            try!(write!(f, "; "));
            try!(pprint_expr(tail, f));
        },
    }
    write!(f, "]")
}

