use std::fmt;

use lexer::{Span, TokenKind, TokensRef};
use parse::{ParseError,
            Expr, parse_expr,
            CompositeTermElem, parse_composite_term_elem};

#[derive(Debug, Clone, PartialEq)]
pub struct EnumTerm {
    pub kind: EnumTermKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumTermKind {
    Head(CompositeTermElem),
    Tail(Expr),
}

pub fn parse_enum_term_inner<'t, 's: 't>(ts: TokensRef<'t, 's>) -> Result<EnumTermKind, ParseError> {
    debug!("parse_enum_term_inner({:?})", ts);
    for (i, token) in ts.tokens.iter().enumerate() {
        match token.kind {
            TokenKind::Symbol(";") => {
                let (before, after) = ts.split_around(i);
                for t in before.tokens.iter() {
                    if !t.is_whitespace() {
                        return Err(ParseError::UnexpectedToken { pos: t.start });
                    }
                }
                let expr = parse_expr(after)?;
                return Ok(EnumTermKind::Tail(expr));
            },
            _ => (),
        }
    }

    let elem = parse_composite_term_elem(ts)?;
    Ok(EnumTermKind::Head(elem))
}

impl fmt::Display for EnumTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            EnumTermKind::Head(ref head) => {
                write!(f, "[{}]", head)
            },
            EnumTermKind::Tail(ref tail) => {
                write!(f, "[; {}]", tail)
            },
        }
    }
}

