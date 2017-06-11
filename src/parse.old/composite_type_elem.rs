use std::fmt;

use lexer::{Span, TokenKind, TokensRef};
use parse::{ParseError, Ident, parse_ident,
                        Expr, parse_expr};

#[derive(Debug, Clone, PartialEq)]
pub struct CompositeTypeElem {
    pub ident: Option<Ident>,
    pub expr: Expr,
    pub span: Span,
}

pub fn parse_maybe_composite_type_elem<'t, 's: 't>(ts: TokensRef<'t, 's>) -> Result<Option<CompositeTypeElem>, ParseError> {
    println!("parse_maybe_composite_type_elem({:?})", ts);
    if ts.tokens.iter().all(|t| t.is_whitespace()) {
        return Ok(None)
    }

    let composite_type_elem = parse_composite_type_elem(ts)?;
    Ok(Some(composite_type_elem))
}

pub fn parse_composite_type_elem<'t, 's: 't>(ts: TokensRef<'t, 's>) -> Result<CompositeTypeElem, ParseError> {
    println!("parse_composite_type_elem({:?})", ts);
    for (i, token) in ts.tokens.iter().enumerate() {
        // See if there's a ':' in there.
        match token.kind {
            TokenKind::Symbol(":") => {
                let (ident_tokens, expr_tokens) = ts.split_around(i);

                let ident = parse_ident(ident_tokens)?;
                let expr = parse_expr(expr_tokens)?;
                let span = Span {
                    start: ident.span.start,
                    end: expr.span.end,
                };
                return Ok(CompositeTypeElem {
                    ident: Some(ident),
                    expr: expr,
                    span: span,
                });
            },
            _ => (),
        }
    }
    // Didn't find an ':', must just be an expression
    let expr = parse_expr(ts)?;
    let span = expr.span;
    return Ok(CompositeTypeElem {
        ident: None,
        expr: expr,
        span: span,
    });
}

impl fmt::Display for CompositeTypeElem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref ident) = self.ident {
            write!(f, "{}: ", ident)?;
        }
        write!(f, "{}", self.expr)
    }
}

