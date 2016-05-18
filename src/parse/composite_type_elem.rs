use std::fmt;

use lexer::{Span, Token, TokenKind};
use parse::{ParseError, Ident, parse_ident, pprint_ident,
                        Expr, parse_expr, pprint_expr};

pub struct CompositeTypeElem {
    pub ident: Option<Ident>,
    pub expr: Expr,
    pub span: Span,
}

pub fn parse_maybe_composite_type_elem(tokens: &[Token]) -> Result<Option<CompositeTypeElem>, ParseError> {
    debug!("parse_maybe_composite_type_elem({:?})", tokens);
    if tokens.iter().all(|t| t.is_whitespace()) {
        return Ok(None)
    }

    let composite_type_elem = try!(parse_composite_type_elem(tokens));
    Ok(Some(composite_type_elem))
}

pub fn parse_composite_type_elem(tokens: &[Token]) -> Result<CompositeTypeElem, ParseError> {
    debug!("parse_composite_type_elem({:?})", tokens);
    for (i, token) in tokens.iter().enumerate() {
        // See if there's a ':' in there.
        match token.kind {
            TokenKind::Colon => {
                let ident_tokens = &tokens[..i];
                let expr_tokens = &tokens[(i + 1)..];

                let ident = try!(parse_ident(ident_tokens));
                let expr = try!(parse_expr(expr_tokens));
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
    let expr = try!(parse_expr(tokens));
    let span = expr.span;
    return Ok(CompositeTypeElem {
        ident: None,
        expr: expr,
        span: span,
    });
}

pub fn pprint_composite_type_elem(composite_type_elem: &CompositeTypeElem, f: &mut fmt::Formatter) -> fmt::Result {
    if let Some(ref ident) = composite_type_elem.ident {
        try!(pprint_ident(ident, f));
        try!(write!(f, ": "));
    };
    pprint_expr(&composite_type_elem.expr, f)
}

