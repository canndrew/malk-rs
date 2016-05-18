use std::fmt;

use lexer::{Span, Token, TokenKind};
use parse::{ParseError, CompositeTermElem, parse_maybe_composite_term_elem, parse_composite_term_elem,
                        pprint_composite_term_elem,
                        Expr, parse_expr, pprint_expr};

pub struct StructTerm {
    pub head_elems: Vec<CompositeTermElem>,
    pub tail: Option<Expr>,
    pub span: Span,
}

pub fn parse_struct_term_inner(tokens: &[Token]) -> Result<(Vec<CompositeTermElem>, Option<Expr>), ParseError> {
    println!("parse_struct_term_inner({:?})", tokens);
    for (i, token) in tokens.iter().enumerate() {
        // Find the first ',' or ';'
        match token.kind {
            // Found comma first `{ head, ... }`
            TokenKind::Comma => {
                let head_tokens = &tokens[..i];
                let tail_tokens = &tokens[(i + 1)..];

                let head_elem = try!(parse_composite_term_elem(head_tokens));
                let (sub_head_elems, tail) = try!(parse_struct_term_inner(tail_tokens));
                let mut head_elems = vec![head_elem];
                head_elems.extend(sub_head_elems);
                return Ok((head_elems, tail))
            },
            // Found semicolon first: `{ head ; ...}` or `{; ...}`
            TokenKind::Semicolon => {
                let head_tokens = &tokens[..i];
                let tail_tokens = &tokens[(i + 1)..];

                let head_elem = try!(parse_maybe_composite_term_elem(head_tokens));
                let tail = try!(parse_expr(tail_tokens));
                return Ok((head_elem.into_iter().collect(), Some(tail)));
            },
            // Ignore anything else
            _ => (),
        }
    }
    // Found neither ',' or ';'. Must be of the form `{ head }` or `{ }`
    let head_elem = try!(parse_maybe_composite_term_elem(tokens));
    Ok((head_elem.into_iter().collect(), None))
}

pub fn pprint_struct_term(struct_term: &StructTerm, f: &mut fmt::Formatter) -> fmt::Result {
    try!(write!(f, "{{"));
    if let Some((first, rest)) = struct_term.head_elems[..].split_first() {
        try!(pprint_composite_term_elem(first, f));
        for elem in rest {
            try!(write!(f, ", "));
            try!(pprint_composite_term_elem(elem, f));
        }
    }
    if let Some(ref tail) = struct_term.tail {
        try!(write!(f, "; "));
        try!(pprint_expr(tail, f));
    }
    write!(f, "}}")
}

