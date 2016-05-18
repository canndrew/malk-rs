use std::fmt;

use lexer::{Span, Token, TokenKind};
use parse::{ParseError, StructTypeElem, parse_maybe_struct_type_elem, parse_struct_type_elem,
                        pprint_struct_type_elem,
                        Expr, parse_expr, pprint_expr};

pub struct StructType {
    pub head_elems: Vec<StructTypeElem>,
    pub tail: Option<Expr>,
    pub span: Span,
}

pub fn parse_struct_type_inner(tokens: &[Token]) -> Result<(Vec<StructTypeElem>, Option<Expr>), ParseError> {
    println!("parse_struct_type_inner({:?})", tokens);
    for (i, token) in tokens.iter().enumerate() {
        // Find the first ',' or ';'
        match token.kind {
            // Found comma first `#{ head, ... }`
            TokenKind::Comma => {
                let head_tokens = &tokens[..i];
                let tail_tokens = &tokens[(i + 1)..];

                let head_elem = try!(parse_struct_type_elem(head_tokens));
                let (sub_head_elems, tail) = try!(parse_struct_type_inner(tail_tokens));
                let mut head_elems = vec![head_elem];
                head_elems.extend(sub_head_elems);
                return Ok((head_elems, tail))
            },
            // Found semicolon first: `#{ head ; ...}` or `#{; ...}`
            TokenKind::Semicolon => {
                let head_tokens = &tokens[..i];
                let tail_tokens = &tokens[(i + 1)..];

                let head_elem = try!(parse_maybe_struct_type_elem(head_tokens));
                let tail = try!(parse_expr(tail_tokens));
                return Ok((head_elem.into_iter().collect(), Some(tail)));
            },
            // Ignore anything else
            _ => (),
        }
    }
    // Found neither ',' or ';'. Must be of the form `#{ head }` or `#{ }`
    let head_elem = try!(parse_maybe_struct_type_elem(tokens));
    Ok((head_elem.into_iter().collect(), None))
}

pub fn pprint_struct_type(struct_type: &StructType, f: &mut fmt::Formatter) -> fmt::Result {
    try!(write!(f, "#{{"));
    if let Some((first, rest)) = struct_type.head_elems[..].split_first() {
        try!(pprint_struct_type_elem(first, f));
        for elem in rest {
            try!(write!(f, ", "));
            try!(pprint_struct_type_elem(elem, f));
        }
    }
    if let Some(ref tail) = struct_type.tail {
        try!(write!(f, "; "));
        try!(pprint_expr(tail, f));
    }
    write!(f, "}}")
}

