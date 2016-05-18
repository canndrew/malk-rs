use lexer::{Token, TokenKind};
use parse::{ParseError, CompositeTypeElem, parse_maybe_composite_type_elem,
                        parse_composite_type_elem,
                        Expr, parse_expr};

pub fn parse_composite_type_inner(tokens: &[Token]) -> Result<(Vec<CompositeTypeElem>, Option<Expr>), ParseError> {
    println!("parse_composite_type_inner({:?})", tokens);
    for (i, token) in tokens.iter().enumerate() {
        // Find the first ',' or ';'
        match token.kind {
            // Found comma first `#{ head, ... }`
            TokenKind::Comma => {
                let head_tokens = &tokens[..i];
                let tail_tokens = &tokens[(i + 1)..];

                let head_elem = try!(parse_composite_type_elem(head_tokens));
                let (sub_head_elems, tail) = try!(parse_composite_type_inner(tail_tokens));
                let mut head_elems = vec![head_elem];
                head_elems.extend(sub_head_elems);
                return Ok((head_elems, tail))
            },
            // Found semicolon first: `#{ head ; ...}` or `#{; ...}`
            TokenKind::Semicolon => {
                let head_tokens = &tokens[..i];
                let tail_tokens = &tokens[(i + 1)..];

                let head_elem = try!(parse_maybe_composite_type_elem(head_tokens));
                let tail = try!(parse_expr(tail_tokens));
                return Ok((head_elem.into_iter().collect(), Some(tail)));
            },
            // Ignore anything else
            _ => (),
        }
    }
    // Found neither ',' or ';'. Must be of the form `#{ head }` or `#{ }`
    let head_elem = try!(parse_maybe_composite_type_elem(tokens));
    Ok((head_elem.into_iter().collect(), None))
}
