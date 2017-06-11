use lexer::{TokenKind, TokensRef};
use parse::{ParseError, CompositeTypeElem, parse_maybe_composite_type_elem,
                        parse_composite_type_elem,
                        Expr, parse_expr};

pub fn parse_composite_type_inner<'t, 's: 't>(ts: TokensRef<'t, 's>) -> Result<(Vec<CompositeTypeElem>, Option<Expr>), ParseError> {
    println!("parse_composite_type_inner({:?})", ts);
    let mut head_elems = Vec::new();
    let mut unread = ts;
    'chunk: loop {
        for (i, token) in unread.tokens.iter().enumerate() {
            match token.kind {
                TokenKind::Symbol(",") => {
                    let (head_tokens, tail_tokens) = unread.split_around(i);
                    let head = parse_composite_type_elem(head_tokens)?;
                    head_elems.push(head);
                    unread = tail_tokens;
                    continue 'chunk;
                },
                TokenKind::Symbol(";") => {
                    let (head_tokens, tail_tokens) = unread.split_around(i);
                    let head = parse_maybe_composite_type_elem(head_tokens)?;
                    head_elems.extend(head);
                    let tail = parse_expr(tail_tokens)?;
                    return Ok((head_elems, Some(tail)));
                },
                _ => (),
            }
        }
        break;
    }
    // Didn't find a semicolon. There is no tail elem.
    let head = parse_maybe_composite_type_elem(unread)?;
    head_elems.extend(head);
    Ok((head_elems, None))
}

