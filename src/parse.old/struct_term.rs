use std::fmt;

use lexer::{Span, TokenKind, TokensRef};
use parse::{ParseError, CompositeTermElem, parse_maybe_composite_term_elem, parse_composite_term_elem,
                        Expr, parse_expr};

#[derive(Debug, Clone, PartialEq)]
pub struct StructTerm {
    pub head_elems: Vec<CompositeTermElem>,
    pub tail: Option<Expr>,
    pub span: Span,
}

pub fn parse_struct_term_inner<'t, 's: 't>(ts: TokensRef<'t, 's>) -> Result<(Vec<CompositeTermElem>, Option<Expr>), ParseError> {
    println!("parse_struct_term_inner({:?})", ts);
    let mut head_elems = Vec::new();
    let mut elem_start = 0;
    for (i, token) in ts.tokens.iter().enumerate() {
        match token.kind {
            TokenKind::Symbol(",") => {
                let head_tokens = ts.range(elem_start, i);
                let head = parse_composite_term_elem(head_tokens)?;
                head_elems.push(head);
                elem_start = i + 1;
            },
            TokenKind::Symbol(";") => {
                let head_tokens = ts.range(elem_start, i);
                let head = parse_maybe_composite_term_elem(head_tokens)?;
                head_elems.extend(head);

                let tail_tokens = ts.range_from(i + 1);
                let tail = parse_expr(tail_tokens)?;
                return Ok((head_elems, Some(tail)));
            },
            _ => (),
        }
    }
    // Didn't find a semicolon. There is no tail elem.
    let head_tokens = ts.range_from(elem_start);
    let head = parse_maybe_composite_term_elem(head_tokens)?;
    head_elems.extend(head);
    Ok((head_elems, None))
}

impl fmt::Display for StructTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{")?;
        if let Some((first, rest)) = self.head_elems.split_first() {
            write!(f, "{}", first)?;
            for elem in rest {
                write!(f, ", {}", elem)?;
            }
        }
        if let Some(ref tail) = self.tail {
            write!(f, "; {}", tail)?;
        }
        write!(f, "}}")
    }
}

