use std::fmt;

use lexer::{Span, TokensRef, TokenKind};
use parse::ParseError;

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub text: String,
    pub span: Span,
}

pub fn parse_ident<'t, 's: 't>(ts: TokensRef<'t, 's>) -> Result<Ident, ParseError> {
    debug!("parse_ident({:?})", ts);
    let ts = ts.trim_whitespace();
    let mut iter = ts.tokens.iter();
    match (iter.next(), iter.next()) {
        (None, _) => return Err(ParseError::ExpectedIdent { pos: ts.end }),
        (Some(token), None) => {
            let text = match token.kind {
                TokenKind::Ident(s) => String::from(s),
                _ => return Err(ParseError::ExpectedIdent { pos: token.start }),
            };
            Ok(Ident {
                text: text,
                span: Span {
                    start: token.start,
                    end: ts.end,
                },
            })
        },
        (Some(_), Some(token)) => {
            return Err(ParseError::UnexpectedToken { pos: token.start });
        },
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.text)
    }
}

