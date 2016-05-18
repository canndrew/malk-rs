use std::fmt;

use lexer::{Span, Token, TokenKind};
use parse::ParseError;

pub struct Ident {
    pub text: String,
    pub span: Span,
}

pub fn parse_ident(tokens: &[Token]) -> Result<Ident, ParseError> {
    debug!("parse_ident({:?})", tokens);
    let mut iter = tokens.iter().filter(|t| !t.is_whitespace());
    if let (Some(token), None) = (iter.next(), iter.next()) {
        if let TokenKind::Word(ref s) = token.kind {
            let ident = Ident {
                text: s.clone(),
                span: token.span,
            };
            return Ok(ident)
        }
    }
    Err(ParseError::ExpectedIdent)
}

pub fn pprint_ident(ident: &Ident, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", ident.text)
}

