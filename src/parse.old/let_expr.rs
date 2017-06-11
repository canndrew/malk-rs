use std::fmt;

use lexer::{TokenKind, TokensRef};
use parse::{ParseError, Pattern, Expr, parse_expr, reparse_pattern_from_expr};

#[derive(Debug, Clone, PartialEq)]
pub struct LetExpr {
    pub pattern: Pattern,
    pub let_to: Expr,
    pub let_in: Expr,
}

pub fn parse_let_expr<'t, 's: 't>(tr: TokensRef<'t, 's>) -> Result<LetExpr, ParseError> {
    for (i, token) in tr.tokens.iter().enumerate() {
        if let TokenKind::Symbol("=") = token.kind {
            let (pattern_tokens, rest) = tr.split_around(i);
            for (i, token) in rest.tokens.iter().enumerate() {
                if let TokenKind::Symbol(";") = token.kind {
                    let (to_tokens, in_tokens) = rest.split_around(i);
                    let pattern_expr = parse_expr(pattern_tokens)?;
                    let pattern = reparse_pattern_from_expr(&pattern_expr)?;
                    let let_to = parse_expr(to_tokens)?;
                    let let_in = parse_expr(in_tokens)?;
                    return Ok(LetExpr {
                        pattern: pattern,
                        let_to: let_to,
                        let_in: let_in,
                    });
                }
            }
            return Err(ParseError::ExpectedToken {
                pos: tr.end,
            });
        }
    };
    Err(ParseError::ExpectedToken {
        pos: tr.end,
    })
}

impl fmt::Display for LetExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "let {} = {}; {}", self.pattern, self.let_to, self.let_in)
    }
}

