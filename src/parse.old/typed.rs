use std::fmt;

use lexer::Span;
use parse::{Expr};

#[derive(Debug, Clone, PartialEq)]
pub struct Typed {
    pub typed_term: Expr,
    pub typed_type: Expr,
    pub span: Span,
}

impl fmt::Display for Typed {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.typed_term, self.typed_type)
    }
}

