use std::fmt;
use lexer::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeLiteral {
    pub rank: usize,
    pub span: Span,
}

impl fmt::Display for TypeLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Type")?;
        for _ in 0..self.rank {
            write!(f, "+")?;
        }
        Ok(())
    }
}

