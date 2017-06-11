use std::fmt;
use lexer::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct LevelLiteral {
    pub rank: usize,
    pub span: Span,
}

impl fmt::Display for LevelLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Level")?;
        for _ in 0..self.rank {
            write!(f, "+")?;
        }
        Ok(())
    }
}


