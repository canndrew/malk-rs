use std::fmt;

use lexer::Span;
use parse::{CompositeTypeElem, Expr};

#[derive(Debug, Clone, PartialEq)]
pub struct EnumType {
    pub head_elems: Vec<CompositeTypeElem>,
    pub tail: Option<Expr>,
    pub span: Span,
}


impl fmt::Display for EnumType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#[")?;
        if let Some((first, rest)) = self.head_elems[..].split_first() {
            write!(f, "{}", first)?;
            for elem in rest {
                write!(f, ", {}", elem)?;
            }
        }
        if let Some(ref tail) = self.tail {
            write!(f, "; {}", tail)?;
        }
        write!(f, "]")
    }
}

