use std::fmt;
use lexer::Span;
use parse::Expr;

#[derive(Debug, Clone, PartialEq)]
pub struct FuncApp {
    pub func: Expr,
    pub arg: Expr,
    pub span: Span,
}

impl fmt::Display for FuncApp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.func, self.arg)
    }
}

