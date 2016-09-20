use std::fmt;
use lexer::Span;
use parse::{Expr, pprint_expr,
            Pattern, pprint_pattern};

pub struct FuncTerm {
    pub pattern: Box<Pattern>,
    pub body: Box<Expr>,
    pub span: Span,
}

pub fn pprint_func_term(func_term: &FuncTerm, f: &mut fmt::Formatter) -> fmt::Result {
    try!(pprint_pattern(&*func_term.pattern, f));
    try!(write!(f, " => "));
    try!(pprint_expr(&*func_term.body, f));
    Ok(())
}

