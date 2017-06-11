use lexer::Span;
use parse::Expr;

pub struct Parens {
    pub expr: Expr,
    pub span: Span,
}

