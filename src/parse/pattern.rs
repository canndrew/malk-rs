use std::fmt;
use lexer::Span;
use parse::{ParseError,
            Ident, pprint_ident,
            Expr, ExprKind, pprint_expr};

pub struct Pattern {
    pub kind: PatternKind,
    pub span: Span,
}

pub enum PatternKind {
    Bind(BindPattern),
}

pub struct BindPattern {
    pub ident: Ident,
    pub pat_ty: Option<Box<Expr>>,
    pub sub_pat: Option<Box<Pattern>>,
    pub span: Span,
}

pub fn reparse_pattern_from_expr(expr: &Expr) -> Result<Pattern, ParseError> {
    let kind = match expr.kind {
        ExprKind::Parens(ref sub_expr) => {
            match sub_expr.kind {
                ExprKind::Variable(ref ident) => {
                    PatternKind::Bind(BindPattern {
                        ident: (*ident).clone(),
                        pat_ty: None,
                        sub_pat: None,
                        span: sub_expr.span,
                    })
                },
                _ => return Err(ParseError::InvalidVariableBind),
            }
        },
        _ => return Err(ParseError::InvalidPattern),
    };
    Ok(Pattern {
        kind: kind,
        span: expr.span,
    })
}

pub fn pprint_pattern(pattern: &Pattern, f: &mut fmt::Formatter) -> fmt::Result {
    match pattern.kind {
        PatternKind::Bind(ref bind_pattern) => {
            try!(pprint_ident(&bind_pattern.ident, f));
            if let Some(ref pat_ty) = bind_pattern.pat_ty {
                try!(write!(f, ": "));
                try!(pprint_expr(pat_ty, f));
            }
            if let Some(ref sub_pat) = bind_pattern.sub_pat {
                try!(write!(f, " = "));
                try!(pprint_pattern(sub_pat, f));
            }
        },
    }
    Ok(())
}

