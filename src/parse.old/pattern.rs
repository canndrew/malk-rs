use std::fmt;
use lexer::Span;
use parse::{ParseError, Ident, Expr, ExprKind};

#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    pub kind: PatternKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind {
    Parens(Box<Pattern>),
    Bind(Ident),
    Typed(Box<TypedPattern>),
    Struct(Box<StructPattern>),
    Equality(Box<EqualityPattern>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedPattern {
    pub sub_pattern: Pattern,
    pub pattern_type: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructPattern {
    pub head_elems: Vec<CompositePatternElem>,
    pub tail: Option<Pattern>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompositePatternElem {
    pub name: Option<Ident>,
    pub sub_pattern: Pattern,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EqualityPattern {
    pub a: Expr,
    pub b: Expr,
}

pub fn reparse_pattern_from_expr(expr: &Expr) -> Result<Pattern, ParseError> {
    println!("reparse_pattern_from_expr({})", expr);
    let kind = match expr.kind {
        ExprKind::Parens(ref sub_expr) => {
            let pattern = reparse_pattern_from_expr(sub_expr)?;
            PatternKind::Parens(Box::new(pattern))
        },
        ExprKind::Variable(ref ident) => {
            PatternKind::Bind(ident.clone())
        },
        ExprKind::Typed(ref typed) => {
            PatternKind::Typed(Box::new(TypedPattern {
                sub_pattern: reparse_pattern_from_expr(&typed.typed_term)?,
                pattern_type: typed.typed_type.clone(),
            }))
        },
        ExprKind::StructTerm(ref struct_term) => {
            let mut head_elems = Vec::with_capacity(struct_term.head_elems.len());
            for elem in &struct_term.head_elems {
                head_elems.push(CompositePatternElem {
                    name: elem.ident.clone(),
                    sub_pattern: reparse_pattern_from_expr(&elem.expr)?,
                    span: elem.span,
                });
            }
            let tail = match struct_term.tail {
                Some(ref tail_term) => Some(reparse_pattern_from_expr(tail_term)?),
                None => None,
            };
            PatternKind::Struct(Box::new(StructPattern {
                head_elems: head_elems,
                tail: tail,
            }))
        },
        _ => return Err(ParseError::InvalidPattern),
    };
    Ok(Pattern {
        kind: kind,
        span: expr.span,
    })
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            PatternKind::Parens(ref sub_pattern) => {
                write!(f, "({})", sub_pattern)
            },
            PatternKind::Bind(ref ident) => {
                write!(f, "{}", ident)
            }
            PatternKind::Typed(ref typed_pattern) => {
                write!(f, "{}: {}", typed_pattern.sub_pattern, typed_pattern.pattern_type)
            },
            PatternKind::Struct(ref struct_pattern) => {
                write!(f, "{{")?;
                if let Some((first, rest)) = struct_pattern.head_elems.split_first() {
                    write!(f, "{}", first)?;
                    for elem in rest {
                        write!(f, ", {}", elem)?;
                    }
                }
                if let Some(ref tail_pattern) = struct_pattern.tail {
                    write!(f, "; {}", tail_pattern)?;
                }
                write!(f, "}}")
            },
            PatternKind::Equality(ref equality_pattern) => {
                write!(f, "{} == {}", equality_pattern.a, equality_pattern.b)
            },
        }
    }
}

impl fmt::Display for CompositePatternElem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref name) = self.name {
            write!(f, "{} = ", name)?;
        }
        write!(f, "{}", self.sub_pattern)
    }
}

