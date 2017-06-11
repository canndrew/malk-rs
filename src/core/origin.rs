use std::rc::Rc;

use parse::Span;

#[derive(Debug, Clone, Hash)]
pub enum Origin {
    ExprAt(Span),
    PatternAt(Span),
    TypeOf(Rc<Origin>),
    ElimOf(Rc<Origin>),
    Inferred(Span),
}

