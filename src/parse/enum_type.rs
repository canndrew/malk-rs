use std::fmt;

use lexer::Span;
use parse::{CompositeTypeElem, pprint_composite_type_elem,
            Expr, pprint_expr};

pub struct EnumType {
    pub head_elems: Vec<CompositeTypeElem>,
    pub tail: Option<Expr>,
    pub span: Span,
}

pub fn pprint_enum_type(enum_type: &EnumType, f: &mut fmt::Formatter) -> fmt::Result {
    try!(write!(f, "#["));
    if let Some((first, rest)) = enum_type.head_elems[..].split_first() {
        try!(pprint_composite_type_elem(first, f));
        for elem in rest {
            try!(write!(f, ", "));
            try!(pprint_composite_type_elem(elem, f));
        }
    }
    if let Some(ref tail) = enum_type.tail {
        try!(write!(f, "; "));
        try!(pprint_expr(tail, f));
    }
    write!(f, "]")
}


