use std::fmt;

use lexer::Span;
use parse::{CompositeTypeElem, pprint_composite_type_elem,
            Expr, pprint_expr};

pub struct StructType {
    pub head_elems: Vec<CompositeTypeElem>,
    pub tail: Option<Expr>,
    pub span: Span,
}

pub fn pprint_struct_type(struct_type: &StructType, f: &mut fmt::Formatter) -> fmt::Result {
    try!(write!(f, "#{{"));
    if let Some((first, rest)) = struct_type.head_elems[..].split_first() {
        try!(pprint_composite_type_elem(first, f));
        for elem in rest {
            try!(write!(f, ", "));
            try!(pprint_composite_type_elem(elem, f));
        }
    }
    if let Some(ref tail) = struct_type.tail {
        try!(write!(f, "; "));
        try!(pprint_expr(tail, f));
    }
    write!(f, "}}")
}

