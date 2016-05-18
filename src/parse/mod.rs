pub use self::ident::{Ident, parse_ident, pprint_ident};
pub use self::struct_term_elem::{StructTermElem, parse_maybe_struct_term_elem,
                                 parse_struct_term_elem, pprint_struct_term_elem};
pub use self::struct_term::{StructTerm, parse_struct_term_inner, pprint_struct_term};
pub use self::struct_type_elem::{StructTypeElem, parse_maybe_struct_type_elem,
                                 parse_struct_type_elem, pprint_struct_type_elem};
pub use self::struct_type::{StructType, parse_struct_type_inner, pprint_struct_type};
pub use self::expr::{Expr, ExprKind, parse_expr, pprint_expr};

mod ident;
mod struct_term_elem;
mod struct_term;
mod struct_type_elem;
mod struct_type;
mod expr;

quick_error! {
    #[derive(Debug)]
    pub enum ParseError {
        UnexpectedEndOfInput {
            description("Unexpected end of input")
        }
        UnexpectedToken {
            description("Unexpected token")
        }
        ExpectedIdent {
            description("Expected ident")
        }
    }
}

