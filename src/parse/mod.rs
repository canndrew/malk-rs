pub use self::ident::{Ident, parse_ident, pprint_ident};
pub use self::struct_type::{StructType, pprint_struct_type};
pub use self::enum_type::{EnumType, pprint_enum_type};
pub use self::struct_term::{StructTerm, parse_struct_term_inner, pprint_struct_term};
pub use self::composite_type::parse_composite_type_inner;
pub use self::composite_type_elem::{CompositeTypeElem, parse_maybe_composite_type_elem,
                                 parse_composite_type_elem, pprint_composite_type_elem};
pub use self::composite_term_elem::{CompositeTermElem, parse_maybe_composite_term_elem,
                                    parse_composite_term_elem, pprint_composite_term_elem};
pub use self::expr::{Expr, ExprKind, parse_expr, pprint_expr};

mod ident;
mod composite_term_elem;
mod struct_term;
mod composite_type_elem;
mod composite_type;
mod struct_type;
mod enum_type;
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

