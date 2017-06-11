use std::fmt;
use parse::Ident;
use core::Term;

pub use self::ctx::{CtxExt, CtxExtKind};
pub use self::pattern::{build_pattern};
pub use self::expr::*;
pub use self::let_expr::{build_let_expr};
pub use self::ident::{build_ident, build_opt_ident};
pub use self::struct_term::{build_struct_term, /*build_struct_term_typed*/ };
pub use self::struct_type::{build_struct_type, /*build_struct_type_typed*/ };
pub use self::enum_term::{build_enum_term, /*build_struct_type_typed*/ };
pub use self::enum_type::{build_enum_type, /*build_struct_type_typed*/ };
pub use self::func_term::{build_singular_func_term, build_enum_func_term, /*build_func_term_typed*/};
pub use self::func_app::build_func_app;

pub mod ctx;
pub mod pattern;
pub mod expr;
pub mod let_expr;
pub mod ident;
pub mod struct_term;
pub mod struct_type;
pub mod enum_term;
pub mod enum_type;
pub mod func_term;
pub mod func_app;

#[derive(Debug)]
pub enum ConstructError {
    NoSuchVariable(Ident),
}

impl fmt::Display for ConstructError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug)]
pub struct TypedTerm {
    term: Term,
    ty: Term,
}
