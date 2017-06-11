pub use self::ident::{Ident, parse_ident};
pub use self::struct_type::{StructType};
pub use self::enum_type::{EnumType};
pub use self::struct_term::{StructTerm, parse_struct_term_inner};
pub use self::enum_term::{EnumTerm, EnumTermKind, parse_enum_term_inner};
pub use self::composite_type::parse_composite_type_inner;
pub use self::composite_type_elem::{CompositeTypeElem, parse_maybe_composite_type_elem,
                                 parse_composite_type_elem};
pub use self::composite_term_elem::{CompositeTermElem, parse_maybe_composite_term_elem,
                                    parse_composite_term_elem};
pub use self::expr::{Expr, ExprKind, parse_expr};
pub use self::func_term::{FuncTerm, SingularFuncTerm, EnumFuncTerm, EnumFuncTermLeft};
pub use self::func_type::{FuncType, SingularFuncType, EnumFuncType, EnumFuncTypeLeft};
pub use self::func_app::{FuncApp};
pub use self::typed::{Typed};
pub use self::type_literal::TypeLiteral;
pub use self::level_literal::LevelLiteral;
pub use self::let_expr::{LetExpr, parse_let_expr};
pub use self::pattern::*;
pub use self::parens::Parens;

use lexer::TextPos;

mod parens;
mod ident;
mod composite_term_elem;
mod struct_term;
mod composite_type_elem;
mod composite_type;
mod struct_type;
mod enum_type;
mod enum_term;
mod func_term;
mod func_type;
mod func_app;
mod typed;
mod type_literal;
mod level_literal;
mod let_expr;
mod pattern;
mod expr;

quick_error! {
    #[derive(Debug)]
    pub enum ParseError {
        ExpectedExpression {
            pos: TextPos,
        } {
            description("Expected expression")
        }
        ExpectedToken {
            pos: TextPos,
        } {
            description("Expected token")
        }
        UnexpectedToken {
            pos: TextPos,
        } {
            description("Unexpected token")
        }
        ExpectedIdent {
            pos: TextPos,
        } {
            description("Expected ident")
        }
        ExpectedEnumContents {
            description("Expected enum contents")
        }
        InvalidVariableBind {
            description("Invalid variable bind")
        }
        InvalidPattern {
            description("Invalid pattern")
        }
    }
}

pub const SYMBOL_TABLE: &'static [&'static str] = &[
    ":", ";", ",", "=", "=>", "#", "->", "+",
];

/*
#[cfg(test)]
mod test {
    use super::*;

    use lexer::lex;

    #[test]
    fn unit_term() {
        let srcs = &["{}", " { } "];
        for src in srcs {
            let ts = lex(src, SYMBOL_TABLE).unwrap();
            let expr = parse_expr(ts.borrow()).unwrap();

            let st = match expr.kind {
                ExprKind::StructTerm(st) => *st,
                _ => panic!(),
            };
            assert_eq!(st.head, None);
            assert_eq!(st.head_elems[..], []);
        }
    }

    /*
    #[test]
    fn struct_term() {
        let srcs = &["{x, y = 23}", "{x  , y=23,}", "{{}; x, y = 23}", "{{x,}; y =23, }"];
        for src in srcs {
            let ts = lex(src, SYMBOL_TABLE).unwrap();
            let expr = parse_expr(ts.borrow()).unwrap();

            let st = match expr.kind {
                ExprKind::StructTerm(st) => *st,
                _ => panic!(),
            };
            assert_
        }
    }
    */
}
*/

