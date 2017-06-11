use parse::{EnumTerm, EnumTermKind};
use core::{Ctx, Term, TermKind, Origin};
use construct::{build_opt_ident, build_operator_expr, ConstructError};


/*
fn wrap_in_rights(term: Term) -> Term {
    // TODO: a way to do this would be to use a recursive lambda and nat to wrap
    // the term with left, n times.
}
*/

pub fn build_enum_term(ctx: &Ctx, enum_term: &mut EnumTerm) -> Result<Term, ConstructError> {
    match enum_term.kind {
        EnumTermKind::Left(ref mut left) => {
            let left_name = build_opt_ident(&left.ident);
            let left = build_operator_expr(ctx, &mut left.expr)?;
            let kind = TermKind::EitherLeft {
                left_name: left_name,
                left: left,
            };
            Ok(Term::new(kind, Origin::ExprAt(enum_term.span)))
        },
        EnumTermKind::Right(ref mut right) => {
            let right = build_operator_expr(ctx, right)?;
            let kind = TermKind::EitherRight {
                right: right,
            };
            Ok(Term::new(kind, Origin::ExprAt(enum_term.span)))
        },
    }
}

