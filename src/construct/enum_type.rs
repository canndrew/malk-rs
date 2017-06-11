use core::{Origin, Ctx, Term, TermKind};
use parse::{Syntax, Span, EnumType, CompositeTypeElem, Expr, OperatorPrecedenceExprKind};
use construct::{build_operator_expr, build_opt_ident, ConstructError};

pub fn build_enum_type(ctx: &Ctx, enum_type: &mut EnumType) -> Result<Term, ConstructError> {
    fn build(ctx: &Ctx, head_elems: &mut [CompositeTypeElem], tail: &mut Option<Expr<OperatorPrecedenceExprKind>>, end_pos: usize) -> Result<Term, ConstructError> {
        match head_elems.split_first_mut() {
            None => match *tail {
                None => {
                    let kind = TermKind::UnitType;
                    let span = Span {
                        start: end_pos,
                        end: end_pos,
                    };
                    Ok(Term::new(kind, Origin::ExprAt(span)))
                },
                Some(ref mut t) => build_operator_expr(ctx, t),
            },
            Some((first, rest)) => {
                let head_name = build_opt_ident(&mut first.ident);
                let head_type = build_operator_expr(ctx, &mut first.expr)?;
                let tail_type = build(ctx, rest, tail, end_pos)?;
                let kind = TermKind::PairType {
                    head_name: head_name,
                    head_type: head_type,
                    tail_type: tail_type,
                };
                let span = Span {
                    start: first.span().start,
                    end: end_pos,
                };
                Ok(Term::new(kind, Origin::ExprAt(span)))
            },
        }
    }

    let end_pos = enum_type.span().end;
    build(ctx, &mut enum_type.head_elems[..], &mut enum_type.tail, end_pos)
}

