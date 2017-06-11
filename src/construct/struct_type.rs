use core::{Origin, Ctx, Term, TermKind};
use parse::{Syntax, Span, StructType, CompositeTypeElem, Expr, OperatorPrecedenceExprKind};
use construct::{build_operator_expr, build_opt_ident, ConstructError};

pub fn build_struct_type(ctx: &Ctx, struct_type: &mut StructType) -> Result<Term, ConstructError> {
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
                let sub_ctx = Ctx::cons(head_name.clone(), head_type.clone(), ctx.clone());
                let tail_type = build(&sub_ctx, rest, tail, end_pos)?;
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

    let end_pos = struct_type.span().end;
    build(ctx, &mut struct_type.head_elems[..], &mut struct_type.tail, end_pos)
}


