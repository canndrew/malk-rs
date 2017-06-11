use core::{Origin, Ctx, Term, TermKind};
use parse::{Syntax, Span, StructTerm, CompositeTermElem, Expr, OperatorPrecedenceExprKind};
use construct::{build_operator_expr, build_opt_ident, ConstructError};

pub fn build_struct_term(ctx: &Ctx, struct_term: &mut StructTerm) -> Result<Term, ConstructError> {
    fn build(ctx: &Ctx, head_elems: &mut [CompositeTermElem], tail: &mut Option<Expr<OperatorPrecedenceExprKind>>, end_pos: usize) -> Result<Term, ConstructError> {
        match head_elems.split_first_mut() {
            None => match *tail {
                None => {
                    let kind = TermKind::UnitTerm;
                    let span = Span {
                        start: end_pos,
                        end: end_pos,
                    };
                    Ok(Term::new(kind, Origin::ExprAt(span)))
                },
                Some(ref mut t) => build_operator_expr(ctx, t),
            },
            Some((first, rest)) => {
                let head = build_operator_expr(ctx, &mut first.expr)?;
                let tail = build(ctx, rest, tail, end_pos)?;
                let kind = TermKind::PairTerm {
                    head_name: build_opt_ident(&mut first.ident),
                    head: head,
                    tail: tail,
                };
                let span = Span {
                    start: first.span().start,
                    end: end_pos,
                };
                Ok(Term::new(kind, Origin::ExprAt(span)))
            },
        }
    }

    let end_pos = struct_term.span().end;
    build(ctx, &mut struct_term.head_elems[..], &mut struct_term.tail, end_pos)
}

/*
pub fn build_struct_term_typed(meta_ctx: &mut MetaCtx, ctx: &Ctx, struct_term: &StructTerm) -> Result<TypedTerm, ConstructError> {
    fn build(meta_ctx: &mut MetaCtx, ctx: &Ctx, head_elems: &[CompositeTermElem], tail: &Option<Expr>) -> Result<TypedTerm, ConstructError> {
        match head_elems.split_first() {
            None => match *tail {
                None => Ok(TypedTerm {
                    term: Term::new(TermKind::UnitTerm),
                    ty: Term::new(TermKind::UnitType),
                }),
                Some(ref t) => build_expr_typed(meta_ctx, ctx, t),
            },
            Some((first, rest)) => {
                let head = build_expr_typed(meta_ctx, ctx, &first.expr)?;
                let tail = build(meta_ctx, ctx, rest, tail)?;
                Ok(TypedTerm {
                    term: Term::new(TermKind::PairTerm {
                        head: head.term,
                        tail: tail.term,
                    }),
                    ty: Term::new(TermKind::PairType {
                        head_name: build_opt_ident(&first.ident),
                        head_type: head.ty,
                        tail_type: tail.ty,
                    }),
                })
            },
        }
    }

    build(meta_ctx, ctx, &struct_term.head_elems[..], &struct_term.tail)
}
*/

