use std::cmp::max;
use lexer::Span;
use core::{Debug, Type, Form};
use parse::{Expr, CompositeTypeElem};
use typecheck::{Context, TypeCheckError};
use typecheck::{typecheck_type, typeget_type};

pub fn typecheck_struct_type(
        head_elems: &[CompositeTypeElem],
        tail: &Option<Expr>,
        expected_level: u32,
        ctx: Context,
        span: Span
    ) -> Result<Type, TypeCheckError>
{
    let c = d!({ ctx.core_ctx() });
    match head_elems.split_first() {
        None => {
            match *tail {
                None => {
                    Ok(Type::form(Form::unit(c)))
                },
                Some(ref tail_ty) => {
                    typecheck_type(tail_ty, expected_level, ctx)
                },
            }
        },
        Some((ref first, rest)) => {
            let head_ty = typecheck_type(&first.expr, expected_level, ctx.clone())?;
            let tail_ty = match first.ident {
                Some(ref head_name) => {
                    let core_ctx = ctx.core_ctx();
                    let new_core_ctx = core_ctx.append(head_ty.clone());
                    let sub_ctx = ctx.append(head_name, 1, new_core_ctx);
                    typecheck_struct_type(rest, tail, expected_level, sub_ctx, span)?
                },
                None => {
                    let tail_ty = typecheck_struct_type(rest, tail, expected_level, ctx, span)?;
                    tail_ty.weaken(head_ty.clone(), 0)
                },
            };
            Ok(Type::form(Form::pair(head_ty, tail_ty, d!({ Some(expected_level) }), c)))
        },
    }
}

pub fn typeget_struct_type(
        head_elems: &[CompositeTypeElem],
        tail: &Option<Expr>,
        ctx: Context,
        span: Span
    ) -> Result<(Type, u32), TypeCheckError>
{
    let c = d!({ ctx.core_ctx() });
    match head_elems.split_first() {
        None => {
            match *tail {
                None => {
                    Ok((Type::form(Form::unit(c)), 0))
                },
                Some(ref tail_ty) => {
                    typeget_type(tail_ty, ctx)
                },
            }
        },
        Some((ref first, rest)) => {
            let (head_ty, head_level) = typeget_type(&first.expr, ctx.clone())?;
            let (tail_ty, tail_level) = match first.ident {
                Some(ref head_name) => {
                    let core_ctx = ctx.core_ctx();
                    let new_core_ctx = core_ctx.append(head_ty.clone());
                    let sub_ctx = ctx.append(head_name, 1, new_core_ctx);
                    typeget_struct_type(rest, tail, sub_ctx, span)?
                },
                None => {
                    let (tail_ty, tail_level) = typeget_struct_type(rest, tail, ctx, span)?;
                    let tail_ty = tail_ty.weaken(head_ty.clone(), 0);
                    (tail_ty, tail_level)
                },
            };
            let struct_level = max(head_level, tail_level + 1);
            Ok((Type::form(Form::pair(head_ty, tail_ty, d!({ Some(struct_level) }), c)), struct_level))
        },
    }
}

/*
pub fn typecheck_struct_type(head_elems: &[CompositeTypeElem],
                             tail: &Option<Expr>,
                             expected_type: &Term,
                             ctx: &Ctx,
                             span: Span,
                             world: &World) -> Result<Term, TypeCheckError>
{
    if !is_subtype(expected_type, &Term::new(TermKind::Omega)) {
        return Err(TypeCheckError::TypeMismatch(TypeMismatch {
            span: span,
            expected: expected_type.clone(),
        }));
    };

    match head_elems.split_first() {
        None => {
            match *tail {
                None => {
                    Ok(Term::new(TermKind::UnitType))
                },
                Some(ref tail_type) => {
                    typecheck_expr(tail_type, expected_type, ctx, world)
                },
            }
        }
        Some((ref first, rest)) => {
            let sub_span = Span {
                start: match head_elems.get(2) {
                    Some(ref elem) => elem.span.start,
                    None => match *tail {
                        Some(ref tail_type) => tail_type.span.start,
                        None => span.end,
                    },
                },
                end: span.end,
            };
            let head_type = typecheck_expr(&first.expr, expected_type, ctx, world)?;
            let tail_type = {
                let sub_ctx = Ctx::Cons {
                    tail: ctx,
                    var_name: first.ident.as_ref().map_or("", |i| &i.text),
                    var_type: &head_type,
                };
                typecheck_struct_type(rest, tail, expected_type, &sub_ctx, sub_span, world)?
            };
            Ok(Term::new(TermKind::PairType {
                head_type: head_type,
                tail_type: tail_type,
            }))
        },
    }
}
*/

