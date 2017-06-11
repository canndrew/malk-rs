use lexer::Span;
use core::{Term, Type, TypeKind, Form, FormKind, Debug};
use parse::{Expr, CompositeTermElem};
use typecheck::{Context, TypeCheckError, types_match};
use typecheck::{typecheck_expr, typeget_expr};

pub fn typecheck_struct_term<'p>(
        head_elems: &'p [CompositeTermElem],
        tail: &'p Option<Expr>,
        expected_type: Type,
        ctx: Context,
        span: Span) -> Result<Term, TypeCheckError>
{
    let unexpected_struct = || {
        Err(TypeCheckError::UnexpectedStruct {
            span: span,
            expected: expected_type.clone(),
        })
    };
    let c = d!({ ctx.core_ctx() });
    match head_elems.split_first() {
        None => {
            match *tail {
                None => {
                    let ty = Type::form(Form::unit(c.clone()));
                    types_match(&ty, &expected_type, span)?;
                    Ok(Term::unit(d!({ None }), c))
                },
                Some(ref tail_term) => {
                    typecheck_expr(tail_term, expected_type.clone(), ctx)
                }
            }
        }
        Some((ref first, rest)) => {
            match *expected_type.kind() {
                TypeKind::Form(ref f) => {
                    match *f.kind() {
                        FormKind::Pair { head: ref head_ty, tail: ref tail_ty } => {
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
                            let head = typecheck_expr(&first.expr, head_ty.clone(), ctx.clone())?;
                            let tail = typecheck_struct_term(rest, tail, tail_ty.clone(), ctx, sub_span)?;
                            Ok(Term::pair(head, tail, d!({ None }), c))
                        },
                        _ => unexpected_struct(),

                    }
                },
                _ => unexpected_struct(),
            }
        },
    }
}

pub fn typeget_struct_term<'p>(
        head_elems: &'p [CompositeTermElem],
        tail: &'p Option<Expr>,
        ctx: Context,
        span: Span) -> Result<(Term, Type), TypeCheckError>
{
    let c = d!({ ctx.core_ctx() });
    match head_elems.split_first() {
        None => {
            return match *tail {
                None => {
                    let ty = Type::form(Form::unit(c.clone()));
                    Ok((Term::unit(d!({ None }), c), ty))
                },
                Some(ref tail_term) => {
                    typeget_expr(tail_term, ctx)
                }
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
            let (head, head_ty) = typeget_expr(&first.expr, ctx.clone())?;
            let (tail, tail_ty) = typeget_struct_term(rest, tail, ctx, sub_span)?;
            let term = Term::pair(head, tail, d!({ None }), c.clone());
            let weak_tail_ty = tail_ty.weaken(head_ty.clone(), 0);
            let ty = Type::form(Form::pair(head_ty, weak_tail_ty, d!({ None }), c));
            Ok((term, ty))
        },
    }
}
/*

pub fn typecheck_struct_term(head_elems: &[CompositeTermElem],
                             tail: &Option<Expr>,
                             expected_type: &Term,
                             ctx: &Ctx,
                             span: Span,
                             world: &World) -> Result<Term, TypeCheckError>
{
    match head_elems.split_first() {
        None => {
            match *tail {
                None => match **expected_type {
                    TermKind::UnitType => Ok(Term::new(TermKind::UnitTerm)),
                    _ => Err(TypeCheckError::TypeMismatch(TypeMismatch {
                        span: span,
                        expected: expected_type.clone(),
                    })),
                },
                Some(ref tail_term) => {
                    typecheck_expr(tail_term, expected_type, ctx, world)
                },
            }
        },
        Some((ref first, rest)) => {
            match **expected_type {
                TermKind::PairType { ref head_type, ref tail_type } => {
                    let sub_ctx = Ctx::Cons {
                        tail: ctx,
                        var_name: first.ident.as_ref().map_or("", |i| &i.text),
                        var_type: head_type,
                    };
                    let tail = typecheck_struct_term(rest, tail, tail_type, &sub_ctx, sub_span, world)?;
                    let pair = Term::new(TermKind::PairTerm {
                        head: head,
                        tail: tail,
                    });
                    Ok(reduce_head(&pair, world))
                },
                _ => Err(TypeCheckError::TypeMismatch(TypeMismatch {
                    span: span,
                    expected: expected_type.clone(),
                })),
            }
        },
    }
}
*/

