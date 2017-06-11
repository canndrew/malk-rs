use std::rc::Rc;
use lexer::{Span, TextPos};
use core::{Ctx, Debug, Term, TermKind, Type};
use parse::{Expr, Pattern, PatternKind, CompositePatternElem};
use typecheck::{Context, ContextNode, TypeCheckError};
use typecheck::{typecheck_expr, typeget_type, types_match};


pub fn typecheck_open_pattern<'p>(
        ctx_index: u32,
        pattern: &'p Pattern,
        expected_type: Type,
        ctx: Context<'p>,
    ) -> Result<(Context<'p>, u32), TypeCheckError>
{
    match pattern.kind {
        PatternKind::Parens(ref sub_pattern) => {
            typecheck_open_pattern(ctx_index, sub_pattern, expected_type, ctx)
        },
        PatternKind::Bind(ref ident) => {
            let core_ctx = ctx.core_ctx();
            let new_core_ctx = core_ctx.append(expected_type);
            let new_ctx = ctx.append(ident, 1, new_core_ctx);
            Ok((new_ctx, 1))
        },
        PatternKind::Typed(ref typed_pattern) => {
            let (typed_type, _) = typeget_type(&typed_pattern.pattern_type, ctx.clone())?;
            types_match(&typed_type, &expected_type, pattern.span)?;
            typecheck_open_pattern(ctx_index, &typed_pattern.sub_pattern, typed_type, ctx)
        },
        _ => panic!("typecheck other pattern types not implemented"),
    }
}

pub fn typeget_open_pattern<'p>(
        ctx_index: u32,
        pattern: &'p Pattern,
        ctx: Context<'p>,
    ) -> Result<(Context<'p>, u32, Type), TypeCheckError>
{
    match pattern.kind {
        PatternKind::Parens(ref sub_pattern) => {
            typeget_open_pattern(ctx_index, sub_pattern, ctx)
        },
        PatternKind::Bind(ref ident) => {
            Err(TypeCheckError::BindUnknownType {
                span: ident.span,
            })
        },
        PatternKind::Typed(ref typed_pattern) => {
            let (typed_type, _) = typeget_type(&typed_pattern.pattern_type, ctx.clone())?;
            let (ctx, extension) = typecheck_open_pattern(ctx_index, &typed_pattern.sub_pattern, typed_type.clone(), ctx)?;
            Ok((ctx, extension, typed_type))
        },
        _ => panic!("typeget other pattern types not implemented: {:?}", pattern.kind),
    }
}

pub fn close_pattern<'p>(
        term: Term,
        ctx_index: u32,
        pattern: &'p Pattern,
        pattern_type: Type,
        ctx: Context<'p>
    ) -> Result<Term, TypeCheckError>
{
    let c = d!({ ctx.core_ctx() });
    match pattern.kind {
        PatternKind::Parens(ref sub_pattern) => {
            close_pattern(term, ctx_index, sub_pattern, pattern_type, ctx)
        },
        PatternKind::Bind(ref ident) => {
            let sub_c = d!({
                let c = c?;
                assert!(c.lookup(0).subtype_of(&pattern_type));
                unwrap!(c.try_strengthen(0))
            });
            let lambda = Term::lambda(term, d!({ None }), sub_c.clone());
            let var = Term::var(ctx_index, d!({ Some(pattern_type) }), sub_c.clone());
            let app = Term::app(lambda, var, d!({ None }), sub_c);
            Ok(app)
        },
        PatternKind::Typed(ref typed_pattern) => {
            let (typed_type, _) = typeget_type(&typed_pattern.pattern_type, ctx.clone())?;
            unwrap!(types_match(&typed_type, &pattern_type, pattern.span));
            close_pattern(term, ctx_index, &typed_pattern.sub_pattern, pattern_type, ctx)
        },
        _ => panic!("close not implemented for other pattern types"),
    }
}






/*
pub fn typecheck_pattern<'c, 'p: 'c>(
        pattern: &'p Pattern,
        pattern_type: &Term,
        ctx: Ctx<'c, 'p>,
        world: &World,
    ) -> Result<CtxNode<'c, 'p>, TypeCheckError>
{
    println!("typecheck_pattern({}): {:?}", pattern, pattern_type);
    match pattern.kind {
        PatternKind::Parens(ref sub_pattern) => {
            typecheck_pattern(sub_pattern, pattern_type, ctx, world)
        },
        PatternKind::Bind(ref ident) => {
            let ctx_node_kind = CtxNodeKind::Var {
                var_name: ident,
                var_type: pattern_type.clone(),
            };
            let ctx_node = CtxNode {
                tail: ctx,
                kind: ctx_node_kind,
            };
            Ok(ctx_node)
        },
        PatternKind::Typed(ref typed_pattern) => {
            let checked_type = typecheck_expr(&typed_pattern.pattern_type, &Term::new(TermKind::Omega), ctx, world)?;
            if !is_subtype(&checked_type, pattern_type) {
                return Err(TypeCheckError::TypeMismatch(TypeMismatch {
                    span: pattern.span,
                    expected: pattern_type.clone(),
                }));
            }
            typecheck_pattern(&typed_pattern.sub_pattern,
                              pattern_type,
                              ctx,
                              world)
        },
        PatternKind::Struct(ref struct_pattern) => {
            typecheck_struct_pattern(&struct_pattern.head_elems[..],
                                     &struct_pattern.tail,
                                     pattern_type,
                                     ctx,
                                     pattern.span.end,
                                     world)
        },
    }
}

pub fn typecheck_struct_pattern<'c, 'p: 'c>(
        head_elems: &'p [CompositePatternElem],
        tail_elem: &'p Option<Pattern>,
        pattern_type: &Term,
        ctx: Ctx<'c, 'p>,
        span_end: TextPos,
        world: &World
    ) -> Result<CtxNode<'c, 'p>, TypeCheckError>
{
    print!("typecheck_struct_pattern(");
    for elem in head_elems {
        print!("{},", elem);
    }
    if let Some(ref elem) = *tail_elem {
        print!("; {}", elem);
    }
    println!(")");
    match head_elems.split_first() {
        None => {
            match *tail_elem {
                None => {
                    if let TermKind::UnitType = **pattern_type {
                        let ctx_node_kind = CtxNodeKind::Unit;
                        let ctx_node = CtxNode {
                            tail: ctx,
                            kind: ctx_node_kind,
                        };
                        Ok(ctx_node)
                    } else {
                        let span = Span {
                            start: span_end,
                            end: span_end,
                        };
                        Err(TypeCheckError::TypeMismatch(TypeMismatch {
                            span: span,
                            expected: pattern_type.clone(),
                        }))
                    }
                }
                Some(ref tail) => {
                    typecheck_pattern(tail, pattern_type, ctx, world)
                }
            }
        },
        Some((ref first, rest)) => {
            if let TermKind::PairType { ref head_type, ref tail_type } = **pattern_type {
                let head_node = typecheck_pattern(&first.sub_pattern,
                                                  head_type,
                                                  ctx,
                                                  world)?;
                let skewed_tail_type = head_node.skew(tail_type, world);
                println!("tail_type == {:?}", tail_type);
                println!("skewed_tail_type == {:?}", skewed_tail_type);
                let tail_node_kind = {
                    let sub_ctx = Ctx::Cons(&head_node);
                    let tail_node = typecheck_struct_pattern(rest,
                                                             tail_elem,
                                                             &skewed_tail_type,
                                                             sub_ctx,
                                                             span_end,
                                                             world)?;
                    tail_node.kind
                };
                let ctx_node_kind = CtxNodeKind::Pair {
                    head: box head_node.kind,
                    tail: box tail_node_kind,
                };
                let ctx_node = CtxNode {
                    tail: ctx,
                    kind: ctx_node_kind,
                };
                Ok(ctx_node)
            } else {
                let span = Span {
                    start: first.span.start,
                    end: span_end,
                };
                Err(TypeCheckError::TypeMismatch(TypeMismatch {
                    span: span,
                    expected: pattern_type.clone(),
                }))
            }
        },
    }
}

pub fn typeget_pattern<'c, 'p: 'c>(
        pattern: &'p Pattern,
        ctx: Ctx<'c, 'p>,
        world: &World,
    ) -> Result<(CtxNode<'c, 'p>, Term), TypeCheckError>
{
    match pattern.kind {
        PatternKind::Parens(ref sub_pattern) => {
            typeget_pattern(sub_pattern, ctx, world)
        },
        PatternKind::Bind(ref ident) => {
            Err(TypeCheckError::UnknownType {
                span: pattern.span,
            })
        },
        PatternKind::Typed(ref typed_pattern) => {
            let checked_type = typecheck_expr(&typed_pattern.pattern_type, &Term::new(TermKind::Omega), ctx, world)?;
            let ctx_node = typecheck_pattern(&typed_pattern.sub_pattern,
                                             &checked_type,
                                             ctx,
                                             world)?;
            Ok((ctx_node, checked_type))
        },
        PatternKind::Struct(ref struct_pattern) => {
            typeget_struct_pattern(&struct_pattern.head_elems[..],
                                   &struct_pattern.tail,
                                   ctx,
                                   pattern.span.end,
                                   world)
        },
    }
}

pub fn typeget_struct_pattern<'c, 'p: 'c>(
        head_elems: &'p [CompositePatternElem],
        tail_elem: &'p Option<Pattern>,
        ctx: Ctx<'c, 'p>,
        span_end: TextPos,
        world: &World
    ) -> Result<(CtxNode<'c, 'p>, Term), TypeCheckError>
{
    print!("typecheck_struct_pattern(");
    for elem in head_elems {
        print!("{},", elem);
    }
    if let Some(ref elem) = *tail_elem {
        print!("; {}", elem);
    }
    println!(")");
    match head_elems.split_first() {
        None => {
            match *tail_elem {
                None => {
                    let ctx_node_kind = CtxNodeKind::Unit;
                    let ctx_node = CtxNode {
                        tail: ctx,
                        kind: ctx_node_kind,
                    };
                    let pattern_type = Term::new(TermKind::UnitType);
                    Ok((ctx_node, pattern_type))
                }
                Some(ref tail) => {
                    typeget_pattern(tail, ctx, world)
                }
            }
        },
        Some((ref first, rest)) => {
            let (head_node, head_type) = typeget_pattern(&first.sub_pattern,
                                                         ctx,
                                                         world)?;
            let (tail_node_kind, skewed_tail_type) = {
                let sub_ctx = Ctx::Cons(&head_node);
                let (tail_node, skewed_tail_type) = typeget_struct_pattern(rest,
                                                                           tail_elem,
                                                                           sub_ctx,
                                                                           span_end,
                                                                           world)?;
                (tail_node.kind, skewed_tail_type)
            };
            let tail_type = head_node.deskew(&skewed_tail_type, world);
            let pattern_type = Term::new(TermKind::PairType {
                head_type: head_type,
                tail_type: tail_type,
            });
            let ctx_node_kind = CtxNodeKind::Pair {
                head: box head_node.kind,
                tail: box tail_node_kind,
            };
            let ctx_node = CtxNode {
                tail: ctx,
                kind: ctx_node_kind,
            };
            Ok((ctx_node, pattern_type))
        },
    }
}

*/




