use std::rc::Rc;
use parse::{Span, Pattern, PatternKind, StructPattern, CompositePatternElem, Syntax};
use construct::{CtxExt, CtxExtKind, ConstructError, build_ident, build_opt_ident};
use core::{Ctx, MVar, Origin};

pub fn build_pattern(
        ctx: &Ctx,
        pattern: &mut Pattern
    ) -> Result<CtxExt , ConstructError>
{
    match pattern.kind {
        PatternKind::Parens(ref mut parens_pattern) => {
            build_pattern(ctx, &mut parens_pattern.sub_pattern)
        },
        PatternKind::Bind(ref ident) => {
            let ty = MVar::fresh_type(ctx, Origin::TypeOf(Rc::new(Origin::PatternAt(ident.span))));
            let kind = CtxExtKind::Bind {
                name: build_ident(ident),
                ty: ty,
            };
            Ok(CtxExt::new(kind, Origin::PatternAt(pattern.span())))
        },
        /*
        PatternKind::Typed(ref mut typed_pattern) => {
            let ty = build_expr(ctx, &mut typed_pattern.pattern_type)?;
            let ext = build_pattern(ctx, &mut typed_pattern.sub_pattern)?;
            Ok(CtxPattern::Typed {
                ty: ty,
                sub_pattern: sub_pattern,
            })
        },
        */
        PatternKind::Struct(ref mut struct_pattern) => {
            build_struct_pattern(ctx, struct_pattern)
        },
        /*
        PatternKind::Equality(ref equality_pattern) => {
            let a_term = build_expr(meta_ctx, ctx, &equality_pattern.a)?;
            let b_term = build_expr(meta_ctx, ctx, &equality_pattern.b)?;
            Ok(CtxPattern::Equality {
                a: a_term,
                b: b_term,
            })
        },
        */
        _ => unimplemented!(),
    }
}

pub fn build_struct_pattern(ctx: &Ctx, struct_pattern: &mut StructPattern) -> Result<CtxExt, ConstructError> {
    fn build(
            ctx: &Ctx,
            head_elems: &mut [CompositePatternElem],
            tail: &mut Option<Pattern>,
            end_pos: usize,
        ) -> Result<CtxExt, ConstructError>
    {
        match head_elems.split_first_mut() {
            None => match *tail {
                None => {
                    let span = Span::new(end_pos, end_pos);
                    Ok(CtxExt::new(CtxExtKind::Unit, Origin::PatternAt(span)))
                },
                Some(ref mut pattern) => build_pattern(ctx, pattern),
            },
            Some((first, rest)) => {
                let head_ext = build_pattern(ctx, &mut first.sub_pattern)?;
                let tail_ctx = head_ext.extend(ctx.clone());
                let tail_ext = build(&tail_ctx, rest, tail, end_pos)?;
                let span = Span::new(first.span().start, end_pos);
                let origin = Origin::PatternAt(span);
                Ok(CtxExt::new(CtxExtKind::Pair {
                    head_name: build_opt_ident(&first.name),
                    head: head_ext,
                    tail: tail_ext,
                }, origin))
            },
        }
    }

    let end_pos = struct_pattern.span().end;
    build(ctx, &mut struct_pattern.head_elems[..], &mut struct_pattern.tail, end_pos)
}

