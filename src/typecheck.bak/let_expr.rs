use lexer::Span;
use core::{Term, TermKind, World, bump_index, reduce_head};
use parse::{LetExpr, PatternKind};
use typecheck::{Ctx, TypeCheckError, typecheck_expr, typecheck_pattern};

pub fn typecheck_let_expr(let_expr: &LetExpr,
                          expected_type: &Term,
                          ctx: &Ctx,
                          span: Span,
                          world: &World) -> Result<Term, TypeCheckError>
{
    let typed_pattern;
    let mut pattern = &let_expr.pattern;
    loop {
        match pattern.kind {
            PatternKind::Parens(ref pat) => {
                pattern = pat;
            },
            PatternKind::Typed(ref pat) => {
                typed_pattern = pat;
                break;
            },
            _ => return Err(TypeCheckError::UnknownType {
                span: pattern.span,
            }),
        }
    }

    let pattern_type = typecheck_expr(&typed_pattern.pattern_type, &Term::new(TermKind::Omega), ctx, world)?;
    let let_to = typecheck_expr(&let_expr.let_to, &pattern_type, ctx, world)?;
    let body = typecheck_pattern(&typed_pattern.sub_pattern,
                                 &pattern_type,
                                 ctx,
                                 expected_type,
                                 ctx,
                                 world,
                                 Box::new(|sub_cont_type: &Term, sub_ctx: &Ctx| {
        typecheck_expr(&let_expr.let_in, sub_cont_type, sub_ctx, world)
    }))?;
    let func = Term::new(TermKind::FuncTerm {
        body: body,
    });
    let res_type = bump_index(expected_type, 1, 0);
    let app = Term::new(TermKind::FuncApp {
        func: func,
        arg: let_to,
        arg_type: pattern_type,
        res_type: res_type,
    });
    let app = reduce_head(&app, world);
    Ok(app)
}

