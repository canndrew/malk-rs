use std::rc::Rc;
use core::{Origin, Ctx, Term, TermKind};
use core::meta::MVar;
use parse::{Syntax, LetExpr};
use construct::{build_block_expr, build_operator_expr, build_pattern, ConstructError};

pub fn build_let_expr(ctx: &Ctx, let_expr: &mut LetExpr) -> Result<Term, ConstructError> {
    let arg = build_operator_expr(ctx, &mut let_expr.let_to)?;

    let arg_type = MVar::fresh_type(ctx, Origin::TypeOf(Rc::new(Origin::PatternAt(let_expr.pattern.span()))));
    let ctx_ext = build_pattern(ctx, &mut let_expr.pattern)?;
    let ctx = ctx_ext.extend(ctx.clone());
    let res = build_block_expr(&ctx, &mut let_expr.let_in)?;
    let res = ctx_ext.build_elim(res);
    let func_kind = TermKind::FuncTerm {
        arg_name: ctx_ext.name(),
        arg_type: arg_type,
        res: res,
    };
    // TODO: fix this origin
    let func_term = Term::new(func_kind, Origin::ExprAt(let_expr.let_to.span()));
    let app_kind = TermKind::FuncApp {
        func: func_term,
        arg: arg,
    };
    // TODO: fix this origin
    let app_term = Term::new(app_kind, Origin::ExprAt(let_expr.let_to.span()));
    Ok(app_term)
}

