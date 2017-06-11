use core::{Origin, Ctx, Term, TermKind};
use parse::{Syntax, FuncApp};
use construct::{build_app_expr, build_enclosed_expr, ConstructError};

pub fn build_func_app(ctx: &Ctx, func_app: &mut FuncApp) -> Result<Term, ConstructError> {
    let func = build_app_expr(ctx, &mut func_app.func)?;
    let arg = build_enclosed_expr(ctx, &mut func_app.arg)?;
    let kind = TermKind::FuncApp {
        func: func,
        arg: arg,
    };
    Ok(Term::new(kind, Origin::ExprAt(func_app.span())))
}

/*
pub fn build_func_app_typed(meta_ctx: &mut MetaCtx, ctx: &Ctx, func_app: &FuncApp) -> Result<TypedTerm, ConstructError> {
    let func = build_expr(meta_ctx, ctx, &func_app.func)?;
    let arg = build_expr(meta_ctx, ctx, &func_app.arg)?;
    let arg_universe = meta_ctx.fresh_universe(ctx);
    let arg_type = meta_ctx.new_var(ctx.clone(), arg_universe);
    let res_ctx = Ctx::cons(arg_type, ctx);
    let res_universe = meta_ctx.fresh_universe(&res_ctx);
    let res_type = meta_ctx.new_var(res_ctx, res_universe);
    Ok(Term::new(TermKind::FuncApp {
        func: func,
        arg: arg,
        arg_type: arg_type,
        res_type: res_type,
    }))
}
*/

