use lexer::Span;
use core::{Term, TermKind, World, substitute, is_subtype, reduce_head};
use parse::{ExprKind, FuncApp};
use typecheck::{TypeCheckError, TypeMismatch, Ctx, typecheck_expr, typeget_expr};

pub fn typecheck_func_app<'c, 'p: 'c>(
        func_app: &'p FuncApp,
        span: Span,
        expected_type: &Term,
        ctx: Ctx<'c, 'p>,
        world: &World) -> Result<Term, TypeCheckError>
{
    let (func, func_type) = typeget_expr(&func_app.func, ctx, world)?;
    match *func_type {
        TermKind::FuncType { ref arg_type, ref res_type } => {
            let arg = typecheck_expr(&func_app.arg, arg_type, ctx, world)?;
            let substituted_res_type = substitute(res_type, &arg, 0);
            if !is_subtype(&substituted_res_type, expected_type) {
                return Err(TypeCheckError::TypeMismatch(TypeMismatch {
                    span: span,
                    expected: expected_type.clone(),
                }));
            }
            let app = Term::new(TermKind::FuncApp {
                func: func,
                arg: arg,
                arg_type: arg_type.clone(),
                res_type: res_type.clone(),
            });
            Ok(reduce_head(&app, world))
        },
        _ => {
            return Err(TypeCheckError::ExpectedFunctionType {
                span: func_app.func.span,
            });
        },
    }
}

