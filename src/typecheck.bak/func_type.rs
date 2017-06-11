use core::{Term, TermKind, World, bump_index};
use parse::{Expr, FuncType, PatternKind};
use typecheck::{Ctx, TypeCheckError, TypeMismatch};
use typecheck::{typecheck_expr, typeget_pattern};

pub fn typecheck_func_type<'c, 'p: 'c>(
        func_type: &FuncType,
        expected_type: &Term,
        ctx: Ctx<'c, 'p>,
        world: &World) -> Result<Term, TypeCheckError>
{
    println!("typecheck_func_type({})", func_type);
    match **expected_type {
        TermKind::Omega | TermKind::Type { .. } => {
            let (arg_node, arg_type) = typeget_pattern(&func_type.pattern, ctx, world)?;
            let res_type = {
                let res_type_type = bump_index(expected_type, 1, 0);
                let skewed_res_type_type = arg_node.skew(&res_type_type, world);
                let sub_ctx = Ctx::Cons(&arg_node);
                let skewed_res_type = typecheck_expr(&func_type.body_type,
                                                     &skewed_res_type_type,
                                                     sub_ctx,
                                                     world)?;
                println!("skewed_res_type == {:?}", skewed_res_type);
                arg_node.deskew(&skewed_res_type, world)
            };
            Ok(Term::new(TermKind::FuncType {
                arg_type: arg_type,
                res_type: res_type,
            }))
        },
        _ => return Err(TypeCheckError::TypeMismatch(TypeMismatch {
            span: func_type.span,
            expected: expected_type.clone(),
        })),
    }
}

