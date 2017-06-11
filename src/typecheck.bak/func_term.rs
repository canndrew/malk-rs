use core::{Term, TermKind, Type, TypeKind, Form, FormKind, Debug, Ctx};
use parse::{FuncTerm};
use typecheck::{Context, TypeCheckError};
use typecheck::{typecheck_expr, typeget_expr};
use typecheck::{typecheck_open_pattern, typeget_open_pattern, close_pattern};
use typecheck::debug::{print_ctx, print_type};

pub fn typecheck_func_term<'p>(
        func_term: &FuncTerm,
        expected_type: Type,
        ctx: Context<'p>
    ) -> Result<Term, TypeCheckError>
{
    let c = d!({ ctx.core_ctx() });
    match *expected_type.kind() {
        TypeKind::Form(ref f) => {
            match *f.kind() {
                FormKind::Func { ref arg, ref res } => {
                    // Adds the new names and core nodes to the context, not including the main
                    // function arg (at ctx_index == 0). This is possible because none of the added
                    // nodes depend on the function arg, so we're free to weaken it in later.
                    // extension is the number of core nodes that was added.
                    let (sub_ctx, extension) = typecheck_open_pattern(0, &func_term.pattern, arg.clone(), ctx.clone())?;

                    // insert the main function arg.
                    let sub_ctx = sub_ctx.weaken_core(arg.clone(), extension);

                    // Then we need to weaken res to include all the extra nodes we just added.
                    let mut res = res.clone();
                    for i in (0..extension).rev() {
                        let ty = sub_ctx.core_ctx().lookup(i);
                        res = res.weaken(ty, 0);
                    };

                    // Typecheck body under this weird context we've built up.
                    let body = typecheck_expr(&func_term.body, res, sub_ctx.clone())?;

                    // And close it to get back to the normal context.
                    let body = close_pattern(body, extension, &func_term.pattern, arg.clone(), sub_ctx)?;

                    return Ok(Term::lambda(body, d!({ Some(expected_type.clone()) }), c));
                },
                _ => (),
            }
        },
        _ => (),
    }
    Err(TypeCheckError::UnexpectedFunction {
        span: func_term.span,
        expected: expected_type,
    })
}

pub fn typeget_func_term<'p>(
        func_term: &FuncTerm,
        ctx: Context<'p>
    ) -> Result<(Term, Type), TypeCheckError>
{
    let c = d!({ ctx.core_ctx() });

    // Adds the new names and core nodes to the context, not including the main
    // function arg (at ctx_index == 0). This is possible because none of the added
    // nodes depend on the function arg, so we're free to weaken it in later.
    // extension is the number of core nodes that was added.
    let (sub_ctx, extension, arg_ty) = typeget_open_pattern(0, &func_term.pattern, ctx.clone())?;

    print!("sub_ctx == ");
    print_ctx(&sub_ctx);
    println!("");
    println!("extension == {}", extension);
    print!("arg_ty == ");
    print_type(&arg_ty, 0);
    println!("");

    // insert the main function arg.
    let sub_ctx = sub_ctx.weaken_core(arg_ty.clone(), extension);
    print!("sub_ctx (again) == ");
    print_ctx(&sub_ctx);
    println!("");

    // Typecheck body under this weird context we've built up.
    let (body, res_ty) = typeget_expr(&func_term.body, sub_ctx.clone())?;

    // And close it to get back to the normal context.
    let body = close_pattern(body, extension, &func_term.pattern, arg_ty.clone(), sub_ctx)?;

    let ty = Type::form(Form::func(arg_ty, res_ty, d!({ None }), c.clone()));
    let term = Term::lambda(body, d!({ Some(ty.clone()) }), c);
    Ok((term, ty))
}


/*
pub fn typecheck_func_term<'c, 'p: 'c>(
        func_term: &FuncTerm,
        expected_type: &Term,
        ctx: Ctx<'c, 'p>,
        world: &World) -> Result<Term, TypeCheckError>
{
    match **expected_type {
        TermKind::FuncType { ref arg_type, ref res_type } => {
            let arg_ctx = typecheck_pattern(&func_term.pattern,
                                            arg_type,
                                            ctx,
                                            world)?;
            let skewed_body = {
                let skewed_res_type = arg_ctx.skew(res_type, world);
                let sub_ctx = Ctx::Cons(&arg_ctx);
                typecheck_expr(&func_term.body, &skewed_res_type, sub_ctx, world)?
            };
            println!("skewed_body == {:#?}", skewed_body);
            let body = arg_ctx.deskew(&skewed_body, world);
            println!("body == {:#?}", body);
            let func_term = Term::new(TermKind::FuncTerm {
                body: body,
            });
            Ok(reduce_head(&func_term, world))
        },
        _ => return Err(TypeCheckError::TypeMismatch(TypeMismatch {
            span: func_term.span,
            expected: expected_type.clone(),
        })),
    }
}
*/

