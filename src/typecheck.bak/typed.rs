use core::{Term, Type};
use parse::Typed;
use typecheck::{typecheck_expr, typeget_expr};
use typecheck::{Context, TypeCheckError, into_type};

pub fn typecheck_typed<'p>(
    typed: &'p Typed,
    expected_type: Type,
    ctx: Context) -> Result<Term, TypeCheckError>
{
    let (typed_type, typed_universe) = typeget_expr(&typed.typed_type, ctx.clone())?;
    let typed_type = into_type(typed_type, typed_universe, typed.typed_type.span)?;
    if !typed_type.subtype_of(&expected_type) {
        return Err(TypeCheckError::TypeMismatch {
            span: typed.span,
            expected: expected_type,
            got: typed_type,
        });
    }
    typecheck_expr(&typed.typed_term, typed_type, ctx)
}

pub fn typeget_typed<'p>(
    typed: &'p Typed,
    ctx: Context) -> Result<(Term, Type), TypeCheckError>
{
    let (typed_type, typed_universe) = typeget_expr(&typed.typed_type, ctx.clone())?;
    let typed_type = into_type(typed_type, typed_universe, typed.typed_type.span)?;
    let term = typecheck_expr(&typed.typed_term, typed_type.clone(), ctx)?;
    Ok((term, typed_type))
}

/*

pub fn typecheck_typed<'c, 'p: 'c>(
        typed: &'p Typed,
        expected_type: &Term,
        ctx: Ctx<'c, 'p>,
        world: &World) -> Result<Term, TypeCheckError>
{
    println!("typed_type == {:?}", typed_type);
    println!("expected_type == {:?}", expected_type);
    if !is_subtype(&typed_type, expected_type) {
        return Err(TypeCheckError::TypeMismatch(TypeMismatch {
            span: typed.span,
            expected: expected_type.clone(),
        }));
    }
    typecheck_expr(&typed.typed_term, expected_type, ctx, world)
}

pub fn typeget_typed<'c, 'p: 'c>(
        typed: &'p Typed,
        ctx: Ctx<'c, 'p>,
        world: &World) -> Result<(Term, Term), TypeCheckError>
{
    let typed_type = typecheck_expr(&typed.typed_type, &Term::new(TermKind::Omega), ctx, world)?;
    let typed_term = typecheck_expr(&typed.typed_term, &typed_type, ctx, world)?;
    Ok((typed_term, typed_type))
}
*/

