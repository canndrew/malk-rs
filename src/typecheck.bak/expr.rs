use core::{Debug, Term, Type, TypeKind, Form, FormKind};
use parse::{Expr, ExprKind};
use typecheck::{Context, TypeCheckError, into_type, into_term, into_universe};
use typecheck::{typecheck_typed, typeget_typed};
use typecheck::{typecheck_variable, typeget_variable};
use typecheck::{typecheck_struct_term, typeget_struct_term};
use typecheck::{typecheck_struct_type, typeget_struct_type};
use typecheck::{typecheck_func_term, typeget_func_term};

pub fn typecheck_expr<'p>(
    expr: &'p Expr,
    expected_type: Type,
    ctx: Context) -> Result<Term, TypeCheckError>
{
    println!("typecheck_expr({})", expr);
    match expr.kind {
        ExprKind::Parens(ref e) => typecheck_expr(e, expected_type, ctx),
        ExprKind::Typed(ref typed) => typecheck_typed(typed, expected_type, ctx),
        ExprKind::Variable(ref ident) => typecheck_variable(ident, expected_type, ctx),
        ExprKind::StructTerm(ref struct_term) => typecheck_struct_term(
                    &struct_term.head_elems[..],
                    &struct_term.tail,
                    expected_type,
                    ctx,
                    struct_term.span),
        ExprKind::FuncTerm(ref func_term) => typecheck_func_term(func_term, expected_type, ctx),
        _ => {
            let expected_level = into_universe(expected_type, expr.span)?;
            match expr.kind {
                ExprKind::StructType(ref struct_type) => {
                    let ty = typecheck_struct_type(
                            &struct_type.head_elems[..],
                            &struct_type.tail,
                            expected_level,
                            ctx,
                            struct_type.span)?;
                    let term = ty.as_term();
                    Ok(term)
                },
                _ => panic!("other syntax forms not implemented for typecheck"),
            }
        },
    }
}

pub fn typeget_expr<'p>(
    expr: &'p Expr,
    ctx: Context) -> Result<(Term, Type), TypeCheckError>
{
    println!("typeget_expr({})", expr);
    match expr.kind {
        ExprKind::Parens(ref e) => typeget_expr(e, ctx),
        ExprKind::Typed(ref typed) => typeget_typed(typed, ctx),
        ExprKind::Variable(ref ident) => typeget_variable(ident, ctx),
        ExprKind::StructTerm(ref struct_term) => typeget_struct_term(
                    &struct_term.head_elems[..],
                    &struct_term.tail,
                    ctx,
                    struct_term.span),
        ExprKind::FuncTerm(ref func_term) => typeget_func_term(func_term, ctx),
        ExprKind::StructType(ref struct_type) => {
            let (ty, level) = typeget_struct_type(
                        &struct_type.head_elems[..],
                        &struct_type.tail,
                        ctx.clone(),
                        struct_type.span)?;
            Ok(into_term(ty, level, ctx))
        },
        _ => panic!("other syntax forms not implemented for typeget"),
    }
}

pub fn typecheck_type<'p>(
        expr: &'p Expr,
        expected_level: u32,
        ctx: Context
    ) -> Result<Type, TypeCheckError>
{
    let c = d!({ ctx.core_ctx() });
    let universe = Type::form(Form::ty(expected_level, d!({ None }), c));
    let ty = typecheck_expr(expr, universe.clone(), ctx)?;
    let ty = into_type(ty, universe, expr.span)?;
    Ok(ty)
}

pub fn typeget_type<'p>(
        expr: &'p Expr,
        ctx: Context
    ) -> Result<(Type, u32), TypeCheckError>
{
    let (ty, universe) = typeget_expr(expr, ctx)?;
    let ty = into_type(ty, universe.clone(), expr.span)?;
    let level = match *universe.kind() {
        TypeKind::Form(ref f) => {
            match *f.kind() {
                FormKind::Type { level } => level,
                _ => panic!("impossible. into_type should have filtered this."),
            }
        },
        _ => panic!("impossible. into_type should have filtered this."),
    };
    Ok((ty, level))
}

/*
pub fn universecheck_expr>'p>(
    expr: &'p Expr,
    ctx: Ctx) -> Result<Type, TypeCheckError>
{
    println!("typecheck_expr({})", expr);
    match expr.kind {
        ExprKind::Parens(ref e) => typecheck_expr(e, expected_type, ctx),
        ExprKind::Typed(ref typed) => typecheck_typed(typed, expected_type, ctx),
        _ => unimplemented!(),
    }
}
*/

/*
use core::{Term, World};
use parse::{Expr, ExprKind};
use typecheck::{Ctx, TypeCheckError};
use typecheck::{/*typecheck_struct_term, typecheck_struct_type, */typecheck_typed,
                typecheck_variable, typecheck_func_type, typecheck_numeric,
                typecheck_func_app, typecheck_func_term, /* typecheck_let_expr, */ 
                typeget_typed, typeget_variable};

pub fn typecheck_expr<'c, 'p: 'c>(
        expr: &'p Expr,
        expected_type: &Term,
        ctx: Ctx<'c, 'p>,
        world: &World) -> Result<Term, TypeCheckError>
{
    println!("typecheck_expr({})", expr);
    match expr.kind {
        ExprKind::Parens(ref e) => typecheck_expr(e, expected_type, ctx, world),
        /*
        ExprKind::StructTerm(ref st) => {
            typecheck_struct_term(&st.head_elems[..], &st.tail, expected_type, ctx, expr.span, world)
        },
        ExprKind::StructType(ref st) => {
            println!("typecheck_struct_type({})", st);
            typecheck_struct_type(&st.head_elems[..], &st.tail, expected_type, ctx, expr.span, world)
        },
        */
        ExprKind::Typed(ref typed) => {
            typecheck_typed(typed, expected_type, ctx, world)
        },
        ExprKind::Variable(ref ident) => {
            typecheck_variable(ident, expected_type, ctx, world)
        },
        ExprKind::Numeric(ref ident) => {
            typecheck_numeric(ident, expected_type)
        },
        ExprKind::FuncType(ref func_type) => {
            typecheck_func_type(func_type, expected_type, ctx, world)
        },
        ExprKind::FuncTerm(ref func_type) => {
            typecheck_func_term(func_type, expected_type, ctx, world)
        },
        ExprKind::FuncApp(ref func_app) => {
            typecheck_func_app(func_app, expr.span, expected_type, ctx, world)
        },
        /*
        ExprKind::LetExpr(ref let_expr) => {
            typecheck_let_expr(let_expr, expected_type, ctx, expr.span, world)
        },
        */
        _ => unimplemented!(),
    }
}

pub fn typeget_expr<'c, 'p: 'c>(
        expr: &'p Expr,
        ctx: Ctx<'c, 'p>,
        world: &World) -> Result<(Term, Term), TypeCheckError>
{
    match expr.kind {
        ExprKind::Parens(ref e) => typeget_expr(e, ctx, world),
        /*
        ExprKind::StructTerm(ref st) => {
            
        },
        */
        ExprKind::Typed(ref typed) => {
            typeget_typed(typed, ctx, world)
        },
        ExprKind::Variable(ref ident) => {
            typeget_variable(ident, ctx, world)
        },
        _ => return Err(TypeCheckError::UnknownType {
            span: expr.span,
        }),
    }
}
*/

