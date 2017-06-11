use core::{Debug, Term, Type};
use parse::Ident;
use typecheck::{Context, TypeCheckError, types_match};

pub fn typecheck_variable<'p>(
    ident: &'p Ident,
    expected_type: Type,
    ctx: Context) -> Result<Term, TypeCheckError>
{
    let (index, c) = ctx.lookup_by_name(&ident.text[..])?;
    let ty = c.lookup(0);
    types_match(&ty, &expected_type, ident.span)?;
    Ok(Term::var(index, d!({ Some(ty) }), d!({ c })))
}

pub fn typeget_variable<'p>(
    ident: &'p Ident,
    ctx: Context) -> Result<(Term, Type), TypeCheckError>
{
    let (index, c) = ctx.lookup_by_name(&ident.text[..])?;
    let ty = c.lookup(0);
    Ok((Term::var(index, d!({ Some(ty.clone()) }), d!({ c })), ty))
}

/*
use core::{Term, TermKind, World, is_subtype};
use parse::{Ident};
use typecheck::{Ctx, TypeCheckError, TypeMismatch};

pub fn typecheck_variable<'c, 'p: 'c>(
        ident: &'p Ident,
        expected_type: &Term,
        ctx: Ctx<'c, 'p>,
        world: &World
    ) -> Result<Term, TypeCheckError>
{
    println!("typecheck_variable({})", ident);
    if &ident.text[..] == "Type" {
        let type_of_keyword = Term::new(TermKind::FuncType {
            arg_type: Term::new(TermKind::Level),
            res_type: Term::new(TermKind::Type {
                level: Term::new(TermKind::LevelSucc {
                    pred: Term::new(TermKind::Var(0)),
                }),
            }),
        });
        if !is_subtype(&type_of_keyword, expected_type) {
            return Err(TypeCheckError::TypeMismatch(TypeMismatch {
                span: ident.span,
                expected: expected_type.clone(),
            }));
        }
        let term_of_keyword = Term::new(TermKind::FuncTerm {
            body: Term::new(TermKind::Type {
                level: Term::new(TermKind::Var(0)),
            }),
        });
        return Ok(term_of_keyword);
    }

    let (index, var_type) = match ctx.lookup_by_name(&ident.text, world) {
        Some(v) => v,
        None => return Err(TypeCheckError::NoSuchVariable {
            name: ident.text.clone(),
        }),
    };
    println!("var_type == {:?}", var_type);
    println!("expected_type == {:?}", expected_type);
    if !is_subtype(&var_type, expected_type) {
        return Err(TypeCheckError::TypeMismatch(TypeMismatch {
            span: ident.span,
            expected: expected_type.clone(),
        }));
    };
    Ok(Term::new(TermKind::Var(index)))
}

pub fn typeget_variable<'c, 'p: 'c>(
        ident: &'p Ident,
        ctx: Ctx<'c, 'p>,
        world: &World
    ) -> Result<(Term, Term), TypeCheckError>
{
    if &ident.text[..] == "Type" {
        let type_of_keyword = Term::new(TermKind::FuncType {
            arg_type: Term::new(TermKind::Level),
            res_type: Term::new(TermKind::Type {
                level: Term::new(TermKind::LevelSucc {
                    pred: Term::new(TermKind::Var(0)),
                }),
            }),
        });
        let term_of_keyword = Term::new(TermKind::FuncTerm {
            body: Term::new(TermKind::Type {
                level: Term::new(TermKind::Var(0)),
            }),
        });
        return Ok((term_of_keyword, type_of_keyword));
    }

    let (index, var_type) = match ctx.lookup_by_name(&ident.text, world) {
        Some(v) => v,
        None => return Err(TypeCheckError::NoSuchVariable {
            name: ident.text.clone(),
        }),
    };
    let var_term = Term::new(TermKind::Var(index));
    Ok((var_term, var_type))
}
*/

