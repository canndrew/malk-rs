use core::{Name, Origin, Ctx, Term, TermKind};
use core::meta::MVar;
use parse::{Span, FuncTerm, SingularFuncTerm, EnumFuncTerm, EnumFuncTermLeft};
use construct::{build_opt_ident, build_operator_expr, build_pattern, ConstructError};
//use construct::{build_expr, /*build_expr_typed, */ build_pattern, ConstructError, TypedTerm};

fn dummy_origin() -> Origin {
    Origin::ExprAt(Span {
        start: 0,
        end: 0,
    })
}

pub fn build_singular_func_term(ctx: &Ctx, singular_func_term: &mut SingularFuncTerm) -> Result<Term, ConstructError> {
    let (arg_name, res) = build_singular_func_term_inner(ctx, singular_func_term)?;
    let arg_type = MVar::fresh_type(ctx, dummy_origin());
    let kind = TermKind::FuncTerm {
        arg_name: arg_name,
        arg_type: arg_type,
        res: res,
    };
    Ok(Term::new(kind, dummy_origin()))
}

pub fn build_enum_func_term(ctx: &Ctx, enum_func_term: &mut EnumFuncTerm) -> Result<Term, ConstructError> {
    let end_pos = enum_func_term.span.end;
    let res = build_enum_func_term_inner(ctx, &mut enum_func_term.left_branches[..], &mut enum_func_term.right_branch, end_pos)?;
    let arg_type = MVar::fresh_type(ctx, dummy_origin());
    let kind = TermKind::FuncTerm {
        arg_name: None,
        arg_type: arg_type,
        res: res,
    };
    Ok(Term::new(kind, dummy_origin()))
}

fn build_func_term_inner(ctx: &Ctx, func_term: &mut FuncTerm) -> Result<(Option<Name>, Term), ConstructError> {
    match *func_term {
        FuncTerm::Singular(ref mut singular_func_term) => {
            build_singular_func_term_inner(ctx, singular_func_term)
        },
        FuncTerm::Enum(ref mut enum_func_term) => {
            let end_pos = enum_func_term.span.end;
            let res = build_enum_func_term_inner(ctx, &mut enum_func_term.left_branches[..], &mut enum_func_term.right_branch, end_pos)?;
            Ok((None, res))
        },
    }
}

fn build_singular_func_term_inner(ctx: &Ctx, singular_func_term: &mut SingularFuncTerm) -> Result<(Option<Name>, Term), ConstructError> {
    let ctx_ext = build_pattern(ctx, &mut singular_func_term.pattern)?;
    let ctx = ctx_ext.extend(ctx.clone());
    let res = build_operator_expr(&ctx, &mut singular_func_term.res)?;
    let res = ctx_ext.build_elim(res);
    Ok((ctx_ext.name(), res))
}

fn build_enum_func_term_inner(
        ctx: &Ctx,
        left_branches: &mut [EnumFuncTermLeft],
        right_branch: &mut Option<Box<FuncTerm>>,
        end_pos: usize,
    ) -> Result<Term, ConstructError>
{
    let arg_kind = TermKind::Var(0);
    match left_branches.split_first_mut() {
        None => {
            match *right_branch {
                None => {
                    let kind = TermKind::NeverElim {
                        target_type: MVar::fresh_type(ctx, dummy_origin()),
                        never: Term::new(arg_kind, dummy_origin()),
                    };
                    Ok(Term::new(kind, dummy_origin()))
                },
                Some(ref mut branch) => {
                    let (_, res) = build_func_term_inner(ctx, branch)?;
                    Ok(res)
                },
            }
        },
        Some((mut first, mut rest)) => {
            let (_, on_left) = build_func_term_inner(ctx, &mut first.func)?;
            let on_right = build_enum_func_term_inner(ctx, rest, right_branch, end_pos)?;
            let kind = TermKind::EitherElim {
                either: Term::new(TermKind::Var(0), dummy_origin()),
                left_name: build_opt_ident(&first.left_name),
                left_res: on_left,
                right_res: on_right,
            };
            Ok(Term::new(kind, dummy_origin()))
        },
    }
}

