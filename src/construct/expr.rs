use core::{Ctx, Term, TermKind, /* Rank, RankKind, */ Origin};
use parse::*;
use construct::ConstructError;
use construct::{build_ident, build_singular_func_term, build_enum_term, build_enum_func_term, build_struct_term, build_struct_type, build_enum_type, build_let_expr, build_func_app};
//use construct::{TypedTerm, ConstructError, build_pattern, build_ident, build_opt_ident, build_func_term, build_func_app, /*build_func_term_typed, */ build_struct_term, /*build_struct_term_typed*/};

/*
pub fn build_expr_typed(meta_ctx: &mut MetaCtx, ctx: &Ctx, expr: &Expr) -> Result<TypedTerm, ConstructError> {
    match expr.kind {
        ExprKind::Parens(ref sub_expr) => build_expr_typed(meta_ctx, ctx, sub_expr),
        ExprKind::Variable(ref ident) => build_variable_typed(meta_ctx, ctx, ident),
        ExprKind::FuncTerm(ref func_term) => build_func_term_typed(meta_ctx, ctx, func_term),
        ExprKind::StructTerm(ref struct_term) => build_struct_term_typed(meta_ctx, ctx, struct_term),
        ExprKind::FuncApp(ref func_app) => build_func_app_typed(meta_ctx, ctx, func_app),
        ExprKind::LetExpr(..) |
        ExprKind::EnumTerm(..) |
        ExprKind::Numeric(..) |
        ExprKind::Typed(..) => unimplemented!(),
        _ => {
            Ok(TypedTerm {
                term: build_expr(meta_ctx, ctx, expr)?,
                ty: meta_ctx.fresh_universe(ctx),
            })
        },
    }
}
*/

pub fn build_block_expr(ctx: &Ctx, expr: &mut Expr<BlockPrecedenceExprKind>) -> Result<Term, ConstructError> {
    let term = build_block_expr_kind(ctx, &mut expr.kind)?;
    expr.term = Some(term.clone());
    expr.ctx = Some(ctx.clone());
    Ok(term)
}

pub fn build_operator_expr(ctx: &Ctx, expr: &mut Expr<OperatorPrecedenceExprKind>) -> Result<Term, ConstructError> {
    let term = build_operator_expr_kind(ctx, &mut expr.kind)?;
    expr.term = Some(term.clone());
    expr.ctx = Some(ctx.clone());
    Ok(term)
}

pub fn build_app_expr(ctx: &Ctx, expr: &mut Expr<AppPrecedenceExprKind>) -> Result<Term, ConstructError> {
    let term = build_app_expr_kind(ctx, &mut expr.kind)?;
    expr.term = Some(term.clone());
    expr.ctx = Some(ctx.clone());
    Ok(term)
}

pub fn build_enclosed_expr(ctx: &Ctx, expr: &mut Expr<EnclosedPrecedenceExprKind>) -> Result<Term, ConstructError> {
    let term = build_enclosed_expr_kind(ctx, &mut expr.kind)?;
    expr.term = Some(term.clone());
    expr.ctx = Some(ctx.clone());
    Ok(term)
}

pub fn build_block_expr_kind(ctx: &Ctx, kind: &mut BlockPrecedenceExprKind) -> Result<Term, ConstructError> {
    match *kind {
        BlockPrecedenceExprKind::LetExpr(ref mut let_expr) => build_let_expr(ctx, let_expr),
        BlockPrecedenceExprKind::OperatorPrecedence(ref mut e) => build_operator_expr_kind(ctx, e),
    }
}

pub fn build_operator_expr_kind(ctx: &Ctx, kind: &mut OperatorPrecedenceExprKind) -> Result<Term, ConstructError> {
    match *kind {
        OperatorPrecedenceExprKind::SingularFuncTerm(ref mut singular_func_term) => build_singular_func_term(ctx, singular_func_term),
        //OperatorPrecedenceExprKind::SingularFuncType(ref mut singular_func_type) => build_singular_func_type(ctx, singular_func_type),
        //OperatorPrecedenceExprKind::EqualityTerm(ref mut equality_term) => build_equality_term(ctx, equality_term),
        //OperatorPrecedenceExprKind::EqualityType(ref mut equality_type) => build_equality_type(ctx, equality_type),
        //OperatorPrecedenceExprKind::Typed(ref mut typed) => build_typed(ctx, typed),
        OperatorPrecedenceExprKind::AppPrecedence(ref mut e) => build_app_expr_kind(ctx, e),
        _ => unimplemented!(),
    }
}

pub fn build_app_expr_kind(ctx: &Ctx, kind: &mut AppPrecedenceExprKind) -> Result<Term, ConstructError> {
    match *kind {
        AppPrecedenceExprKind::FuncApp(ref mut func_app) => build_func_app(ctx, func_app),
        AppPrecedenceExprKind::EnclosedPrecedence(ref mut e) => build_enclosed_expr_kind(ctx, e),
    }
}

pub fn build_enclosed_expr_kind(ctx: &Ctx, kind: &mut EnclosedPrecedenceExprKind) -> Result<Term, ConstructError> {
    match *kind {
        EnclosedPrecedenceExprKind::EnumFuncTerm(ref mut enum_func_term) => build_enum_func_term(ctx, enum_func_term),
        //EnclosedPrecedenceExprKind::EnumFuncType(ref mut enum_func_type) => build_enum_func_type(ctx, enum_func_type),
        EnclosedPrecedenceExprKind::Parens(ref mut parens) => build_parens(ctx, parens),
        EnclosedPrecedenceExprKind::StructTerm(ref mut struct_term) => build_struct_term(ctx, struct_term),
        EnclosedPrecedenceExprKind::StructType(ref mut struct_type) => build_struct_type(ctx, struct_type),
        EnclosedPrecedenceExprKind::EnumTerm(ref mut enum_term) => build_enum_term(ctx, enum_term),
        EnclosedPrecedenceExprKind::EnumType(ref mut enum_type) => build_enum_type(ctx, enum_type),
        EnclosedPrecedenceExprKind::Variable(ref mut variable) => build_variable(ctx, variable),
        EnclosedPrecedenceExprKind::IntLiteral(ref mut int_literal) => build_int_literal(int_literal),
        //EnclosedPrecedenceExprKind::TypeLiteral(ref mut type_literal) => build_type_literal(ctx, type_literal),
        //EnclosedPrecedenceExprKind::LevelLiteral(ref mut level_literal) => build_level_literal(ctx, level_literal),
        //EnclosedPrecedenceExprKind::Numeric(ref mut numeric) => build_numeric(ctx, numeric),
        _ => unimplemented!(),
    }
}

pub fn build_parens(ctx: &Ctx, parens: &mut ParensExpr) -> Result<Term, ConstructError> {
    build_block_expr(ctx, &mut parens.sub_expr)
}

/*
pub fn build_variable_typed(meta_ctx: &mut MetaCtx, ctx: &Ctx, ident: &Ident) -> Result<TypedTerm, ConstructError> {
    let name = build_ident(ident);
    let (i, ty) = match ctx.lookup_name(&name) {
        Some(x) => x,
        None => return Err(ConstructError::NoSuchVariable(ident.clone())),
    };
    Ok(TypedTerm {
        term: Term::new(TermKind::Var(i)),
        ty: ty,
    })
}
*/

pub fn build_variable(ctx: &Ctx, ident: &Ident) -> Result<Term, ConstructError> {
    let name = build_ident(ident);
    let (i, _) = match ctx.lookup_name(&name) {
        Some(x) => x,
        None => return Err(ConstructError::NoSuchVariable(ident.clone())),
    };
    let kind = TermKind::Var(i);
    Ok(Term::new(kind, Origin::ExprAt(ident.span)))
}

pub fn build_int_literal(_int_literal: &IntLiteral) -> Result<Term, ConstructError> {
    unimplemented!()
}

/*
pub fn build_enum_type(meta_ctx: &mut MetaCtx, ctx: &Ctx, enum_type: &EnumType) -> Result<Term, ConstructError> {
    fn build(meta_ctx: &mut MetaCtx, ctx: &Ctx, head_elems: &[CompositeTypeElem], tail: &Option<Expr>) -> Result<Term, ConstructError> {
        match head_elems.split_first() {
            None => {
                match *tail {
                    None => Ok(Term::new(TermKind::NeverType)),
                    Some(ref tail_expr) => {
                        build_expr(meta_ctx, ctx, tail_expr)
                    },
                }
            },
            Some((ref first, ref rest)) => {
                let left_type = build_expr(meta_ctx, ctx, &first.expr)?;
                let right_type = build(meta_ctx, ctx, rest, tail)?;
                Ok(Term::new(TermKind::EitherType {
                    left_name: build_opt_ident(&first.ident),
                    left_type: left_type,
                    right_type: right_type,
                }))
            },
        }
    }

    build(meta_ctx, ctx, &enum_type.head_elems[..], &enum_type.tail)
}
*/

/*
pub fn build_rank(rank: usize) -> Rank {
    match rank {
        0 => Rank::new(RankKind::Zero),
        _ => Rank::new(RankKind::Succ(build_rank(rank - 1))),
    }
}
*/

/*
pub fn build_level_literal(meta_ctx: &mut MetaCtx, ctx: &Ctx, level_literal: &LevelLiteral) -> Result<Term, ConstructError> {
    let rank = build_rank(level_literal.rank);
    Ok(Term::new(TermKind::Level {
        rank: rank,
    }))
}

pub fn build_type_literal(meta_ctx: &mut MetaCtx, ctx: &Ctx, type_literal: &TypeLiteral) -> Result<Term, ConstructError> {
    let rank = build_rank(type_literal.rank);
    let ty = Term::new(TermKind::Level {
        rank: rank.clone(),
    });
    Ok(Term::new(TermKind::Type {
        rank: rank,
        level: meta_ctx.new_var(ctx.clone(), ty),
    }))
}
*/

