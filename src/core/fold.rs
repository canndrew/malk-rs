use core::Ctx;
use core::Term;
use core::Rank;
use core::RankKind;

pub fn contextual_noop<Contextual: Clone>(c: &mut Contextual) -> Contextual {
    c.clone()
}

pub fn ctx_func_noop<Contextual, State, Error>(c: Ctx, _: &mut Contextual, _: &mut State) -> Result<Ctx, Error> {
    Ok(c)
}

pub fn rank_func_noop<State, Error>(r: Rank, _: &mut State) -> Result<Rank, Error> {
    Ok(r)
}

pub fn term_func_noop<Contextual, State, Error>(t: Term, _: &mut Contextual, _: &mut State) -> Result<Term, Error> {
    Ok(t)
}

pub fn rank_ref_func_noop<State, Error>(r: &Rank, _: &mut State) -> Result<Rank, Error> {
    Ok(r.clone())
}

pub fn term_ref_func_noop<Contextual, State, Error>(t: &Term, _: &mut Contextual, _: &mut State) -> Result<Term, Error> {
    Ok(t.clone())
}

pub fn fold_ctx<Contextual, State, Bump, Lower, CtxFunc, TermFunc, Error>(
        ctx: &Ctx,
        contextual: &mut Contextual,
        state: &mut State,
        bump: Bump,
        mut lower: Lower,
        mut ctx_func: CtxFunc,
        mut term_func: TermFunc,
    ) -> Result<Ctx, Error>
    where CtxFunc: Copy + FnMut(Ctx, &mut Contextual, &mut State) -> Result<Ctx, Error>,
          TermFunc: Copy + FnMut(&Term, &mut Contextual, &mut State) -> Result<Term, Error>,
          Bump: Copy + FnMut(&mut Contextual) -> Contextual,
          Lower: Copy + FnMut(&mut Contextual) -> Contextual,
{
    let Ctx(ref node_opt) = *ctx;
    match *node_opt {
        None => Ok(Ctx::nil()),
        Some(ref node) => {
            let mut contextual = lower(contextual);
            let ty = term_func(&node.ty, &mut contextual, state)?;
            let tail = fold_ctx(&node.tail, &mut contextual, state, bump, lower, ctx_func, term_func)?;
            let ctx = Ctx::cons(node.name.clone(), ty, tail);
            ctx_func(ctx, &mut contextual, state)
        },
    }
}

pub fn fold_rank<State, RankFunc, Error>(rank: &Rank, state: &mut State, mut rank_func: RankFunc) -> Result<Rank, Error>
    where RankFunc: Copy + FnMut(Rank, &mut State) -> Result<Rank, Error>
{
    let pre = match **rank {
        RankKind::Zero |
        RankKind::MetaVar(..) => rank.clone(),
        RankKind::Succ(ref pred) => {
            let pred = fold_rank(pred, state, rank_func)?;
            Rank::new(RankKind::Succ(pred))
        },
        RankKind::Max(ref a, ref b) => {
            let a = fold_rank(a, state, rank_func)?;
            let b = fold_rank(b, state, rank_func)?;
            Rank::new(RankKind::Max(a, b))
        },
    };
    rank_func(pre, state)
}

pub fn fold<Contextual, State, Bump, TermFunc, RankFunc, Error>(
        term: &Term,
        contextual: &mut Contextual,
        state: &mut State,
        mut bump: Bump,
        mut term_func: TermFunc,
        mut rank_func: RankFunc,
    ) -> Result<Term, Error>
    where TermFunc: Copy + FnMut(Term, &mut Contextual, &mut State) -> Result<Term, Error>,
          RankFunc: Copy + FnMut(&Rank, &mut State) -> Result<Rank, Error>,
          Bump: Copy + FnMut(&mut Contextual) -> Contextual,
{
    use core::TermKind::*;

    let pre = match **term {
        MetaVar(..) |
        Var(..) |
        UnitType |
        UnitTerm |
        NeverType => term.clone(),
        /*
        WorldType |
        WorldTerm |
        UmType |
        UmTerm { .. } |
        */
        Level { ref rank } => {
            let kind = Level {
                rank: rank_func(rank, state)?,
            };
            Term::new(kind, term.origin.clone()) //, term.ctx.clone(), term.ty.clone())
        },

        LevelZero { ref rank } => {
            let kind = LevelZero {
                rank: rank_func(rank, state)?,
            };
            Term::new(kind, term.origin.clone()) //, term.ctx.clone(), term.ty.clone())
        },

        LevelSucc { ref pred } => {
            let kind = LevelSucc {
                pred: fold(pred, contextual, state, bump, term_func, rank_func)?,
            };
            Term::new(kind, term.origin.clone()) //, term.ctx.clone(), term.ty.clone())
        },

        LevelMax { ref a, ref b } => {
            let kind = LevelMax {
                a: fold(a, contextual, state, bump, term_func, rank_func)?,
                b: fold(b, contextual, state, bump, term_func, rank_func)?,
            };
            Term::new(kind, term.origin.clone()) //, term.ctx.clone(), term.ty.clone())
        },

        Type { ref rank, ref level } => {
            let kind = Type {
                rank: rank_func(rank, state)?,
                level: fold(level, contextual, state, bump, term_func, rank_func)?,
            };
            Term::new(kind, term.origin.clone()) //, term.ctx.clone(), term.ty.clone())
        },

        FuncType { ref arg_name, ref arg_type, ref res_type } => {
            let arg_type = fold(arg_type, contextual, state, bump, term_func, rank_func)?;
            let mut contextual = bump(contextual);
            let res_type = fold(res_type, &mut contextual, state, bump, term_func, rank_func)?;
            let kind = FuncType {
                arg_name: arg_name.clone(),
                arg_type: arg_type,
                res_type: res_type,
            };
            Term::new(kind, term.origin.clone()) //, term.ctx.clone(), term.ty.clone())
        },

        FuncTerm { ref arg_name, ref arg_type, ref res } => {
            let arg_type = fold(arg_type, contextual, state, bump, term_func, rank_func)?;
            let mut contextual = bump(contextual);
            let res = fold(res, &mut contextual, state, bump, term_func, rank_func)?;
            let kind = FuncTerm {
                arg_type: arg_type,
                arg_name: arg_name.clone(),
                res: res,
            };
            Term::new(kind, term.origin.clone()) //, term.ctx.clone(), term.ty.clone())
        },

        FuncApp { ref func, ref arg } => {
            let func = fold(func, contextual, state, bump, term_func, rank_func)?;
            let arg = fold(arg, contextual, state, bump, term_func, rank_func)?;
            let kind = FuncApp {
                func: func,
                arg: arg,
            };
            Term::new(kind, term.origin.clone()) //, term.ctx.clone(), term.ty.clone())
        },

        PairType { ref head_name, ref head_type, ref tail_type } => {
            let head_type = fold(head_type, contextual, state, bump, term_func, rank_func)?;
            let mut contextual = bump(contextual);
            let tail_type = fold(tail_type, &mut contextual, state, bump, term_func, rank_func)?;
            let kind = PairType {
                head_name: head_name.clone(),
                head_type: head_type,
                tail_type: tail_type,
            };
            Term::new(kind, term.origin.clone()) //, term.ctx.clone(), term.ty.clone())
        },

        PairTerm { ref head_name, ref head, ref tail } => {
            let kind = PairTerm {
                head_name: head_name.clone(),
                head: fold(head, contextual, state, bump, term_func, rank_func)?,
                tail: fold(tail, contextual, state, bump, term_func, rank_func)?,
            };
            Term::new(kind, term.origin.clone()) //, term.ctx.clone(), term.ty.clone())
        },

        PairElim { ref head_name, ref pair, ref res } => {
            let pair = fold(pair, contextual, state, bump, term_func, rank_func)?;
            let mut contextual = bump(contextual);
            let mut contextual = bump(&mut contextual);
            let res = fold(res, &mut contextual, state, bump, term_func, rank_func)?;
            let kind = PairElim {
                head_name: head_name.clone(),
                pair: pair,
                res: res,
            };
            Term::new(kind, term.origin.clone()) //, term.ctx.clone(), term.ty.clone())
        },

        NeverElim { ref target_type, ref never } => {
            let target_type = fold(target_type, contextual, state, bump, term_func, rank_func)?;
            let never = fold(never, contextual, state, bump, term_func, rank_func)?;
            let kind = NeverElim {
                target_type: target_type,
                never: never,
            };
            Term::new(kind, term.origin.clone()) //, term.ctx.clone(), term.ty.clone())
        },

        EitherType { ref left_name, ref left_type, ref right_type } => {
            let kind = EitherType {
                left_name: left_name.clone(),
                left_type: fold(left_type, contextual, state, bump, term_func, rank_func)?,
                right_type: fold(right_type, contextual, state, bump, term_func, rank_func)?,
            };
            Term::new(kind, term.origin.clone()) //, term.ctx.clone(), term.ty.clone())
        },

        EitherLeft { ref left_name, ref left } => {
            let kind = EitherLeft {
                left_name: left_name.clone(),
                left: fold(left, contextual, state, bump, term_func, rank_func)?,
            };
            Term::new(kind, term.origin.clone()) //, term.ctx.clone(), term.ty.clone())
        },

        EitherRight { ref right } => {
            let kind = EitherRight {
                right: fold(right, contextual, state, bump, term_func, rank_func)?,
            };
            Term::new(kind, term.origin.clone()) //, term.ctx.clone(), term.ty.clone())
        },

        EitherElim { ref either, ref left_name, ref left_res, ref right_res } => {
            let either = fold(either, contextual, state, bump, term_func, rank_func)?;
            let mut contextual = bump(contextual);
            let left_res = fold(left_res, &mut contextual, state, bump, term_func, rank_func)?;
            let right_res = fold(right_res, &mut contextual, state, bump, term_func, rank_func)?;
            let kind = EitherElim {
                either: either,
                left_name: left_name.clone(),
                left_res: left_res,
                right_res: right_res,
            };
            Term::new(kind, term.origin.clone()) //, term.ctx.clone(), term.ty.clone())
        },

        IdentType { ref a, ref b } => {
            let kind = IdentType {
                a: fold(a, contextual, state, bump, term_func, rank_func)?,
                b: fold(b, contextual, state, bump, term_func, rank_func)?,
            };
            Term::new(kind, term.origin.clone()) //, term.ctx.clone(), term.ty.clone())
        },

        IdentTerm { ref x } => {
            let kind = IdentTerm {
                x: fold(x, contextual, state, bump, term_func, rank_func)?,
            };
            Term::new(kind, term.origin.clone()) //, term.ctx.clone(), term.ty.clone())
        },

        IdentElim { ref path, ref context, ref proof } => {
            let path = fold(path, contextual, state, bump, term_func, rank_func)?;
            let mut contextual = bump(contextual);
            let proof = fold(proof, &mut contextual, state, bump, term_func, rank_func)?;
            let mut contextual = bump(&mut contextual);
            let mut contextual = bump(&mut contextual);
            let context = fold(context, &mut contextual, state, bump, term_func, rank_func)?;
            let kind = IdentElim {
                path: path,
                context: context,
                proof: proof,
            };
            Term::new(kind, term.origin.clone()) //, term.ctx.clone(), term.ty.clone())
        },

        RecType { ref rec_name, ref rec_type } => {
            let mut contextual = bump(contextual);
            let kind = RecType {
                rec_name: rec_name.clone(),
                rec_type: fold(rec_type, &mut contextual, state, bump, term_func, rank_func)?,
            };
            Term::new(kind, term.origin.clone()) //, term.ctx.clone(), term.ty.clone())
        },

        /*
        RecTerm { ref rec_term } => {
            let kind = RecTerm {
                rec_term: fold(rec_term, contextual, state, bump, term_func, rank_func)?,
            };
        },
        */

        /*
        WorldElim { intrinsic, ref world_in, ref arg, ref expr } => {
            let world_in = fold(world_in, contextual, state, bump, term_func, rank_func)?;
            let arg = fold(arg, contextual, state, bump, term_func, rank_func)?;
            let mut contextual = bump(contextual);
            let mut contextual = bump(&mut contextual);
            let expr = fold(expr, &mut contextual, state, bump, term_func, rank_func)?;
            Term::new(WorldElim {
                intrinsic: intrinsic,
                world_in: world_in,
                arg: arg,
                expr: expr,
            })
        },

        UmSucc { ref pred } => {
            Term::new(UmSucc {
                pred: fold(pred, contextual, state, bump, term_func, rank_func)?,
            })
        },

        UmElim { ref arg, ref res_type, ref on_zero, ref on_succ } => {
            let arg = fold(arg, contextual, state, bump, term_func, rank_func)?;
            let mut contextual = bump(contextual);
            let res_type = fold(res_type, &mut contextual, state, bump, term_func, rank_func)?;
            let on_zero = fold(on_zero, &mut contextual, state, bump, term_func, rank_func)?;
            let on_succ = fold(on_succ, &mut contextual, state, bump, term_func, rank_func)?;
            Term::new(UmElim {
                arg: arg,
                res_type: res_type,
                on_zero: on_zero,
                on_succ: on_succ,
            })
        },
        */
    };
    term_func(pre, contextual, state)
}

