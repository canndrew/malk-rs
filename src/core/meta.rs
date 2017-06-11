use std::rc::Rc;
use core::{Ctx, Rank, RankKind, Term};
use core::TermKind::*;
use core::Origin;

//use debug::Debug;

// typechecking for malk.
// What do we actually want/need out of this?
// 
// Ability to not taint meta variables, avoid having a global context on which operations can't be
// reversed.

#[derive(Clone, Debug, Hash)]
pub struct RankMVar {
    inner: Rc<RankMVarInner>,
}

#[derive(Debug, Hash)]
struct RankMVarInner;

impl PartialEq for RankMVar {
    fn eq(&self, other: &RankMVar) -> bool {
        let a = self.id();
        let b = other.id();
        a == b
    }
}

impl Eq for RankMVar {}

impl RankMVar {
    pub fn new() -> Rank {
        let rank_m_var = RankMVar {
            inner: Rc::new(RankMVarInner),
        };
        Rank::new(RankKind::MetaVar(rank_m_var))
    }

    pub fn id(&self) -> usize {
        &self.inner as &RankMVarInner as *const RankMVarInner as usize
    }
}

#[derive(Clone, Debug, Hash)]
pub struct MVar {
    inner: Rc<MVarInner>,
}

#[derive(Debug, Hash)]
struct MVarInner {
    ty: Term,
}

impl PartialEq for MVar {
    fn eq(&self, other: &MVar) -> bool {
        let a = self.id();
        let b = other.id();
        a == b
    }
}

impl Eq for MVar {}

impl MVar {
    pub fn new(ctx: &Ctx, ty: &Term, origin: Origin) -> Term {
        fn build_term(ctx: &Ctx, ty: Term, index: usize, origin: Origin) -> Term {
            let Ctx(ref opt_ctx) = *ctx;
            match *opt_ctx {
                None => {
                    let m_var_inner = MVarInner {
                        ty: ty,
                    };
                    let m_var = MVar {
                        inner: Rc::new(m_var_inner),
                    };
                    let kind = MetaVar(m_var);
                    Term::new(kind, origin)
                },
                Some(ref ctx_node) => {
                    let arg_type = ctx_node.ty.clone();
                    let res_type = ty;
                    let func_type_kind = FuncType {
                        arg_name: ctx_node.name.clone(),
                        arg_type: arg_type.clone(),
                        res_type: res_type.clone(),
                    };
                    let func_type = Term::new(func_type_kind, origin.clone());
                    let func = build_term(&ctx_node.tail, func_type, index + 1, origin.clone());
                    let arg_kind = Var(index);
                    let arg = Term::new(arg_kind, origin.clone());
                    let kind = FuncApp {
                        func: func,
                        arg: arg,
                    };
                    Term::new(kind, origin)
                },
            }
        }
        
        build_term(ctx, ty.clone(), 0, origin)
    }

    pub fn id(&self) -> usize {
        &self.inner as &MVarInner as *const MVarInner as usize
    }

    pub fn fresh_universe(ctx: &Ctx, origin: Origin) -> Term {
        let rank = RankMVar::new();
        let level_kind = Level { rank: rank.clone() };
        let level = MVar::new(ctx, &Term::new(level_kind, origin.clone()), origin.clone());
        let kind = Type {
            rank: rank,
            level: level,
        };
        Term::new(kind, origin)
    }

    pub fn fresh_type(ctx: &Ctx, origin: Origin) -> Term {
        let universe = MVar::fresh_universe(ctx, origin.clone());
        MVar::new(ctx, &universe, origin)
    }

    pub fn ty(&self) -> Term {
        self.inner.ty.clone()
    }
}

