use std::cmp::Ordering::*;

use core::Ctx;
use core::Term;
use core::TermKind;
use core::Rank;
use core::RankKind;
use core::term::World;
use core::fold;
use core::fold_rank;
use core::reduce_head;
use core::fold::{contextual_noop, rank_ref_func_noop};

use render::debug_render_term;

/*
pub fn substitute_meta(term: &Term, meta_index: usize, sub: &Term) -> Term {
    fn term_func(term: Term, _: &mut (), state: &mut (usize, &Term)) -> Result<Term, !> {
        let (meta_index, sub) = *state;
        let ret = match *term {
            TermKind::MetaVar(i) => {
                match i == meta_index {
                    true    => sub.clone(),
                    false   => term,
                }
            },
            _ => term,
        };
        Ok(ret)
    }

    let Ok(ret) = fold(term, &mut (), &mut (meta_index, sub), contextual_noop, term_func, rank_ref_func_noop);
    ret
}


pub fn substitute_rank_meta(term: &Term, meta_index: usize, sub: &Rank) -> Term {
    fn rank_func(rank: &Rank, &mut (meta_index, sub): &mut (usize, &Rank)) -> Result<Rank, !> {
        Ok(rank_substitute_rank_meta(rank, meta_index, sub))
    }
    let Ok(ret) = fold(term, &mut (), &mut (meta_index, sub), contextual_noop, term_func_noop, rank_func);
    ret
}

pub fn rank_substitute_rank_meta(rank: &Rank, meta_index: usize, sub: &Rank) -> Rank {
    fn rank_func(rank: Rank, &mut (meta_index, sub): &mut (usize, &Rank)) -> Result<Rank, !> {
        let ret = match *rank {
            RankKind::MetaVar(i) => {
                match i == meta_index {
                    true    => sub.clone(),
                    false   => rank,
                }
            }
            _ => rank,
        };
        Ok(ret)
    };
    let Ok(ret) = fold_rank(rank, &mut (meta_index, sub), rank_func);
    ret
}
*/

pub fn normalise(term: &Term, mut world: &World) -> Term {
    println!("normalising: {}", debug_render_term(&Ctx::nil(), term));
    fn term_func(term: Term, _: &mut (), world: &mut &World) -> Result<Term, !> {
        println!("   before reducing: {}", debug_render_term(&Ctx::nil(), &term));
        let ret = reduce_head(&term, *world);
        println!("    after reducing: {}", debug_render_term(&Ctx::nil(), &ret));
        Ok(ret)
    }
    let Ok(ret) = fold(term, &mut (), &mut world, contextual_noop, term_func, rank_ref_func_noop);
    println!("got: {}", debug_render_term(&Ctx::nil(), &ret));
    ret
}

pub fn substitute(term: &Term, sub: &Term, index: usize) -> Term {
    fn bump(state: &mut (Term, usize)) -> (Term, usize) {
        let (ref sub, index) = *state;
        let sub = bump_index(sub, 0);
        (sub, index + 1)
    }
    fn term_func(term: Term, state: &mut (Term, usize), _: &mut ()) -> Result<Term, !> {
        let (ref sub, index) = *state;
        let ret = match *term {
            TermKind::Var(i) => {
                match i.cmp(&index) {
                    Less => term,
                    Equal => sub.clone(),
                    Greater => {
                        let kind = TermKind::Var(i - 1);
                        Term::new(kind, term.origin.clone()) //, term.ctx.clone(), term.ty.clone())
                    },
                }
            },
            _ => term,
        };
        Ok(ret)
    }

    println!("substituting {} into {} at {}", debug_render_term(&Ctx::nil(), sub), debug_render_term(&Ctx::nil(), term), index);
    let Ok(ret) = fold(term, &mut (sub.clone(), index), &mut (), bump, term_func, rank_ref_func_noop);
    ret
}

/// Bump all the index of the variables in a term by 1, ignoring variables whose index is
/// less than `cutoff`. This is hygenic when it recurses into subcontexts (ie. cutoff is adjusted
/// appropriately).
///
/// # Example
/// ```ignore
/// bump_index(`(Var(0), Var(1), Var(2))`, 1) => `(Var(0), Var(2), Var(3))`
/// bump_index(`(Var(0), FuncTerm(Var(0))))`, 0) => `(Var(1), FuncTerm(Var(0)))`
/// ```
pub fn bump_index(term: &Term, mut cutoff: usize) -> Term {
    fn bump(cutoff: &mut usize) -> usize {
        *cutoff + 1
    }
    fn term_func(term: Term, cutoff: &mut usize, _: &mut ()) -> Result<Term, !> {
        let ret = match *term {
            TermKind::Var(i) => {
                match i < *cutoff {
                    true => term,
                    false => {
                        let kind = TermKind::Var(i + 1);
                        Term::new(kind, term.origin.clone()) //, term.ctx.clone(), term.ty.clone())
                    },
                }
            },
            _ => term,
        };
        Ok(ret)
    }

    let Ok(ret) = fold(term, &mut cutoff, &mut (), bump, term_func, rank_ref_func_noop);
    ret
}

pub fn rank_meta_free(rank: &Rank) -> bool {
    fn rank_func(rank: Rank, _: &mut ()) -> Result<Rank, ()> {
        match *rank {
            RankKind::MetaVar(..) => Err(()),
            _ => Ok(rank)
        }
    };
    fold_rank(rank, &mut (), rank_func).is_ok()
}

pub fn meta_free(term: &Term) -> bool {
    fn term_func(term: Term, _: &mut (), _: &mut ()) -> Result<Term, ()> {
        match *term {
            TermKind::MetaVar(..) => Err(()),
            _ => Ok(term),
        }
    }
    fn rank_func(rank: &Rank, _: &mut ()) -> Result<Rank, ()> {
        match rank_meta_free(rank) {
            true    => Ok(rank.clone()),
            false   => Err(()),
        }
    }

    fold(term, &mut (), &mut (), contextual_noop, term_func, rank_func).is_ok()
}

pub fn try_lower_index(term: &Term, mut cutoff: usize) -> Option<Term> {
    fn bump(cutoff: &mut usize) -> usize {
        *cutoff + 1
    }
    fn term_func(term: Term, cutoff: &mut usize, _: &mut ()) -> Result<Term, ()> {
        let ret = match *term {
            TermKind::Var(i) => {
                match i.cmp(cutoff) {
                    Less => term,
                    Equal => return Err(()),
                    Greater => {
                        let kind = TermKind::Var(i - 1);
                        Term::new(kind, term.origin.clone()) //, term.ctx.clone(), term.ty.clone())
                    },
                }
            },
            _ => term,
        };
        Ok(ret)
    }

    fold(term, &mut cutoff, &mut (), bump, term_func, rank_ref_func_noop).ok()
}

/*
pub fn contains_meta(term: &Term, mut meta_index: usize) -> bool {
    fn term_func(term: Term, _: &mut (), &mut meta_index: &mut usize) -> Result<Term, ()> {
        match *term {
            TermKind::MetaVar(i) if i == meta_index => Err(()),
            _ => Ok(term),
        }
    }
    fold(term, &mut (), &mut meta_index, contextual_noop, term_func, rank_ref_func_noop).is_err()
}
*/

