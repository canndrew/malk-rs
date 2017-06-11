use std::collections::{HashSet, HashMap};
use std::rc::{Rc, Weak};
use std::hash::{Hash, Hasher};
use std::ops::Deref;

use core::meta::{RankMVar, MVar};
use core::{Rank, RankKind, Term, TermKind, fold, fold_rank};
use core::fold::contextual_noop;

#[derive(Debug, Hash)]
pub enum ConstraintError {
}

#[derive(Debug)]
pub struct ConstraintSet {
    solutions: HashMap<MVar, MVarState>,
    rank_solutions: HashMap<RankMVar, RankMVarState>,
    constraints: HashMap<Constraint, Vec<WeakConstraint>>,
}

#[derive(Debug)]
enum MVarState {
    Solved(Term),
    Unsolved {
        constraints: HashSet<WeakConstraint>,
    },
    Error(ConstraintError),
}

#[derive(Debug)]
enum RankMVarState {
    Solved(Rank),
    Unsolved {
        constraints: HashSet<WeakConstraint>,
    },
    Error(ConstraintError),
}

#[derive(Debug)]
pub struct Constraint(Rc<ConstraintKind>);

impl PartialEq for Constraint {
    fn eq(&self, other: &Constraint) -> bool {
        let a = &*self.0 as *const ConstraintKind;
        let b = &*other.0 as *const ConstraintKind;
        a == b
    }
}

impl Eq for Constraint {}

impl Hash for Constraint {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let addr = &*self.0 as *const ConstraintKind as usize;
        addr.hash(state);
    }
}

impl Deref for Constraint {
    type Target = ConstraintKind;

    fn deref(&self) -> &ConstraintKind {
        &*self.0
    }
}

#[derive(Debug)]
pub struct WeakConstraint(Weak<ConstraintKind>);

impl PartialEq<Constraint> for WeakConstraint {
    fn eq(&self, other: &Constraint) -> bool {
        let strong = self.0.upgrade();
        if let Some(this) = strong {
            let a = &*this as *const ConstraintKind;
            let b = &*other.0 as *const ConstraintKind;
            return a == b;
        }
        false
    }
}

impl PartialEq for WeakConstraint {
    fn eq(&self, other: &WeakConstraint) -> bool {
        let strong_this = self.0.upgrade();
        let strong_other = other.0.upgrade();
        match (strong_this, strong_other) {
            (Some(this), Some(other)) => {
                let a = &*this as *const ConstraintKind;
                let b = &*other as *const ConstraintKind;
                a == b
            },
            (None, None) => true,
            _ => false,
        }
    }
}

impl Eq for WeakConstraint {}

impl Hash for WeakConstraint {
    fn hash<H: Hasher>(&self, state: &mut H) {
        if let Some(strong) = self.0.upgrade() {
            let addr = &*strong as *const ConstraintKind as usize;
            addr.hash(state);
        }
    }
}

#[derive(Debug, Hash)]
pub enum ConstraintKind {
    HasType {
        ctx: Ctx,
        term: Term,
        ty: Term,
    },

    TermsEqual {
        term0: Term,
        term1: Term,
    },

    LevelContains {
        rank_low: Rank,
        level_low: Term,
        rank_high: Rank,
        level_high: Term,
    },
}

impl ConstraintSet {
    pub fn new() -> ConstraintSet {
        ConstraintSet {
            solutions: HashMap::new(),
            rank_solutions: HashMap::new(),
            constraints: HashSet::new(),
        }
    }

    pub fn add_constraint(&mut self, constraint: Constraint, dependencies: Vec<WeakConstraint>) {
        let (metas, rank_metas) = constraint.collect_metas();
        for meta in metas {
            let mut state = self.solutions.entry(meta).or_insert_with(|| {
                MVarState::Unsolved {
                    constraints: HashSet::new(),
                }
            });
            match state {
                
            }
        }

        let mut all_dependencies_resolved = true;
        for dependency in &dependencies {
            if let Some(_) = dependency.0.upgrade() {
                all_dependencies_resolved = false;
                break;
            }
        }

        if all_dependencies_resolved {
            match constraint.kind {
                ConstraintKind::HasType { ref ctx, ref term, ref ty } => {
                    let constrain_result = has_type(self, ctx, term, ty);
                    match constrain_result {
                        ConstrainResult::Done => {
                            return;
                        },
                        ConstrainResult::
                    }
                },
                _ => unimplemented!(),
            }
        }

        // TODO: fix up meta vars
        let _ = self.constraints.insert(constraint, dependencies);
    }

    pub fn substitute_metas(&self, term: &Term) -> Term {
        let mut this = self;
        fn term_func(term: Term, _: &mut (), this: &mut &ConstraintSet) -> Result<Term, !> {
            match *term {
                TermKind::MetaVar(ref m_var) => {
                    match this.solutions.get(m_var) {
                        Some(&MVarState::Solved(ref t)) => return Ok(t.clone()),
                        // TODO: report errors
                        Some(..) => (),
                        None => (),
                    }
                },
                _ => (),
            };
            Ok(term)
        }
        fn rank_ref_func(rank: &Rank, this: &mut &ConstraintSet) -> Result<Rank, !> {
            fold_rank(rank, this, rank_func)
        }
        fn rank_func(rank: Rank, this: &mut &ConstraintSet) -> Result<Rank, !> {
            match *rank {
                RankKind::MetaVar(ref rank_m_var) => {
                    match this.rank_solutions.get(rank_m_var) {
                        Some(&RankMVarState::Solved(ref r)) => return Ok(r.clone()),
                        // TODO: report errors
                        Some(..) => (),
                        None => (),
                    }
                },
                _ => (),
            };
            Ok(rank)
        }

        let Ok(ret) = fold(term, &mut (), &mut this, contextual_noop, term_func, rank_ref_func);
        ret
    }
}

enum ConstrainResult {
    /// We could not make any progress on the constraint.
    NoProgress,

    /// Found *the* solution to a metavar.
    FoundSolution {
        m_var: MVar,
        solution: Term,
    },

    /// Found *a potential* solution to a metavar.
    /// These solutions should only be tried later when using the ConstraintSet to solve a term.
    // A method for using these:
    //  find all the metavars in term we're trying to populate/solve.
    //  recursively find all metvars related to those metavars through constraints.
    //  "related through constraints" forms an equivalence relation on this set of metvars...
    //  for each equivalence class:
    //      find the metavar with the smallest number of candidate solutions.
    //      try one of it's solution, and repeat.
    //      backtrack one level if we hit an error. This is a depth-first-search.
    CandidateSolution {
        m_var: MVar,
        candidate_solution: Term,
    },

    /// The constraint reduces to another set of constraints.
    /// If this set is empty, then the constraint has been solved.
    Reduced {
        constraints: Vec<Constraint>,
    },

    /// Trying to satisfy the constraint results in an error.
    Error(ConstraintError),
}

fn type_as_uninhabited(ty: &Term) -> Option<Term> {
    match **ty {
        NeverType => {
            return Some(Term::new(NeverElim {
                never: Term::new(Var(0)),
            }));
        },
        EitherType { ref left_name, ref left_type, ref right_type } => {
            if let Some(left_elim) = type_as_uninhabited(left_type) {
                if let Some(right_elim) = type_as_uninhabited(right_type) {
                    return Some(Term::new(EitherElim {
                        either: Term::new(Var(0)),
                        left_name: left_name.clone(),
                        left_type: left_type.clone(),
                        left_res: left_elim,
                        right_type: right_type.clone(),
                        right_res: right_elim,
                    }));
                }
            }
        },
        PairType { ref head_name, ref head_type, ref tail_type } => {
            if let Some(head_elim) = type_is_uninhabited(head_type) {
                return Some(Term::new(PairElim {
                    pair: Term::new(Var(0)),
                    res: bump_index(head_elim, 0),
                    head_name: head_name.clone(),
                    head_type: head_type.clone(),
                    tail_type: tail_type.clone(),
                }));
            }
            if let Some(tail_elim) = type_is_uninhabited(tail_type) {
                return Some(Term::new(PairElim {
                    pair: Term::new(Var(0)),
                    res: tail_elim,
                    head_name: head_name.clone(),
                    head_type: head_type.clone(),
                    tail_type: tail_type.clone(),
                }));
            }
        }
    };
    None
}

fn type_as_singleton(ty: &Term) -> Option<Term> {
    match **ty {
        UnitType => return Some(Term::new(UnitTerm)),
        PairType { ref head_name, ref head_type, ref tail_type } => {
            if let Some(head_term) = type_as_singleton(head_type) {
                let tail_type_subbed = substitute(tail_type, head_term);
                if let Some(tail_term) = type_as_singleton(tail_type_subbed) {
                    return Some(Term::new(PairTerm {
                        head_name: head_name.clone(),
                        head: head_term,
                        tail: tail_term,
                    }));
                }
            }
        },
        EitherType { ref left_name, ref left_type, ref right_type } => {
            if let Some(_) = type_as_uninhabited(left_type) {
                if let Some(right_term) = type_as_singleton(right_type) {
                    return Some(Term::new(EitherRight { val: right_term }));
                }
            }
            if let Some(_) = type_as_uninhabited(right_type) {
                if let Some(left_term) = type_as_singleton(left_type) {
                    return Some(Term::new(EitherLeft { val: left_term }));
                }
            }
        },
        FuncType { ref arg_name, ref arg_type, ref res_type } => {
            if let Some(res) = type_as_uninhabited(arg_type) {
                return Some(Term::new(FuncTerm {
                    res: res,
                }));
            }
            if let Some(res_term) = type_as_singleton(res_type) {
                return Some(Term::new(FuncTerm {
                    res: Term::new(res_term),
                }));
            }
        },
    }
}

fn has_type(cs: &mut ConstraintSet, ctx: &Ctx, term: &Term, ty: &Term) -> ConstrainResult {
    use TermKind::*;

    let ctx = cs.substitute_metas_ctx(ctx);
    let term = cs.substitute_metas(term);
    let ty = cs.substitute_metas(ty);

    let mut solutions = Vec::new();
    let mut extra_constraints = Vec::new();

    match *term {
        MetaVar(m_var) => {
            if let Some(val) = type_as_singleton(ty) {
                solutions.push(m_var, val);
            }
        },

        Level { ref rank } => {
            match *ty {
                MetaVar(m_var) => {
                    let ty_rank = RankMVar::new();
                    let ty_level = MVar::new(ctx, Term::new(Level { rank: ty_rank.clone() }));
                    let ty_solution = Term::new(Type {
                        rank: ty_rank.clone(),
                        level: ty_level.clone(),
                    });
                    candidate_solutions.push((m_var, ty_solution));
                },
                Type { rank: ref ty_rank, level: ref ty_level } => {
                    let constraint = Constraint::new(ConstraintKind::LevelContains {
                        rank_low: Rank::new(RankKind::Succ(rank.clone())),
                        level_low: Term::new(LevelZero),
                        rank_high: ty_rank,
                        level_high: ty_level,
                    });
                    extra_constraints.push(constraint);
                },
                FuncApp { .. } |
                PairElim { .. } |
                EitherElim { .. } |
                IdentElim { .. } |
                NeverElim { .. } => (),

                NeverType { .. }  |
                EitherType { .. } |
                UnitType |
                PairType { .. } |
                FuncType { .. } |
                IdentType { .. } => {
                    ...
                },

                RecType { ref rec_type } => {
                    //let rec_type_subbed = substitute(rec_type, term, 0);
                },

                _ => panic!("Malformed has_type constraint"),
            }
        },

        LevelZero => {
            match *ty {
                MetaVar(m_var) => {
                    let rank = RankMVar::new();
                    let level = Term::new(Level { rank: rank });
                    canditate_solutions.push((m_var, level));
                },
                Level { .. } => {
                    done!()
                },
            }
        },

        LevelSucc { ref pred } => {
            let constraint = Constraint::new(ConstraintKind::HasType {
                term: pred.clone(),
                ty: ty.clone(),
            });
            extra_constraints.push(constraint);
        },

        LevelMax { ref a, ref b } => {
            let constraint_a = Constraint::new(ConstraintKind::HasType {
                term: a.clone(),
                ty: ty.clone(),
            });
            let constraint_b = Constraint::new(ConstraintKind::HasType {
                term: b.clone(),
                ty: ty.clone(),
            });
            extra_constraints.push(constraint_a);
            extra_constraints.push(constraint_b);
        },

        Type { ref rank, ref level } => {
            match *ty {
                MetaVar(m_var) => {
                    let ty_rank = RankMVar::new();
                    let ty_level = MVar::new(ctx, Term::new(Level { rank: ty_rank.clone() }));
                    let ty_solution = Term::new(Type {
                        rank: ty_rank.clone(),
                        level: ty_level.clone(),
                    });
                    candidate_solutions.push((m_var, ty_solution));
                },
                Type { rank: ref sup_rank, level: ref sup_level } => {
                    let constraint = Constraint::new(ConstraintKind::Level {
                        rank_low: rank.clone(),
                        rank_high: sup_rank.clone(),
                        level_low: Term::new(TermKind::LevelSucc { pred: level.clone() }),
                        level_high: sup_level.clone(),
                    });
                    extra_constraints.push(constraint);
                },
                ...
            }
        },

        Var(i) => {
            let var_ty = ctx.lookup_type(i).unwrap();
            let constraint = Constraint::new(ConstraintKind::TypesSubequal {
                ctx: ctx.clone(),
                sub_ty: var_ty,
                sup_ty: ty.clone(),
            });
            extra_constraints.push(constraint);
        },

        FuncType { ref arg_name, ref arg_type, ref res_type } => {
            let constraint_arg = Constraint::new(ConstraintKind::HasType {
                ctx: ctx.clone(),
                term: arg_type,
                ty: ty.clone(),
            });
            let res_universe = bump_index(ty, 0);
            let res_ctx = Ctx::cons(arg_type.clone(), ctx.clone());
            let constraint_res = Constraint::new(ConstraintKind::HasType {
                ctx: res_ctx,
                term: res_type,
                ty: res_universe,
            });
            extra_constraints.push(constraint_arg);
            extra_constraints.push(constraint_res);
        },

        FuncTerm { ref res } => {

        },
    }
}

fn terms_equal(ctx: &Ctx, term0: &Term, term1: &Term) -> ConstrainResult {
    if *term0 == *term1 {
        return ConstrainResult::Reduce { constraints: vec![] };
    }

    if let MetaVar(m_var) = **term0 {
        return ConstrainResult::Solution {
            m_var: m_var,
            term: term1.clone(),
        };
    }

    if let MetaVar(m_var) = **term1 {
        return ConstrainResult::Solution {
            m_var: m_var,
            term: term0.clone(),
        };
    }

    match **term0 {
        Level { rank: ref rank0 } => {
            match **term1 {
                Level { rank: ref rank1 } => {
                    return ConstrainResult::Reduce { constraints: vec![
                        Constraint::new(ConstraintKind::RanksEqual {
                            rank0: rank0,
                            rank1: rank1,
                        }),
                    ] };
                },
            }
        },
        // and so forth

        LevelMax { a: ref a0, b: ref b0 } => {
            
        },
    }
}




/*
 *
 * TODO next:
 *
 * figure out the required structure for Constraints/ConstraintSet etc.
 * What we need to do:
 *  Solve a constraint, have it spawn a bunch of sub-constraints, and return to it later.
 *  Be able to find which constraints we want to return to when we solve a metavar.
 *  If a constraint error, get a history of all the solutions that led to the constraint and keep solving unaffected constraints.
 */




