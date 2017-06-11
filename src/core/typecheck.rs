use std::collections::BTreeSet;
use std::mem;

use core::{Term, TermKind, Rank, RankKind, Name, Ctx};
use core::{bump_index, substitute, substitute_meta, substitute_rank_meta, rank_substitute_rank_meta, ctx_substitute_meta, ctx_substitute_rank_meta, fold, fold_rank, fold_ctx};
use core::fold::{contextual_noop, ctx_func_noop, rank_ref_func_noop};
use core::ops::{meta_free, contains_meta};
use core::TermKind::*;

pub struct MetaCtx {
    metas: Vec<MetaVarInfo>,
    rank_metas: Vec<RankMetaVarInfo>,
    constraints: Vec<Constraint>,
}

impl MetaCtx {
    pub fn new() -> MetaCtx {
        MetaCtx {
            metas: Vec::new(),
            rank_metas: Vec::new(),
            constraints: Vec::new(),
        }
    }

    pub fn new_var(&mut self, ctx: Ctx, ty: Term) -> Term {
        let id = self.metas.len();

        fn build_term(ctx: &Ctx, ty: Term, id: usize, index: usize) -> Term {
            let Ctx(ref opt_ctx) = *ctx;
            match *opt_ctx {
                None => Term::new(MetaVar(id)),
                Some(ref ctx_node) => {
                    let arg_type = ctx_node.ty.clone();
                    let res_type = ty;
                    let func_type = Term::new(FuncType {
                        arg_name: ctx_node.name.clone(),
                        arg_type: arg_type.clone(),
                        res_type: res_type.clone(),
                    });
                    let func = build_term(&ctx_node.tail, func_type, id, index + 1);
                    let arg = Term::new(Var(index));
                    Term::new(FuncApp {
                        arg_name: ctx_node.name.clone(),
                        arg_type: arg_type,
                        res_type: res_type,
                        func: func,
                        arg: arg,
                    })
                },
            }
        }

        let ret = build_term(&ctx, ty.clone(), id, 0);

        self.metas.push(MetaVarInfo::Unsolved {
            ctx: ctx,
            ty: ty,
            constraints: BTreeSet::new(),
        });

        ret
    }

    pub fn new_rank_var(&mut self) -> Rank {
        let id = self.rank_metas.len();
        self.rank_metas.push(RankMetaVarInfo {
            constraints: BTreeSet::new(),
        });
        Rank::new(RankKind::MetaVar(id))
    }

    fn meta_solved(&mut self, meta_index: usize, term: Term) {
        let new_info = MetaVarInfo::Solved {
            solution: term.clone(),
        };
        let old_info = mem::replace(self.metas.get_mut(meta_index).unwrap(), new_info);
        match old_info {
            MetaVarInfo::Solved { .. } => panic!("metavar can't be solved twice!"),
            MetaVarInfo::Unsolved { constraints, .. } => {
                for id in constraints {
                    self.constraints.get_mut(id).unwrap().substitute_meta(meta_index, &term);
                }
            },
        }
        for (i, meta) in self.metas.iter_mut().enumerate() {
            if i == meta_index {
                continue;
            }
            match *meta {
                MetaVarInfo::Solved { ref mut solution } => {
                    *solution = substitute_meta(solution, meta_index, &term)
                },
                MetaVarInfo::Unsolved { ref mut ctx, ref mut ty, .. } => {
                    *ctx = ctx_substitute_meta(ctx, meta_index, &term);
                    *ty = substitute_meta(ty, meta_index, &term);
                },
            }
        }
    }

    pub fn add_constraint(&mut self, constraint: Constraint) {
        {
            struct State<'a> {
                constraint_id: usize,
                meta_ctx: &'a mut MetaCtx,
            }

            let mut state = State {
                constraint_id: self.constraints.len(),
                meta_ctx: self,
            };

            fn term_func(term: Term, _: &mut (), state: &mut State) -> Result<Term, !> {
                match *term {
                    TermKind::MetaVar(i) => {
                        match *state.meta_ctx.metas.get_mut(i).unwrap() {
                            MetaVarInfo::Solved { .. } => panic!("metavar should not be solved yet!"),
                            MetaVarInfo::Unsolved { ref mut constraints, .. } => constraints.insert(state.constraint_id),
                        };
                    },
                    _ => (),
                };
                Ok(term)
            }
            fn term_ref_func(term: &Term, _: &mut (), state: &mut State) -> Result<Term, !> {
                fold(term, &mut (), state, contextual_noop, term_func, rank_ref_func)
            }
            fn rank_func(rank: Rank, state: &mut State) -> Result<Rank, !> {
                match *rank {
                    RankKind::MetaVar(i) => {
                        state.meta_ctx.rank_metas.get_mut(i).unwrap().constraints.insert(state.constraint_id);
                    },
                    _ => (),
                }
                Ok(rank)
            }
            fn rank_ref_func(rank: &Rank, state: &mut State) -> Result<Rank, !> {
                fold_rank(rank, state, rank_func)
            }
            
            match constraint {
                Constraint::Solved => panic!("Can't add a solved constraint"),
                Constraint::Eq(ref eq_constraint) => {
                    let _ = fold_ctx(&eq_constraint.ctx, &mut (), &mut state, contextual_noop, contextual_noop, ctx_func_noop, term_ref_func);
                    let _ = fold(&eq_constraint.term0, &mut (), &mut state, contextual_noop, term_func, rank_ref_func);
                    let _ = fold(&eq_constraint.term1, &mut (), &mut state, contextual_noop, term_func, rank_ref_func);
                    let _ = fold(&eq_constraint.ty0, &mut (), &mut state, contextual_noop, term_func, rank_ref_func);
                    let _ = fold(&eq_constraint.ty1, &mut (), &mut state, contextual_noop, term_func, rank_ref_func);
                },
                Constraint::Subeq(ref subeq_constraint) => {
                    let _ = fold_ctx(&subeq_constraint.ctx, &mut (), &mut state, contextual_noop, contextual_noop, ctx_func_noop, term_ref_func);
                    let _ = fold(&subeq_constraint.ty0, &mut (), &mut state, contextual_noop, term_func, rank_ref_func);
                    let _ = fold(&subeq_constraint.ty1, &mut (), &mut state, contextual_noop, term_func, rank_ref_func);
                },
                Constraint::Level(ref level_constraint) => {
                    let _ = fold_ctx(&level_constraint.ctx, &mut (), &mut state, contextual_noop, contextual_noop, ctx_func_noop, term_ref_func);
                    let _ = fold(&level_constraint.level0, &mut (), &mut state, contextual_noop, term_func, rank_ref_func);
                    let _ = fold(&level_constraint.level1, &mut (), &mut state, contextual_noop, term_func, rank_ref_func);
                },
                Constraint::Rank(ref rank_constraint) => {
                    let _ = fold_rank(&rank_constraint.rank0, &mut state, rank_func);
                    let _ = fold_rank(&rank_constraint.rank1, &mut state, rank_func);
                },
            };
        }

        self.constraints.push(constraint);
    }

    pub fn function_type_of_var(&self, id: usize) -> Term {
        fn build_term(ctx: &Ctx, ty: Term) -> Term {
            let Ctx(ref opt_ctx) = *ctx;
            match *opt_ctx {
                None => ty.clone(),
                Some(ref ctx_node) => {
                    let func_ty = Term::new(FuncType {
                        arg_name: ctx_node.name.clone(),
                        arg_type: ctx_node.ty.clone(),
                        res_type: ty,
                    });
                    build_term(&ctx_node.tail, func_ty)
                },
            }
        }
        
        let meta = self.metas.get(id).unwrap();
        match *meta {
            MetaVarInfo::Solved { .. } => panic!("Should really refactor this"),
            MetaVarInfo::Unsolved { ref ctx, ref ty, .. } => {
                build_term(ctx, ty.clone())
            },
        }
    }

    pub fn fresh_universe(&mut self, ctx: &Ctx) -> Term {
        let rank = self.new_rank_var();
        let level = self.new_var(ctx.clone(), Term::new(Level { rank: rank.clone() }));
        Term::new(Type {
            rank: rank,
            level: level,
        })
    }

    fn solved_constraint(&mut self, cid: usize) {
        self.constraints[cid] = Constraint::Solved;

        // A constraints might be solved while still containing metas.
        for meta_info in &mut self.metas {
            match *meta_info {
                MetaVarInfo::Unsolved { ref mut constraints, .. } => {
                    constraints.remove(&cid);
                },
                _ => (),
            }
        }
        for rank_meta_info in &mut self.rank_metas {
            rank_meta_info.constraints.remove(&cid);
        }
    }

    pub fn solve_constraints(&mut self) -> Result<bool, TypeCheckError> {
        'outer: loop {
            let mut all_good = true;
            for cid in 0..self.constraints.len() {
                let (term0, term1, ty0, ty1) = {
                    let constraint = &self.constraints[cid];
                    match *constraint {
                        Constraint::Solved => continue,
                        Constraint::Eq(ref c) => {
                            (c.term0.clone(), c.term1.clone(), c.ty0.clone(), c.ty1.clone())
                        }
                        _ => panic!("We're only dealing with one kind of constraint at the mo"),
                    }
                };
                all_good = false;
                if self.terms_equal(&ty0, &ty1)? {
                    if self.terms_equal(&term0, &term1)? {
                        self.solved_constraint(cid);
                        continue 'outer;
                    }
                }
            }
            return Ok(all_good);
        }
    }

    /*
    /// Solve the constraints as much as possible. Returns a smaller constraint with the solved
    /// meta-vars substituted in.
    pub fn solve(&mut self, constraint: Constraint) -> Result<Constraint, TypecheckError> {
        let mut solved_children = Vec::with_capacity(constraint.children.len());
        for child in constraint.childred {
            let solved = self.solve(child)?;
            if !solved.is_redundant() {
                solved_children.push(solved);
            }
        }

        let mut term0 = self.substitute_metas(&constraint.term0);
        let mut term1 = self.substitute_metas(&constraint.term1);
        let mut ty0 = self.substitute_metas(&constraint.ty0);
        let mut ty1 = self.substitute_metas(&constraint.ty1);

        self.types_subequal(&ty1, &ty0)?;

        term0 = self.substitute_metas(&term0);
        term1 = self.substitute_metas(&term1);
        ty0 = self.substitute_metas(&ty0);
        ty1 = self.substitute_metas(&ty1);

        if meta_free(ty0) && meta_free(ty1) {
            // if we get here, we know the types match.
            self.terms_equal(&term0, &term1)?;
            
            term0 = self.substitute_metas(&term0);
            term1 = self.substitute_metas(&term1);
            ty0 = self.substitute_metas(&ty0);
            ty1 = self.substitute_metas(&ty1);
        }

        Ok(Constraint {
            ctx: constraint.ctx,
            term0: term0,
            term1: term1,
            ty0: ty0,
            ty1: ty1,
            universe: constraint.universe,
            children: solved_children,
        })
    }
    */

    /*
    pub fn types_subequal(&mut self, ctx: &Ctx, lo: &Term, hi: &Term, universe: &Term) -> Result<bool, TypeCheckError> {
        match (**lo, **hi) {
            (LevelZero, _) |
            (_, LevelZero) |
            (LevelSucc { .. }, _) |
            (_, LevelSucc { .. }) |
            (LevelMax { .. }, _) |
            (_, LevelMax { .. }) |
            (UnitTerm, _) |
            (_, UnitTerm) |
            (FuncTerm { .. }, _) |
            (_, FuncTerm { .. }) |
            (UnitTerm, _) |
            (_, UnitTerm) |
            (PairTerm { .. }, _) |
            (_, PairTerm { .. }) |
            (EitherLeft { .. }, _) |
            (_, EitherLeft { .. }) |
            (EitherRight { .. }, _) |
            (_, EitherRight { .. }) |
            (IdentTerm, _) |
            (_, IdentTerm) |
            (RecTerm, _) |
            (_, RecTerm) |
            (WorlTerm, _) |
            (_, WorlTerm) |
            (UmTerm, _) |
            (_, UmTerm) |
            (UmSucc { .. }, _) |
            (_, UmSucc { .. }) => {
                panic!("types_subequal requires lo and hi to actually be types");
            },

            // TODO: can solve if hi has no subtypes
            (MetaVar(..), _) => Ok(false),

            // TODO: can solve if lo has no supertypes
            (_, MetaVar(..)) => Ok(false),

            (Level { rank: ref lo_rank }, Level { rank: ref hi_rank }) => {
                let constraint = RankConstraint {
                    rank0: lo_rank.clone(),
                    rank1: hi_rank.clone(),
                };
                self.add_constraint(Constraint::Rank(constraint));
                Ok(false)
            },

            (Type { rank: ref lo_rank, level: ref lo_level }, Type { rank: ref hi_rank, level: ref hi_level }) => {
                let constraint = LevelConstraint {
                    ctx: ctx.clone(),
                    rank0: lo_rank.clone(),
                    rank1: hi_rank.clone(),
                    level0: lo_level.clone(),
                    level1: hi_level.clone(),
                };
                self.add_constraint(Constraint::Rank(constraint));
                Ok(false)
            },

            (Var(lo_i), Var(hi_i)) if lo_o == hi_i => {
                Ok(true)
            },

            (
                FuncType { arg_type: ref lo_arg_type, res_type: ref lo_res_type },
                FuncType { arg_type: ref hi_arg_type, res_type: ref hi_res_type }
            ) => {
                match self.types_subequal(ctx, hi_arg_type, lo_arg_type)? {
                    true =>  {
                        let ctx = Ctx::cons(lo_arg_type, ctx);
                        self.types_subequal(ctx, lo_res_type, hi_res_type)
                    },
                    false => Ok(false),
                }
            },

            (UnitType, UnitType) => Ok(true),
            (
                PairType { head_type: ref lo_head_type, tail_type: ref lo_tail_type },
                PairType { head_type: ref hi_head_type, tail_type: ref hi_tail_type }
            ) => {
                match self.types_subequal(ctx, lo_head_type, hi_head_type)? {
                    true    => {
                        let ctx = Ctx::cons(hi_head_type, ctx);
                        self.types_subequal(ctx, lo_tail_type, hi_tail_type)
                    },
                    false => Ok(false),
                }
            },
            (NeverType, NeverType) => Ok(true),
            (
                EitherType { left_type: ref lo_left_type, right_type: ref lo_right_type },
                EitherType { left_type: ref hi_left_type, right_type: ref hi_right_type },
            ) => {
                Ok(self.types_subequal(lo_left_type, hi_left_type)? &&
                   self.types_subequal(lo_right_type, hi_right_type)?)
            },
            (
                IdentType { term_type: ref lo_term_type, a: ref lo_a, b: ref lo_b },
                IdentType { term_type: ref hi_term_type, a: ref hi_a, b: ref hi_b },
            ) => {
                match self.types_subequal(ctx, lo_term_type, hi_term_type)? {
                    true => {
                        Ok(self.terms_equal(ctx, lo_a, hi_a)? &&
                           self.temms_equal(ctx, lo_b, hi_b)?)
                    },
                    false => Ok(false),
                }
            },
            (
                RecType { rec_type: ref lo_rec_type },
                RecType { rec_type: ref hi_rec_type },
            ) => {
                let ctx = Ctx::cons(universe, ctx);
                self.types_subequal(ctx, lo_rec_type, hi_rec_type)
            },
            WorldType => Ok(true),
            UmType => Ok(true),
        }
    }
    */

    // Returns Ok(true) if the are definitely equal
    //         Ok(false) if we need to do more solving
    //         Err(e) if they're not equal.
    //  The terms MUST have the same type.
    fn terms_equal(&mut self, term0: &Term, term1: &Term) -> Result<bool, TypeCheckError> {
        let ret = match (&**term0, &**term1) {
            (&MetaVar(i_0), &MetaVar(i_1)) if i_0 == i_1 => true,

            (&MetaVar(mut i), &RecType { ref rec_type, .. }) |
            (&RecType { ref rec_type, .. }, &MetaVar(mut i)) => {
                fn contextual_func(&mut var_index: &mut usize) -> usize {
                    var_index + 1
                }
                fn term_func(term: Term, &mut var_index: &mut usize, &mut meta_var_index: &mut usize) -> Result<Term, !> {
                    match *term {
                        TermKind::MetaVar(i) if i == meta_var_index => Ok(Term::new(Var(var_index))),
                        _ => Ok(term)
                    }
                }
                let Ok(solved) = fold(rec_type, &mut 0, &mut i, contextual_func, term_func, rank_ref_func_noop);
                self.meta_solved(i, solved);
                true
            },

            (&MetaVar(i), _) => {
                if contains_meta(term1, i) {
                    return Err(TypeCheckError::InfiniteTerm {
                        meta_index: i,
                        term: term1.clone(),
                    });
                }
                self.meta_solved(i, term1.clone());
                true
            },
            (_, &MetaVar(i)) => {
                if contains_meta(term0, i) {
                    return Err(TypeCheckError::InfiniteTerm {
                        meta_index: i,
                        term: term0.clone(),
                    });
                }
                self.meta_solved(i, term0.clone());
                true
            },

            (&Var(i_0), &Var(i_1)) if i_0 == i_1 => true,
            (&Level { .. }, &Level { .. }) |
            (&Type { .. }, &Type { .. }) |
            (&UnitType, &UnitType) |
            (&NeverType, &NeverType) => true,
            (
                &FuncType { arg_name: ref arg_name_0, arg_type: ref arg_type_0, res_type: ref res_type_0 },
                &FuncType { arg_name: ref arg_name_1, arg_type: ref arg_type_1, res_type: ref res_type_1 },
            ) => {
                self.names_equal(arg_name_0, arg_name_1)?;
                self.terms_equal(arg_type_0, arg_type_1)? && self.terms_equal(res_type_0, res_type_1)?
            },
            (
                &PairType { head_name: ref head_name_0, head_type: ref head_type_0, tail_type: ref tail_type_0 },
                &PairType { head_name: ref head_name_1, head_type: ref head_type_1, tail_type: ref tail_type_1 },
            ) => {
                self.names_equal(head_name_0, head_name_1)?;
                self.terms_equal(head_type_0, head_type_1)? && self.terms_equal(tail_type_0, tail_type_1)?
            },
            (
                &EitherType { left_name: ref left_name_0, left_type: ref left_type_0, right_type: ref right_type_0 },
                &EitherType { left_name: ref left_name_1, left_type: ref left_type_1, right_type: ref right_type_1 },
            ) => {
                self.names_equal(left_name_0, left_name_1)?;
                self.terms_equal(left_type_0, left_type_1)? && self.terms_equal(right_type_0, right_type_1)?
            },
            (
                &IdentType { term_type: ref term_type_0, a: ref a_0, b: ref b_0 },
                &IdentType { term_type: ref term_type_1, a: ref a_1, b: ref b_1 },
            ) => {
                self.terms_equal(term_type_0, term_type_1)? &&
                self.terms_equal(a_0, a_1)? && self.terms_equal(b_0, b_1)?
            },
            (
                &RecType { rec_name: ref rec_name_0, rec_type: ref rec_type_0 },
                &RecType { rec_name: ref rec_name_1, rec_type: ref rec_type_1 },
            ) => {
                self.names_equal(&Some(rec_name_0.clone()), &Some(rec_name_1.clone()))?;
                self.terms_equal(rec_type_0, rec_type_1)?
            },
            (&LevelZero, &LevelZero) => true,
            (&LevelSucc { pred: ref pred_0 }, &LevelSucc { pred: ref pred_1 }) => {
                self.terms_equal(pred_0, pred_1)?
            },
            (&FuncTerm { res: ref res_0 }, &FuncTerm { res: ref res_1 }) => {
                self.terms_equal(res_0, res_1)?
            },
            (&UnitTerm, &UnitTerm) => true,
            (
                &PairTerm { head: ref head_0, tail: ref tail_0 },
                &PairTerm { head: ref head_1, tail: ref tail_1 },
            ) => {
                self.terms_equal(head_0, head_1)? &&
                self.terms_equal(tail_0, tail_1)?
            },
            (&EitherLeft { val: ref val_0 }, &EitherLeft { val: ref val_1 }) => {
                self.terms_equal(val_0, val_1)?
            },
            (&EitherRight { val: ref val_0 }, &EitherRight { val: ref val_1 }) => {
                self.terms_equal(val_0, val_1)?
            },
            (&IdentTerm, &IdentTerm) => true,


            // Now for the eliminators...
            // Eliminators are tricky. Any two terms could be equal if one has an elim at the head.
            
            _ if term0 == term1 => true,
            
            (&FuncApp { .. }, _) |
            (_, &FuncApp { .. }) |
            (&PairElim { .. }, _) |
            (_, &PairElim { .. }) |
            (&NeverElim { .. }, _) |
            (_, &NeverElim { .. }) |
            (&EitherElim { .. }, _) |
            (_, &EitherElim { .. }) |
            (&IdentElim { .. }, _) |
            (_, &IdentElim { .. })
            if !meta_free(term0) || !meta_free(term1) => {
                false
            },

            // Nothing matched. They're not equal enough for us.

            _ => {
                return Err(TypeCheckError::Mismatch {
                    term0: term0.clone(),
                    term1: term1.clone(),
                })
            },
        };
        Ok(ret)
    }

    pub fn names_equal(&self, name0: &Option<Name>, name1: &Option<Name>) -> Result<(), TypeCheckError> {
        if let Some(ref n0) = *name0 {
            if let Some(ref n1) = *name1 {
                if *n0 != *n1 {
                    return Err(TypeCheckError::NameMismatch {
                        name0: n0.clone(),
                        name1: n1.clone(),
                    });
                }
            }
        }
        Ok(())
    }
}

// TODO: figure out if this scheme actually works.
// The idea here is that we can later cache the results of type-checing to allow for incremental
// compilation. But to do that, we need Term hashes to be the same when type-checking the same
// problem. Putting random meta-var ids into the Terms would screw that up so we want to generate
// ids in a way which is consistent.
pub struct MetaVarId {
    //loc_hash: u64,
    //info_hash: u64,
}

#[derive(Debug)]
pub enum MetaVarInfo {
    Unsolved {
        ctx: Ctx,
        ty: Term,
        constraints: BTreeSet<usize>,
    },
    Solved {
        solution: Term,
    },
}

#[derive(Debug)]
pub struct RankMetaVarInfo {
    constraints: BTreeSet<usize>,
}

#[derive(Debug, Clone)]
pub enum Constraint {
    Solved,
    Eq(EqConstraint),
    Subeq(SubeqConstraint),
    Level(LevelConstraint),
    Rank(RankConstraint),
}

#[derive(Debug, Clone)]
pub struct EqConstraint {
    ctx: Ctx,
    term0: Term,
    term1: Term,
    ty0: Term,
    ty1: Term,
}

#[derive(Debug, Clone)]
pub struct SubeqConstraint {
    ctx: Ctx,
    ty0: Term,
    ty1: Term,
}

#[derive(Debug, Clone)]
pub struct LevelConstraint {
    ctx: Ctx,
    level0: Term,
    level1: Term,
}

#[derive(Debug, Clone)]
pub struct RankConstraint {
    rank0: Rank,
    rank1: Rank,
}

impl Constraint {
    pub fn substitute_meta(&mut self, meta_index: usize, term: &Term) {
        match *self {
            Constraint::Solved => (),
            Constraint::Eq(ref mut eq) => eq.substitute_meta(meta_index, term),
            Constraint::Subeq(ref mut subeq) => subeq.substitute_meta(meta_index, term),
            Constraint::Level(ref mut level) => level.substitute_meta(meta_index, term),
            Constraint::Rank(..) => (),
        }
    }
}

impl EqConstraint {
    pub fn substitute_meta(&mut self, meta_index: usize, term: &Term) {
        self.ctx = ctx_substitute_meta(&self.ctx, meta_index, term);
        self.term0 = substitute_meta(&self.term0, meta_index, term);
        self.term1 = substitute_meta(&self.term1, meta_index, term);
        self.ty0 = substitute_meta(&self.ty0, meta_index, term);
        self.ty1 = substitute_meta(&self.ty1, meta_index, term);
    }

    pub fn substitute_rank_meta(&mut self, meta_index: usize, rank: &Rank) {
        self.ctx = ctx_substitute_rank_meta(&self.ctx, meta_index, rank);
        self.term0 = substitute_rank_meta(&self.term0, meta_index, rank);
        self.term1 = substitute_rank_meta(&self.term1, meta_index, rank);
        self.ty0 = substitute_rank_meta(&self.ty0, meta_index, rank);
        self.ty1 = substitute_rank_meta(&self.ty1, meta_index, rank);
    }
}

impl SubeqConstraint {
    pub fn substitute_meta(&mut self, meta_index: usize, term: &Term) {
        self.ctx = ctx_substitute_meta(&self.ctx, meta_index, term);
        self.ty0 = substitute_meta(&self.ty0, meta_index, term);
        self.ty1 = substitute_meta(&self.ty1, meta_index, term);
    }

    pub fn substitute_rank_meta(&mut self, meta_index: usize, rank: &Rank) {
        self.ctx = ctx_substitute_rank_meta(&self.ctx, meta_index, rank);
        self.ty0 = substitute_rank_meta(&self.ty0, meta_index, rank);
        self.ty1 = substitute_rank_meta(&self.ty1, meta_index, rank);
    }
}

impl LevelConstraint {
    pub fn substitute_meta(&mut self, meta_index: usize, term: &Term) {
        self.ctx = ctx_substitute_meta(&self.ctx, meta_index, term);
        self.level0 = substitute_meta(&self.level0, meta_index, term);
        self.level1 = substitute_meta(&self.level1, meta_index, term);
    }

    pub fn substitute_rank_meta(&mut self, meta_index: usize, rank: &Rank) {
        self.ctx = ctx_substitute_rank_meta(&self.ctx, meta_index, rank);
        self.level0 = substitute_rank_meta(&self.level0, meta_index, rank);
        self.level1 = substitute_rank_meta(&self.level1, meta_index, rank);
    }
}

impl RankConstraint {
    pub fn substitute_rank_meta(&mut self, meta_index: usize, rank: &Rank) {
        self.rank0 = rank_substitute_rank_meta(&self.rank0, meta_index, rank);
        self.rank1 = rank_substitute_rank_meta(&self.rank1, meta_index, rank);
    }
}

#[derive(Debug)]
pub enum TypeCheckError {
    NameMismatch {
        name0: Name,
        name1: Name,
    },
    Mismatch {
        term0: Term,
        term1: Term,
    },
    UnsolvedConstraints {
        //constraints: BTreeSet<Constraint>,
    },
    InfiniteTerm {
        meta_index: usize,
        term: Term,
    },
}

/// Check that the term has the given type under the given context and meta-context.
/// 
/// Produces a constraint which must be solved to finish type-checking. `term0` of the constraint
/// is a term of type `ty` which will be equal to `term` if the constraints can be solved.
fn gen_constraints(meta_ctx: &mut MetaCtx, ctx: &Ctx, term: &Term, ty: &Term) -> Term {
    let term1;
    let ty1;
    match **term {
        MetaVar(i) => {
            term1 = term.clone();
            ty1 = meta_ctx.function_type_of_var(i).clone();
        },
        /*
        Bump { .. } |
        Substitute { .. } => {
            panic!("suspect I actually need to get rid of these and do what the paper does with meta vars having no context");
        },
        */
        Level { ref rank } => {
            term1 = term.clone();
            ty1 = Term::new(Type {
                rank: Rank::new(RankKind::Succ(rank.clone())),
                level: Term::new(LevelZero),
            });
        },
        LevelZero => {
            let rank = meta_ctx.new_rank_var();
            term1 = term.clone();
            ty1 = Term::new(Level { rank: rank });
        },
        LevelSucc { ref pred } => {
            let rank = meta_ctx.new_rank_var();
            let level_ty = Term::new(Level { rank: rank });
            let pred= gen_constraints(meta_ctx, ctx, pred, &level_ty);
            term1 = Term::new(LevelSucc {
                pred: pred,
            });
            ty1 = level_ty;
        },
        LevelMax { ref a, ref b } => {
            let rank = meta_ctx.new_rank_var();
            let level_ty = Term::new(Level { rank: rank });
            let a = gen_constraints(meta_ctx, ctx, a, &level_ty);
            let b = gen_constraints(meta_ctx, ctx, b, &level_ty);
            term1 = Term::new(LevelMax {
                a: a,
                b: b,
            });
            ty1 = level_ty;
        },
        Type { ref rank, ref level } => {
            let level_ty = Term::new(Level { rank: rank.clone() });
            let level = gen_constraints(meta_ctx, ctx, level, &level_ty);
            term1 = Term::new(Type {
                rank: rank.clone(),
                level: level.clone(),
            });
            ty1 = Term::new(Type {
                rank: rank.clone(),
                level: Term::new(LevelSucc {
                    pred: level,
                }),
            });
        },
        Var(i) => {
            term1 = term.clone();
            ty1 = ctx.type_of_var(i).unwrap();
        },
        FuncType { ref arg_name, ref arg_type, ref res_type } => {
            let universe = meta_ctx.fresh_universe(ctx);
            let arg_type = gen_constraints(meta_ctx, ctx, arg_type, &universe);
            let res_ctx = Ctx::cons(arg_name.clone(), arg_type.clone(), ctx.clone());
            let bumped_universe = bump_index(&universe, 0);
            let res_type = gen_constraints(meta_ctx, &res_ctx, res_type, &bumped_universe);

            term1 = Term::new(FuncType {
                arg_name: arg_name.clone(),
                arg_type: arg_type,
                res_type: res_type,
            });
            ty1 = universe;
        },
        FuncTerm { ref res } => {
            let universe = meta_ctx.fresh_universe(ctx);
            let arg_ty = meta_ctx.new_var(ctx.clone(), universe.clone());
            let res_ctx = Ctx::cons(None, arg_ty.clone(), ctx.clone());
            let bumped_universe = bump_index(&universe, 0);
            let res_ty = meta_ctx.new_var(res_ctx.clone(), bumped_universe);
            let res = gen_constraints(meta_ctx, &res_ctx, res, &res_ty);

            term1 = Term::new(FuncTerm {
                res: res,
            });
            ty1 = Term::new(FuncType {
                arg_name: None,
                arg_type: arg_ty,
                res_type: res_ty,
            });
        },
        FuncApp { ref func, ref arg, ref arg_name, ref arg_type, ref res_type } => {
            let universe = meta_ctx.fresh_universe(ctx);

            let arg_type = gen_constraints(meta_ctx, ctx, arg_type, &universe);
            let res_ctx = Ctx::cons(arg_name.clone(), arg_type.clone(), ctx.clone());
            let bumped_universe = bump_index(&universe, 0);
            let res_type = gen_constraints(meta_ctx, &res_ctx, res_type, &bumped_universe);

            let func_type = Term::new(FuncType {
                arg_name: arg_name.clone(),
                arg_type: arg_type.clone(),
                res_type: res_type.clone(),
            });

            let func = gen_constraints(meta_ctx, ctx, func, &func_type);
            let arg = gen_constraints(meta_ctx, ctx, arg, &arg_type);

            ty1 = substitute(&res_type, &arg, 0);
            term1 = Term::new(FuncApp {
                func: func,
                arg: arg,
                arg_name: arg_name.clone(),
                arg_type: arg_type,
                res_type: res_type,
            });
        },
        UnitType => {
            term1 = term.clone();
            ty1 = Term::new(Type {
                rank: Rank::new(RankKind::Zero),
                level: Term::new(LevelZero),
            });
        },
        UnitTerm => {
            term1 = term.clone();
            ty1 = Term::new(UnitType);
        },
        PairType { ref head_name, ref head_type, ref tail_type } => {
            let universe = meta_ctx.fresh_universe(ctx);
            let head_type = gen_constraints(meta_ctx, ctx, head_type, &universe);
            let tail_ctx = Ctx::cons(head_name.clone(), head_type.clone(), ctx.clone());
            let bumped_universe = bump_index(&universe, 0);
            let tail_type = gen_constraints(meta_ctx, &tail_ctx, tail_type, &bumped_universe);

            term1 = Term::new(PairType {
                head_name: head_name.clone(),
                head_type: head_type,
                tail_type: tail_type,
            });
            ty1 = universe;
        },
        PairTerm { ref head, ref tail } => {
            let universe = meta_ctx.fresh_universe(ctx);
            let head_type = meta_ctx.new_var(ctx.clone(), universe.clone());
            let tail_ctx = Ctx::cons(None, head_type.clone(), ctx.clone());
            let bumped_universe = bump_index(&universe, 0);
            let tail_type = meta_ctx.new_var(tail_ctx, bumped_universe);

            let head = gen_constraints(meta_ctx, ctx, head, &head_type);
            let subbed_tail_type = substitute(&tail_type, &head, 0);
            let tail = gen_constraints(meta_ctx, ctx, tail, &subbed_tail_type);

            term1 = Term::new(PairTerm {
                head: head,
                tail: tail,
            });
            ty1 = Term::new(PairType {
                head_name: None,
                head_type: head_type,
                tail_type: tail_type,
            });
        },
        PairElim { ref pair, ref res, ref head_name, ref head_type, ref tail_type } => {
            let universe = meta_ctx.fresh_universe(ctx);
            let head_type = gen_constraints(meta_ctx, ctx, head_type, &universe);
            let tail_ctx = Ctx::cons(head_name.clone(), head_type.clone(), ctx.clone());
            let bumped_universe = bump_index(&universe, 0);
            let tail_type = gen_constraints(meta_ctx, &tail_ctx, tail_type, &bumped_universe);

            let res_ctx = Ctx::cons(None, tail_type.clone(), tail_ctx);
            let res_universe = meta_ctx.fresh_universe(&res_ctx);
            let res_type = meta_ctx.new_var(res_ctx.clone(), res_universe);
            let res = gen_constraints(meta_ctx, &res_ctx, res, &res_type);

            let pair_type = Term::new(PairType {
                head_name: head_name.clone(),
                head_type: head_type.clone(),
                tail_type: tail_type.clone(),
            });
            let pair = gen_constraints(meta_ctx, ctx, pair, &pair_type);

            term1 = Term::new(PairElim {
                pair: pair.clone(),
                res: res,
                head_name: head_name.clone(),
                head_type: head_type.clone(),
                tail_type: tail_type.clone(),
            });
            ty1 = Term::new(PairElim {
                pair: pair,
                res: res_type,
                head_name: head_name.clone(),
                head_type: head_type,
                tail_type: tail_type,
            });
        },
        NeverType => {
            term1 = term.clone();
            ty1 = Term::new(Type {
                rank: Rank::new(RankKind::Zero),
                level: Term::new(LevelZero),
            });
        },
        NeverElim { ref never } => {
            let universe = meta_ctx.fresh_universe(ctx);
            let res_type = meta_ctx.new_var(ctx.clone(), universe.clone());
            let never_type = Term::new(NeverType);
            let never = gen_constraints(meta_ctx, ctx, never, &never_type);
            term1 = Term::new(NeverElim {
                never: never,
            });
            ty1 = res_type;
        },
        EitherType { ref left_name, ref left_type, ref right_type } => {
            let universe = meta_ctx.fresh_universe(ctx);
            let left_type = gen_constraints(meta_ctx, ctx, left_type, &universe);
            let right_type = gen_constraints(meta_ctx, ctx, right_type, &universe);

            term1 = Term::new(EitherType {
                left_name: left_name.clone(),
                left_type: left_type,
                right_type: right_type,
            });
            ty1 = universe;
        },
        EitherLeft { ref val } => {
            let universe = meta_ctx.fresh_universe(ctx);
            let left_type = meta_ctx.new_var(ctx.clone(), universe.clone());
            let right_type = meta_ctx.new_var(ctx.clone(), universe.clone());

            let val = gen_constraints(meta_ctx, ctx, val, &left_type);

            term1 = Term::new(EitherLeft {
                val: val,
            });
            ty1 = Term::new(EitherType {
                left_name: None,
                left_type: left_type,
                right_type: right_type,
            });
        },
        EitherRight { ref val } => {
            let universe = meta_ctx.fresh_universe(ctx);
            let left_type = meta_ctx.new_var(ctx.clone(), universe.clone());
            let right_type = meta_ctx.new_var(ctx.clone(), universe.clone());

            let val = gen_constraints(meta_ctx, ctx, val, &right_type);

            term1 = Term::new(EitherRight {
                val: val,
            });
            ty1 = Term::new(EitherType {
                left_name: None,
                left_type: left_type,
                right_type: right_type,
            });
        },
        EitherElim { ref either, ref left_name, ref left_type, ref right_type, ref left_res, ref right_res } => {
            let universe = meta_ctx.fresh_universe(ctx);
            let res_universe = meta_ctx.fresh_universe(ctx);

            let left_type = gen_constraints(meta_ctx, ctx, left_type, &universe);
            let left_ctx = Ctx::cons(left_name.clone(), left_type.clone(), ctx.clone());
            let left_res_type = meta_ctx.new_var(left_ctx.clone(), res_universe.clone());
            let left_res = gen_constraints(meta_ctx, &left_ctx, left_res, &left_res_type);

            let right_type = gen_constraints(meta_ctx, ctx, right_type, &universe);
            let right_ctx = Ctx::cons(None, right_type.clone(), ctx.clone());
            let right_res_type = meta_ctx.new_var(right_ctx.clone(), res_universe.clone());
            let right_res = gen_constraints(meta_ctx, &right_ctx, right_res, &right_res_type);

            let either_type = Term::new(EitherType {
                left_name: left_name.clone(),
                left_type: left_type.clone(),
                right_type: right_type.clone(),
            });
            let either = gen_constraints(meta_ctx, ctx, either, &either_type);

            term1 = Term::new(EitherElim {
                either: either.clone(),
                left_name: left_name.clone(),
                left_type: left_type.clone(),
                right_type: right_type.clone(),
                left_res: left_res,
                right_res: right_res,
            });
            ty1 = Term::new(EitherElim {
                either: either,
                left_name: left_name.clone(),
                left_type: left_type,
                right_type: right_type,
                left_res: left_res_type,
                right_res: right_res_type,
            });
        },
        IdentType { ref term_type, ref a, ref b } => {
            let universe = meta_ctx.fresh_universe(ctx);
            
            let term_type = gen_constraints(meta_ctx, ctx, term_type, &universe);
            let a = gen_constraints(meta_ctx, ctx, a, &term_type);
            let b = gen_constraints(meta_ctx, ctx, b, &term_type);

            term1 = Term::new(IdentType {
                term_type: term_type,
                a: a,
                b: b,
            });
            ty1 = universe;
        },
        IdentTerm => {
            let universe = meta_ctx.fresh_universe(ctx);
            let x_type = meta_ctx.new_var(ctx.clone(), universe.clone());
            let x = meta_ctx.new_var(ctx.clone(), x_type.clone());
            
            term1 = term.clone();
            ty1 = Term::new(IdentType {
                term_type: x_type,
                a: x.clone(),
                b: x.clone(),
            });
        },
        IdentElim { ref term_type, ref a, ref b, ref path, ref context, ref proof } => {
            let universe = meta_ctx.fresh_universe(ctx);
            
            let term_type = gen_constraints(meta_ctx, ctx, term_type, &universe);
            let a = gen_constraints(meta_ctx, ctx, a, &term_type);
            let b = gen_constraints(meta_ctx, ctx, b, &term_type);

            let path_type = Term::new(IdentType {
                term_type: term_type.clone(),
                a: a.clone(),
                b: b.clone(),
            });
            let path = gen_constraints(meta_ctx, ctx, path, &path_type);

            let context_ctx = ctx.clone();
            let context_ctx = Ctx::cons(None, term_type.clone(), context_ctx);
            let context_ctx = Ctx::cons(None, term_type.clone(), context_ctx);
            let context_ctx = Ctx::cons(None, Term::new(IdentType {
                term_type: term_type.clone(),
                a: Term::new(Var(1)),
                b: Term::new(Var(0)),
            }), context_ctx);
            let context_universe = meta_ctx.fresh_universe(&context_ctx);
            let context = gen_constraints(meta_ctx, &context_ctx, context, &context_universe);

            let proof_ctx = Ctx::cons(None, term_type.clone(), ctx.clone());
            let proof_type = bump_index(&context, 0);
            let proof_type = substitute(&proof_type, &Term::new(Var(0)), 2);
            let proof_type = substitute(&proof_type, &Term::new(Var(0)), 1);
            let proof_type = substitute(&proof_type, &Term::new(IdentTerm), 0);
            let proof = gen_constraints(meta_ctx, &proof_ctx, proof, &proof_type);

            let res_type = context.clone();
            let res_type = substitute(&res_type, &a, 2);
            let res_type = substitute(&res_type, &b, 1);
            let res_type = substitute(&res_type, &path, 0);

            term1 = Term::new(IdentElim {
                term_type: term_type,
                a: a,
                b: b,
                path: path,
                context: context,
                proof: proof,
            });
            ty1 = res_type;
        },
        RecType { ref rec_name, ref rec_type } => {
            let universe = meta_ctx.fresh_universe(ctx);
            let rec_type_ctx = Ctx::cons(Some(rec_name.clone()), universe.clone(), ctx.clone());
            let rec_type = gen_constraints(meta_ctx, &rec_type_ctx, rec_type, &universe);
            
            term1 = Term::new(RecType {
                rec_name: rec_name.clone(),
                rec_type: rec_type,
            });
            ty1 = universe;
        },
        _ => unimplemented!(),
    };
    let res = meta_ctx.new_var(ctx.clone(), ty.clone());
    let eq_constraint = EqConstraint {
        ctx: ctx.clone(),
        term0: res.clone(),
        term1: term1,
        ty0: ty.clone(),
        ty1: ty1,
    };
    let constraint = Constraint::Eq(eq_constraint);
    meta_ctx.add_constraint(constraint);
    res
}

pub fn typecheck(meta_ctx: &mut MetaCtx, ctx: &Ctx, term: &Term, ty: &Term) -> Result<Term, TypeCheckError> {
    let meta_var_index = match *gen_constraints(meta_ctx, ctx, term, ty) {
        MetaVar(i) => i,
        _ => panic!("That's not supposed to happen!"),
    };
    let _ = meta_ctx.solve_constraints()?;
    match meta_ctx.metas[meta_var_index] {
        MetaVarInfo::Unsolved { .. } => {
            Err(TypeCheckError::UnsolvedConstraints {
                //constraints: constraints.clone(),
            })
        },
        MetaVarInfo::Solved { ref solution } => {
            Ok(solution.clone())
        },
    }
}

/*
 *
 * TODO next:
 *
 * Implement a function to solve a constraint-set by simple pattern-matching on two terms.
 * Can leave-out rank and level solving for a while, it would juest mean Type : Type
 *
 *
 */


