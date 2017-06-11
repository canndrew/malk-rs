use std::rc::Rc;
use std::ops::Deref;
use std::collections::HashMap;

use core::{Name, Rank, substitute, try_lower_index};
use core::ops::normalise;
use core::meta::MVar;
use core::Origin;

//use core::Ctx;
//use debug::Debug;

use self::TermKind::*;

pub struct Intrinsic {
    pub arg_type: Term,
    pub ret_type: Term,
    pub func: fn(&Term) -> Term,
}

pub struct World {
    pub intrinsics: HashMap<String, Intrinsic>,
}

/// A pointer to a term.
#[derive(Debug, Clone, Hash)]
pub struct Term {
    pub kind: Rc<TermKind>,
    pub origin: Origin,
    //pub ctx: Debug<Ctx>,
    //pub ty: Debug<Rc<TermKind>>,
}

impl Deref for Term {
    type Target = TermKind;

    fn deref(&self) -> &TermKind {
        &self.kind
    }
}

impl Term {
    /// Create a new term.
    pub fn new(kind: TermKind, origin: Origin) -> Term { //, ctx: Debug<Ctx>, ty: Debug<Rc<TermKind>>) -> Term {
        let ret = Term {
            kind: Rc::new(kind),
            origin: origin,
            //ctx: ctx,
            //ty: ty,
        };

        /*
        #[cfg(debug_assertions)]
        catch(|| {
            fold(&term, &mut ctx, &mut (), contextual_func_noop);
        });
        */

        ret
    }
}

impl PartialEq for Term {
    fn eq(&self, other: &Term) -> bool {
        self.kind == other.kind
    }
}

/// The different kinds of term that can appear in the AST.
/// 
/// Universe polymorphism is implemented through ranks and levels such that:
///
///   Type(rank, 0) : Type(rank, 1) : Type(rank, 2) ... : Type(rank + 1, 0)
///                                         Level(rank) : Type(rank + 1, 0) 
///
/// This way, there is no "biggest type" and we can give a type to any universe.
#[derive(Debug, Clone, Hash, PartialEq)]
pub enum TermKind {
    /// A metavariable, used for typechecking and unification.
    MetaVar(MVar),

    /*
     * It's hard to make these work.
     * We can instead treat metavars as always being context-free closed terms, in which case
     * moving them between contexts is a no-op.

    Bump {
        term: Term,
        cutoff: usize,
    },

    Substitute {
        term: Term,
        sub: Term,
        index: usize,
    },
    */

    /// The type of universe levels of the given rank.
    Level {
        rank: Rank,
    },

    /// Universe level zero.
    LevelZero {
        rank: Rank,
    },

    /// Universe level (pred + 1).
    LevelSucc {
        pred: Term,
    },

    /// Universe level max(a, b)
    // NOTE: it might an idea to add ranks to a and b. That would allow type inference to be more
    // precise for things like FuncType and PairType rather than using a single universe for (eg.)
    // the universes of head_type and tail_type.
    // Would then have LevelMax(ra, rb, la, lb) = if ra > rb  => la
    //                                            if rb > ra  => lb
    //                                            if ra == rb => max(la, lb)
    // Might make the constraint-solving algorithm harder to implement though.
    LevelMax {
        a: Term,
        b: Term,
    },


    /// The type of types.
    Type {
        rank: Rank,
        level: Term,
    },

    /// A variable.
    Var(usize),

    /// The type of dependent functions. (ie pi types)
    FuncType {
        arg_name: Option<Name>,
        arg_type: Term,
        res_type: Term,
    },

    /// Functions.
    FuncTerm {
        arg_name: Option<Name>,
        arg_type: Term,
        res: Term,
    },

    /// Function application.
    FuncApp {
        func: Term,
        arg: Term,
    },

    /// The unit type.
    UnitType,

    /// The unit term.
    UnitTerm,

    /// A dependent pair type (ie. sigma type)
    PairType {
        head_name: Option<Name>,
        head_type: Term,
        tail_type: Term,
    },

    /// A pair term.
    PairTerm {
        head_name: Option<Name>,
        head: Term,
        tail: Term,
    },

    /// Dependent pair elimination.
    PairElim {
        head_name: Option<Name>,
        pair: Term,
        res: Term,
    },

    /// The canonical empty type, (ie. bottom).
    NeverType,

    /// Never type elimintator
    NeverElim {
        target_type: Term,
        never: Term,
    },

    /// Disjoint union, sum type.
    EitherType {
        left_name: Option<Name>,
        left_type: Term,
        right_type: Term,
    },

    /// Inject left.
    EitherLeft {
        left_name: Option<Name>,
        left: Term,
    },

    /// Inject right.
    EitherRight {
        right: Term,
    },

    /// Case match on a sum type.
    EitherElim {
        either: Term,
        left_name: Option<Name>,
        left_res: Term,
        right_res: Term,
    },

    /// The type of identifications (a == b)
    IdentType {
        a: Term,
        b: Term,
    },

    /// Reflexivity,
    IdentTerm {
        x: Term,
    },

    /// J-elimination for identities.
    IdentElim {
        path: Term,
        context: Term,
        proof: Term,
    },

    /// A recursive type. `rec_type` is under a context where Var(0) refers back to the type.
    RecType {
        rec_name: Name,
        rec_type: Term,
    },

    /*
     * TODO: add these. Pretty sure this is how I wanna do it.
     *
    /// The type of subterms of some term.
    SubtermType {
        rec_term: Term,
    },

    /// A recursive function. Can call itself inside itself without infinite recursion.
    RecFuncTerm {
        // A function X -> Y under a context with
        //  (a) the arg : X
        //  (b) a function Split(X, ~x) -> Y
        //      where Split(..) represents recursing into a Rec type replacing sub-occurances
        //      with ~x
        res: Term,
    },
    */

    /*
    /// A recursive type term. Used to mark `rec_term` as a member of a recursive type.
    RecTerm {
        rec_term: Term,
    },


    /// Recursive elimintation. Fold over the term `arg` of recursive type `arg_type`.
    RecElim {
        arg: Term,
        res: Term,
        arg_type: Term,
        res_type: Term,
    }
    */

    /*
    /// The type for interacting with the outside world through intrinsics.
    WorldType,

    /// The term used to represent a fully-normalised world ready to perform IO on.
    WorldTerm,

    /// Performs an intrinsic call.
    WorldElim {
        /// The name of the intrinsic
        intrinsic: &'static str,

        /// The world argument.
        world_in: Term,

        /// The argument type that the intrinsic expects.
        arg: Term,

        /// The result of the expression. Evaluated with the intrinsic result at Var(1) and another
        /// World at Var(0).
        expr: Term,
    },

    /// Unsigned, memory-sized integers.
    UmType,

    /// An intger literal.
    UmTerm {
        val: u64,
    },

    /// An integer literal equal to `1 + pred`
    UmSucc {
        pred: Term,
    },

    /// Case match on a Um.
    UmElim {
        arg: Term,
        res_type: Term,
        on_zero: Term,
        on_succ: Term,
    },
    */
}

/*
pub fn level_get_rank(level: &Term) -> Option<Rank> {
    match **level {
        MetaVar(m_var) => match m_var.ty() {
            Level { rank } => Some(rank),
            _ => None,
        },
        LevelZero { ref rank } => Some(rank.clone()),
        LevelSucc { ref pred } => level_get_rank(pred),
        LevelMax { ref a, ref b } => {
            let rank_a = match level_get_rank(a) {
                Some(rank_a) => rank_a,
                None => return None,
            };
            let rank_b = match level_get_rank(b) {
                Some(rank_b) => rank_b,
                None => return None,
            };
            let rank_max = Rank::Max(rank_a, rank_b);
            Some(rank_normalise(&rank_max))
        },
    }
}
*/

/// Normalise a term assuming all it's subterms are already normalised. Does beta/eta reduction on
/// the head of the term.
pub fn reduce_head(term: &Term, world: &World) -> Term {
    match **term {
        MetaVar(..) |
        Level { .. } |
        LevelZero { .. } |
        LevelSucc { .. } |
        FuncType { .. } |
        Var(..) |
        UnitType |
        UnitTerm |
        IdentType { .. } |
        IdentTerm { .. } |
        RecType { .. } |
        //RecTerm { .. } |
        PairType { .. } |
        NeverType |
        NeverElim { .. } |
        EitherType { .. } |
        EitherLeft { .. } |
        EitherRight { .. } |
        /*
        WorldType |
        WorldTerm |
        UmType |
        UmTerm { .. } |
        */
        Type { .. } => term.clone(),

        LevelMax { .. } => term.clone(),
        /*
             * TODO: aggressively normalise by collecting and sorting all branches
             *
        LevelMax { ref a, ref b } => {
            let rank_a = match level_get_rank(a) {
                Some(rank) => rank,
                None => return term.clone(),
            };
            let rank_b = match level_get_rank(b) {
                Some(rank) => rank,
                None => return term.clone(),
            };
            let rank_max = Rank::new(RankKind::Max(&rank_a, &rank_b));
            let rank_max = rank_normalised(&rank_max);
            match (rank_max == rank_a, rank_max == rank_b) {
                // We don't know which is bigger
                (false, false) => {
                    match (&**a, &**b) {
                        (&LevelSucc { pred: ref a_pred }, &LevelSucc { pred: ref b_pred }) => {
                            let max = Term::new(LevelMax {
                                a: a_pred.clone(),
                                b: b_pred.clone(),
                            });
                            let max = reduce_head(&max, world);
                            Term::new(LevelSucc { pred: max })
                        },
                    },
                },
            }


            match (&**a, &**b) {
                (&LevelZero { rank: ref rank_a }, &LevelZero { rank: ref rank_b }) => {
                    match max_rank(rank_a, rank_b) {
                        Some(rank_max) => Term::new(LevelZero { rank: rank_max }),
                        None => term.clone(),
                    }
                },

            }
            
            match (&**a, &**b) {
                (&LevelZero, _) => b.clone(),
                (_, &LevelZero) => a.clone(),
                (&LevelSucc { pred: ref a_pred }, &LevelSucc { pred: ref b_pred }) => {
                    let max = Term::new(LevelMax {
                        a: a_pred.clone(),
                        b: b_pred.clone(),
                    });
                    let max = reduce_head(&max, world);
                    Term::new(LevelSucc { pred: max })
                },
                _ => term.clone(),
            }
        },
        */

        FuncTerm { ref res, .. } => {
            match **res {
                FuncApp { ref func, ref arg } => {
                    match **arg {
                        Var(0) => {
                            match try_lower_index(func, 0) {
                                Some(no_arg) => no_arg,
                                None => term.clone(),
                            }
                        }
                        _ => term.clone(),
                    }
                },
                _ => term.clone(),
            }
        },

        FuncApp { ref func, ref arg } => {
            match **func {
                FuncTerm { ref res, .. } => {
                    let res = substitute(res, arg, 0);
                    normalise(&res, world)
                },
                _ => term.clone(),
            }
        },

        PairTerm { ref head, ref tail, .. } => {
            match (&**head, &**tail) {
                (&PairElim { pair: ref head_pair, res: ref head_res, .. },
                 &PairElim { pair: ref tail_pair, res: ref tail_res, .. }) => {
                    match (&**head_res, &**tail_res) {
                        (&Var(1), &Var(0)) => {
                            match **head_pair == **tail_pair {
                                true => head_pair.clone(),
                                false => term.clone(),
                            }
                        },
                        _ => term.clone(),
                    }
                },
                _ => term.clone(),
            }
        },

        PairElim { ref pair, ref res, .. } => {
            match **pair {
                PairTerm { ref head, ref tail, .. } => {
                    let res = substitute(res, tail, 0);
                    let res = substitute(&res, head, 0);
                    let res = normalise(&res, world);
                    res
                },
                _ => {
                    match try_lower_index(res, 0) {
                        Some(ref no_tail) => match try_lower_index(no_tail, 0) {
                            Some(no_head) => no_head,
                            None => term.clone(),
                        },
                        None => term.clone(),
                    }
                },
            }
        },

        EitherElim { ref either, ref left_res, ref right_res, .. } => {
            match **either {
                EitherLeft { ref left, .. } => {
                    let res = substitute(left_res, left, 0);
                    normalise(&res, world)
                },
                EitherRight { ref right } => {
                    let res = substitute(right_res, right, 0);
                    normalise(&res, world)
                },
                _ => {
                    match (try_lower_index(left_res, 0), try_lower_index(right_res, 0)) {
                        (Some(left_res_lowered), Some(right_res_lowered)) => {
                            match left_res_lowered == right_res_lowered {
                                true => left_res_lowered,
                                false => term.clone(),
                            }
                        },
                        _ => term.clone(),
                    }
                },
            }
        },

        IdentElim { ref path, ref proof, .. } => {
            match **path {
                IdentTerm { ref x } => {
                    let res = substitute(proof, x, 0);
                    normalise(&res, world)
                },
                _ => term.clone(),
            }
        },

        /*
        RecElim { ref arg, ref res, ref arg_type, .. } => {
            match (**arg, **arg_type) => {
                (RecTerm { ref rec_term }, RecType { ref rec_type }) => {
                    let folded = recurse(rec_term, rec_type, arg_type, res, 0);
                    substitute(res, folded, 0)
                },
                _ => term.clone(),
            }
        },
        */

        /*
        WorldElim { intrinsic, ref world_in, ref arg, ref expr } => {
            match **world_in {
                WorldTerm => {
                    match world.intrinsics.get(intrinsic) {
                        None => term.clone(),
                        Some(intrinsic) => {
                            let ret = (intrinsic.func)(arg);
                            let res = substitute(expr, &Term::new(WorldTerm), 0);
                            let res = substitute(&res, &ret, 0);
                            res
                        },
                    }
                },
                _ => term.clone(),
            }
        },

        UmSucc { ref pred } => {
            match **pred {
                UmTerm { val } => {
                    Term::new(UmTerm { val: val + 1 })
                },
                _ => term.clone(),
            }
        },

        UmElim { ref arg, ref on_zero, ref on_succ, .. } => {
            match **arg {
                UmTerm { val: 0 } => {
                    on_zero.clone()
                },
                UmTerm { val } => {
                    substitute(on_succ, &Term::new(UmTerm { val: val - 1 }), 0)
                },
                UmSucc { ref pred } => {
                    substitute(on_succ, pred, 0)
                },
                _ => term.clone(),
            }
        },
        */
    }
}

