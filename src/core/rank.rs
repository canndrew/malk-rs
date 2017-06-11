use std::rc::Rc;
use std::ops::Deref;

use core::meta::RankMVar;

#[derive(Debug, Clone, Hash, PartialEq)]
pub struct Rank(Rc<RankKind>);

#[derive(Debug, Clone, Hash, PartialEq)]
pub enum RankKind {
    Zero,
    Succ(Rank),
    Max(Rank, Rank),
    MetaVar(RankMVar),
}

impl Deref for Rank {
    type Target = RankKind;

    fn deref(&self) -> &RankKind {
        let Rank(ref rc_rank) = *self;
        &**rc_rank
    }
}

impl Rank {
    pub fn new(kind: RankKind) -> Rank {
        Rank(Rc::new(kind))
    }
}

/*
pub fn rank_normalise(rank: &Rank) -> Rank {
    struct Branches {
        const_len: usize,
        var_lens: BTreeMap<RankMVar, usize>,
    }

    fn collect_branches(branches: &mut Branches, rank: &Rank, depth: usize) {
        match **rank {
            RankKind::Zero => {
                branches.const_len = max(branches.const_len, depth);
            },
            RankKind::Succ(ref pred) => {
                collect_branches(branches, pred, depth + 1);
            },
            RankKind::Max(ref a, ref b) => {
                collect_branches(branches, a, depth);
                collect_branches(branches, b, depth);
            },
            RankKind::MetaVar(ref rank_m_var) => {
                match branches.var_lens.entry(rank_m_var) {
                    btree_map::Entry::Occupied(ref oe) => {
                        let d = oe.get_mut();
                        *d = max(*d, depth);
                    },
                    btree_map::Entry::Vacant(ref ve) => {
                        let _ = ve.insert(depth);
                    },
                };
            },
        }
    }

    let mut branches = Branches {
        const_len: 0,
        var_lens: BTreeMap::new(),
    };
    colect_branches(&mut branches, rank, 0);

    fn build_rank(rank: &Rank, len: usize) -> Rank {
        match len {
            0 => rank.clone(),
            _ => RankKind::Succ(build_rank(rank, len - 1)),
        }
    }

    let max_branch_len = branches.var_lens.values().max().unwrap_or(0);

    let mut branches = branches.var_lens.into_iter();
    let mut ret = match branches.const_len > max_branch_len {
        true => {
            build_rank(RankKind::Zero, branches.const_len)
        },
        false => {
            match branches.next() {
                Some((rank_m_var, len)) => {
                    build_rank(RankKind::MetaVar(rank_m_var), len)
                },
                None => RankKind::Zero,
            },
        },
    }
    for (rank_m_var, len) in branches {
        let built = build_rank(rank_m_var, len);
        ret = RankKind::Max(ret, built);
    }
}
*/

