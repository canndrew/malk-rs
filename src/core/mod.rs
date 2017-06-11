pub use self::fold::{fold, fold_rank, fold_ctx};
pub use self::ops::{substitute, try_lower_index, bump_index}; //, substitute_meta, substitute_rank_meta, rank_substitute_rank_meta};
pub use self::term::{Term, TermKind, reduce_head};
pub use self::rank::{Rank, RankKind};
pub use self::ctx::{Ctx}; //, ctx_substitute_meta, ctx_substitute_rank_meta};
pub use self::name::Name;
pub use self::meta::MVar;
pub use self::origin::Origin;
//pub use self::typecheck::MetaCtx;

//pub mod typecheck;
pub mod term;
pub mod fold;
pub mod ops;
pub mod rank;
pub mod ctx;
pub mod name;
pub mod meta;
pub mod origin;
//pub mod constraints;

