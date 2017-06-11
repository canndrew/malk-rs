use std::rc::Rc;

use core::{Origin, Ctx, Term, TermKind, Name, bump_index};

pub struct CtxExt{
    kind: Rc<CtxExtKind>,
    origin: Origin,
}

pub enum CtxExtKind {
    Bind {
        name: Name,
        ty: Term,
    },
    Pair {
        head_name: Option<Name>,
        head: CtxExt,
        tail: CtxExt,
    },
    Unit,
}

impl CtxExt {
    pub fn new(kind: CtxExtKind, origin: Origin) -> CtxExt {
        CtxExt {
            kind: Rc::new(kind),
            origin: origin,
        }
    }

    /*
    pub fn bump(&self, cutoff: usize) -> CtxExt {
        let CtxExt(ref kind) = *self;
        match *kind {
            CtxExtKind::Bind { ref name, ref ty } => {
                let ty = bump_index(ty, cutoff);
                CtxExtKind::new(CtxExtKind::Bind {
                    name: name.clone(),
                    ty: ty,
                })
            },
            CtxExtKind::Pair { ref head, ref tail } => {
                let head = bump_index(head, cutoff);
                let tail = bump_index(tail, cutoff + 1);
                CtxExtKind::new(CtxExtKind::Pair {
                    head: head,
                    tail: tail,
                })
            },
            CtxExtKind::Unit => self.clone(),
        }
    }
    */

    pub fn name(&self) -> Option<Name> {
        match *self.kind {
            CtxExtKind::Bind { ref name, .. } => Some(name.clone()),
            _ => None,
        }
    }

    pub fn ty(&self) -> Term {
        match *self.kind {
            CtxExtKind::Bind { ref ty, .. } => ty.clone(),
            CtxExtKind::Pair { ref head_name, ref head, ref tail } => {
                let kind = TermKind::PairType {
                    head_name: head_name.clone(),
                    head_type: head.ty(),
                    tail_type: tail.ty(),
                };
                Term::new(kind, Origin::TypeOf(Rc::new(self.origin.clone())))
            },
            CtxExtKind::Unit => Term::new(TermKind::UnitType, Origin::TypeOf(Rc::new(self.origin.clone()))),
        }
    }
    
    fn ext_len(&self) -> usize {
        match *self.kind {
            CtxExtKind::Unit |
            CtxExtKind::Bind { .. } => 0,
            CtxExtKind::Pair { ref head, ref tail, .. } => {
                2 + head.ext_len() + tail.ext_len()
            },
        }
    }

    fn extend_inner(&self, index: usize, ctx: Ctx) -> Ctx {
        match *self.kind {
            CtxExtKind::Unit |
            CtxExtKind::Bind { .. } => ctx,
            CtxExtKind::Pair { ref head, ref tail, .. } => {
                let mut head_type = head.ty();
                let mut tail_type = head.build_elim(tail.ty());
                for _ in 0..(index + 1) {
                    head_type = bump_index(&head_type, 0);
                    tail_type = bump_index(&tail_type, 1);
                }
                let ctx = Ctx::cons(head.name(), head_type, ctx);
                let ctx = Ctx::cons(tail.name(), tail_type, ctx);
                let ctx = head.extend_inner(1, ctx);
                let ctx = tail.extend_inner(head.ext_len(), ctx);
                ctx
            },
        }
    }

    pub fn extend(&self, ctx: Ctx) -> Ctx {
        let ctx = Ctx::cons(self.name(), self.ty(), ctx);
        self.extend_inner(0, ctx)
    }

    fn build_elim_inner(&self, index: usize, term: Term) -> Term {
        match *self.kind {
            CtxExtKind::Unit |
            CtxExtKind::Bind { .. } => term,
            CtxExtKind::Pair { ref head_name, ref head, ref tail } => {
                let mut head_type = head.ty();
                let mut tail_type = head.build_elim(tail.ty());
                for _ in 0..(index + 1) {
                    head_type = bump_index(&head_type, 0);
                    tail_type = bump_index(&tail_type, 1);
                }
                let term = tail.build_elim_inner(head.ext_len(), term);
                let term = head.build_elim_inner(1, term);
                let pair_kind = TermKind::Var(index);
                let kind = TermKind::PairElim {
                    head_name: head_name.clone(),
                    pair: Term::new(pair_kind, self.origin.clone()),
                    res: term,
                };
                Term::new(kind, Origin::ElimOf(Rc::new(self.origin.clone())))
            },
        }
    }

    pub fn build_elim(&self, term: Term) -> Term {
        self.build_elim_inner(0, term)
    }
}













/*
#[derive(Clone)]
pub enum CtxPattern<'c> {
    Bind {
        name: &'c Ident,
        ty: Term,
    },
    /*
    Typed {
        ty: Term,
        sub_pattern: Rc<CtxPattern>,
    },
    */
    Unit,
    Pair {
        head: Rc<CtxPattern<'c>>,
        tail: Rc<CtxPattern<'c>>,
    },
    /*
    Equality {
        a: Term,
        b: Term,
    },
    */
}

impl<'p> CtxPattern<'p> {
    pub fn lookup_inner(&self, sctx: SynCtx, var_name: &'p Ident) -> Result<(usize, Term), ConstructError> {
        match *self {
            CtxPattern::Bind { name, ref ty } => {
                match name == var_name {
                    true  => Ok((0, ty.clone())),
                    false => {
                        let (i, ty) = sctx.lookup_inner(var_name)?;
                        Ok((i + 1, bump_index(&ty, 0)))
                    },
                }
            },
            CtxPattern::Pair { ref head, ref tail } => {
                let sctx = SynCtx::cons(SynCtxNodeKind::PairPatternTail {
                    head: head.clone(),
                }, sctx.clone());
                tail.lookup_inner(sctx, var_name)
            },
            CtxPattern::Unit => {
                let (i, ty) = sctx.lookup_inner(var_name)?;
                Ok((i + 1, bump_index(&ty, 0)))
            },
        }
    }

    /// Creates the context under the pattern's eliminators.
    /// Does not include the overall type, this is added beforehand.
    pub fn build_core_ctx(&self, ctx: Ctx) -> Ctx {
        match *self {
            CtxPattern::Bind { .. } => ctx,
            CtxPattern::Pair { ref head, ref tail } => {
                let head_ty = head.ty();
                let tail_ty = tail.ty();
                let head_ty = bump_index(&head_ty, 0);
                let tail_ty = bump_index(&tail_ty, 1);
                let ctx = Ctx::cons(head_ty, ctx);
                let ctx = Ctx::cons(tail_ty, ctx);
                let ctx = head.build_core_ctx(ctx);
                let ctx = tail.build_core_ctx(ctx);
                ctx
            },
        }
    }

    /// Get the type of the pattern.
    pub fn ty(&self) -> Term {
        match *self {
            CtxPattern::Bind { ref ty, .. } => ty.clone(),
            CtxPattern::Pair { ref head, ref tail } => {
                Term::new(TermKind::PairType {
                    head_type: head.ty(),
                    tail_type: head.build_eliminator(0, tail.ty()),
                })
            },
        }
    }

    /// Build the eliminator for the pattern.
    /// `arg_index` refers to the variable where the pattern's argument lives.
    pub fn build_eliminator(&self, arg_index: usize, res: Term) -> Term {
        match *self {
            CtxPattern::Bind { .. } => res,
            CtxPattern::Pair { ref head, ref tail } => {
                Term::new(TermKind::PairElim {
                    pair: Term::new(TermKind::Var(arg_index)),
                    head_type: head.ty(),
                    tail_type: tail.ty(),
                    res: res,
                })
            },
        }
    }
}

#[derive(Clone)]
pub struct SynCtx<'c>(Option<Rc<SynCtxNode<'c>>>);

pub struct SynCtxNode<'c> {
    kind: SynCtxNodeKind<'c>,
    core_ctx: Ctx,
    tail: SynCtx<'c>,
}

pub enum SynCtxNodeKind<'c> {
    /// Inside the res of a function/let or result type of a function type.
    Func {
        arg: Rc<CtxPattern<'c>>,
    },

    /// Inside the tail of a pair type.
    PairTypeTail {
        head: Rc<CtxPattern<'c>>,
    },

    /// Inside the tail of a pair pattern,
    PairElimTail {
        head: Rc<CtxPattern<'c>>,
        tail_ty: Term,
    },

    /// Inside the left-branch of an enum function.
    EitherLeft,

    /// Inside the right-branch of an enum function.
    EitherRight,

    /// Inside the res of a recursive type.
    RecType {
        name: &'c Ident,
    },
}

impl<'c> SynCtx<'c> {
    pub fn nil() -> SynCtx<'c> {
        SynCtx(None)
    }

    pub fn cons(kind: SynCtxNodeKind<'c>, tail: SynCtx<'c>) -> SynCtx<'c> {
        let core_ctx_tail = tail.core_ctx();
        let core_ctx = match kind {
            SynCtxNodeKind::Func { ref arg } => {
                let ctx = Ctx::cons(arg.ty(), core_ctx_tail);
                arg.build_core_ctx(ctx)
            },
            SynCtxNodeKind::PairTypeTail { ref head } => {
                let ctx = Ctx::cons(head.ty(), core_ctx_tail);
                head.build_core_ctx(ctx)
            },
            SynCtxNodeKind::PairElimTail { ref head, ref tail_type } => {
                let head_type = head.ty();
                let pair_type = Term::new(TermKind::PairType {
                    head_type: head_type.clone(),
                    tail_type: tail_type.clone(),
                });
                let ctx = Ctx::cons(pair_type, core_ctx_tail);
                let ctx = Ctx::cons(head_type, ctx);
                ctx
            },
        };
        let node = SynCtxNode {
            kind: kind,
            core_ctx: core_ctx,
            tail: tail,
        };
        SynCtx(Some(Rc::new(node)))
    }

    pub fn lookup_typed(&self, var_name: &Ident) -> Result<(usize, Term), ConstructError> {
        let (i, ty) = self.lookup_inner(var_name)?;

        let mut ty = ty.clone();
        for _ in 0..i {
            ty = bump_index(&ty, 0);
        }

        Ok((i, ty))
    }
    
    pub fn lookup(&self, var_name: &Ident) -> Result<usize, ConstructError> {
        let (i, _) = self.lookup_inner(var_name)?;
        Ok(i)
    }

    fn lookup_inner(&self, var_name: &Ident) -> Result<(usize, Term), ConstructError> {
        let SynCtx(ref node_kind_opt) = *self;
        match *node_kind_opt {
            None => Err(ConstructError::NoSuchVariable(var_name.clone())),
            Some(ref ctx_node) => {
                match ctx_node.kind {
                    SynCtxNodeKind::Func { ref arg } => {
                        arg.lookup_inner(&ctx_node.tail)
                    },
                    SynCtxNodeKind::PairTypeTail { ref head } => {
                        head.lookup_inner(&ctx_node.tail)
                    },
                    SynCtxNodeKind::PairElimTail { ref head, ref tail_type } => {

                    },
                    /*
                    SynCtxNodeKind::EitherLefts { ref val, index } => {
                    },
                    SynCtxNodeKind::EitherRight { ref val, index } => {
                    },
                    */
                }
            }
        }
    }

    pub fn core_ctx(&self) -> Ctx {
        let SynCtx(ref node_kind_opt) = *self;
        match *node_kind_opt {
            None => Ctx::nil(),
            Some(ref node) => {
                node.core_ctx.clone()
            },
        }
    }
}
*/

