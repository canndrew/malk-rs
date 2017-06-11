use std::rc::Rc;

use core::{Term, Name};
use core::{bump_index, /*substitute_meta, substitute_rank_meta, */ };
//use core::fold::{contextual_noop, ctx_func_noop};

#[derive(Debug, Clone, Hash, PartialEq)]
pub struct Ctx(pub Option<Rc<CtxNode>>);

#[derive(Debug, Hash, PartialEq)]
pub struct CtxNode {
    pub ty: Term,
    pub tail: Ctx,
    pub name: Option<Name>,
}

impl Ctx {
    pub fn nil() -> Ctx {
        Ctx(None)
    }

    /*
    pub fn standard() -> Ctx {
        let ret = Ctx::nil();

        let ubig_type = ubig::mk_type();
        let ret = Ctx::cons(Some("Ubig"), ubig_type, ret);
        ret
    }
    */

    pub fn cons(name: Option<Name>, ty: Term, tail: Ctx) -> Ctx {
        Ctx(Some(Rc::new(CtxNode {
            ty: ty,
            name: name,
            tail: tail,
        })))
    }

    pub fn len(&self) -> usize {
        let Ctx(ref opt_node) = *self;
        match *opt_node {
            None => 0,
            Some(ref node) => 1 + node.tail.len(),
        }
    }

    pub fn type_of_var(&self, index: usize) -> Option<Term> {
        let Ctx(ref opt_node) = *self;
        match *opt_node {
            None => None,
            Some(ref node) => {
                match index {
                    0 => Some(node.ty.clone()),
                    _ => match node.tail.type_of_var(index - 1) {
                        None => None,
                        Some(ref ty) => {
                            let ty = bump_index(ty, 0);
                            Some(ty)
                        },
                    },
                }
            },
        }
    }

    pub fn lookup_name(&self, name: &Name) -> Option<(usize, Term)> {
        let Ctx(ref opt_node) = *self;
        match *opt_node {
            None => None,
            Some(ref node) => {
                match node.name.as_ref() == Some(name) {
                    true => Some((0, node.ty.clone())),
                    false => match node.tail.lookup_name(name) {
                        None => None,
                        Some((i, ref ty)) => {
                            let ty = bump_index(ty, 0);
                            Some((i + 1, ty))
                        },
                    },
                }
            },
        }
    }

    pub fn get_name(&self, index: usize) -> Option<Name> {
        let Ctx(ref opt_node) = *self;
        match *opt_node {
            None => None,
            Some(ref node) => {
                match index {
                    0 => node.name.clone(),
                    _ => node.tail.get_name(index - 1),
                }
            },
        }
    }

    pub fn insert(&self, name: Option<Name>, ty: Term, index: usize) -> Ctx {
        match index {
            0 => self.clone(),
            _ => {
                let Ctx(ref opt_node) = *self;
                match *opt_node {
                    None => panic!("insert index out of range"),
                    Some(ref node) => {
                        let tail = node.tail.insert(name, ty, index - 1);
                        let ty = bump_index(&node.ty, index - 1);
                        Ctx::cons(node.name.clone(), ty, tail)
                    },
                }
            },
        }
    }
}

/*
pub fn ctx_substitute_meta(ctx: &Ctx, meta_index: usize, sub: &Term) -> Ctx {
    fn term_func(term: &Term, _: &mut (), &mut (meta_index, sub): &mut (usize, &Term)) -> Result<Term, !> {
        Ok(substitute_meta(term, meta_index, sub))
    }
    let Ok(ret) = fold_ctx(ctx, &mut (), &mut (meta_index, sub), contextual_noop, contextual_noop, ctx_func_noop, term_func);
    ret
}

pub fn ctx_substitute_rank_meta(ctx: &Ctx, meta_index: usize, sub: &Rank) -> Ctx {
    fn term_func(term: &Term, _: &mut (), &mut (meta_index, sub): &mut (usize, &Rank)) -> Result<Term, !> {
        Ok(substitute_rank_meta(term, meta_index, sub))
    }
    let Ok(ret) = fold_ctx(ctx, &mut (), &mut (meta_index, sub), contextual_noop, contextual_noop, ctx_func_noop, term_func);
    ret
}
*/

