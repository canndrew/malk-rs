use std::rc::Rc;

//pub use core::{Term, TermKind, World, bump_index, substitute, normalise};
use core::{Debug, Ctx, Type};
use typecheck::TypeCheckError;
use parse::Ident;

#[derive(Clone, Debug)]
pub enum Context<'p> {
    Empty,
    Cons(Rc<ContextNode<'p>>),
}

#[derive(Clone, Debug)]
pub struct ContextNode<'p> {
    pub tail: Context<'p>,
    pub var_name: &'p Ident,
    pub step: u32,
    pub core_ctx: Ctx,
}

impl<'p> Context<'p> {
    pub fn lookup_by_name(&self, name: &str) -> Result<(u32, Ctx), TypeCheckError> {
        match *self {
            Context::Empty => Err(TypeCheckError::NoSuchVariable {
                name: name.to_string(),
            }),
            Context::Cons(ref node) => {
                if &node.var_name.text[..] == name {
                    return Ok((0, node.core_ctx.clone()));
                }
                let (index, ctx) = node.tail.lookup_by_name(name)?;
                Ok((index + node.step, ctx))
            },
        }
    }

    pub fn core_ctx(&self) -> Ctx {
        match *self {
            Context::Empty => Ctx::nil(),
            Context::Cons(ref node) => node.core_ctx.clone(),
        }
    }

    pub fn append(self, var_name: &'p Ident, step: u32, core_ctx: Ctx) -> Context {
        d!({
            let mut check_ctx = core_ctx.clone();
            for _ in 0..step {
                check_ctx = unwrap!(check_ctx.try_strengthen(0));
            }
            assert!(check_ctx == self.core_ctx());
        });
        Context::Cons(Rc::new(ContextNode {
            tail: self,
            var_name: var_name,
            step: step,
            core_ctx: core_ctx,
        }))
    }

    pub fn weaken_core(self, ty: Type, index: u32) -> Context<'p> {
        // The start of the core ctx needs to be pointed to by the named variable at the start of
        // the Context. weaken_core is only for inserting anonymous nodes into the core ctx, so it
        // must be inserted behind (at least) the first (named) node.
        assert!(index != 0, "can't weaken_core at the front");

        match self {
            Context::Empty => panic!("can't weaken an empty Context"),
            Context::Cons(ref node) => {
                let mut node = (**node).clone();
                if index <= node.step {
                    node.core_ctx = node.core_ctx.weaken(ty, index);
                    node.step += 1;
                }
                else {
                    node.tail = node.tail.weaken_core(ty, index - node.step);
                };
                Context::Cons(Rc::new(node))
            },
        }
    }
}


/*
#[derive(Clone, Copy)]
pub enum Ctx<'c, 'p: 'c> {
    Empty,
    Cons(&'c CtxNode<'c, 'p>),
}

pub struct CtxNode<'c, 'p: 'c> {
    pub tail: Ctx<'c, 'p>,
    pub var_name: &'p Ident,
    pub var_type: Type,
}
*/

/*
pub struct CtxNode<'c, 'p: 'c> {
    pub tail: Ctx<'c, 'p>,
    pub kind: CtxNodeKind<'p>,
}

pub enum CtxNodeKind<'p> {
    Var {
        var_name: &'p Ident,
        var_type: Term,
    },
    Unit,
    Pair {
        head: Box<CtxNodeKind<'p>>,
        tail: Box<CtxNodeKind<'p>>,
    },
}

impl<'c, 'p: 'c> Ctx<'c, 'p> {
    pub fn lookup_by_name(&self, name: &str, world: &World) -> Option<(usize, Term)> {
        match self.lookup(name, 0, world) {
            None => None,
            Some((index, var_type)) => {
                let var_type = bump_index(&var_type, index + 1, 0);
                Some((index, var_type))
            },
        }
    }

    fn lookup(&self, name: &str, index: usize, world: &World) -> Option<(usize, Term)> {
        match *self {
            Ctx::Empty => None,
            Ctx::Cons(ref ctx_node) => {
                ctx_node.lookup(name, index, world)
            },
        }
    }
}

impl<'c, 'p: 'c> CtxNode<'c, 'p> {
    pub fn deskew(&self, term: &Term, world: &World) -> Term {
        let term = self.kind.deskew(term, world, 0);
        normalise(&term, world)
    }

    pub fn skew(&self, term: &Term, world: &World) -> Term {
        let sub_indices = self.kind.sub_indices();
        let arg = self.kind.gen_argument(sub_indices, 0);
        let term = bump_index(term, sub_indices + 1, 1);
        let term = substitute(&term, &arg, 0);
        normalise(&term, world)
    }

    fn lookup(&self, name: &str, index: usize, world: &World) -> Option<(usize, Term)> {
        println!("CtxNode::lookup({:?}, {})", name, index);
        let (sub_indices, res) = self.kind.lookup(name, 0, 0, world);
        println!("sub_indices == {}", sub_indices);
        match res {
            None => {
                self.tail.lookup(name, index + sub_indices + 1, world)
            },
            Some((from_top, var_type)) => {
                let from_bottom = sub_indices - from_top;
                let var_index = index + from_bottom;
                Some((var_index, var_type))
            },
        }
    }
}

impl<'p> CtxNodeKind<'p> {
    fn gen_argument(&self, index: usize, from_index: usize) -> Term {
        match *self {
            CtxNodeKind::Var { .. } => Term::new(TermKind::Var(index)),
            CtxNodeKind::Unit => Term::new(TermKind::UnitTerm),
            CtxNodeKind::Pair { ref head, ref tail } => {
                let head_sub_indices = head.sub_indices();
                let head_index = index - from_index - 1;
                let tail_index = index - from_index - 2;
                let head_term = head.gen_argument(head_index, 1);
                let tail_term = tail.gen_argument(tail_index, head_sub_indices);
                Term::new(TermKind::PairTerm {
                    head: head_term,
                    tail: tail_term,
                })
            },
        }
    }

    fn sub_indices(&self) -> usize {
        match *self {
            CtxNodeKind::Var { .. } => 0,
            CtxNodeKind::Unit => 0,
            CtxNodeKind::Pair { ref head, ref tail } => {
                head.sub_indices() + tail.sub_indices() + 2
            },
        }
    }

    fn lookup(&self, name: &str, from_top: usize, from_index: usize, world: &World) -> (usize, Option<(usize, Term)>) {
        //println!("CtxNodeKind::lookup({:?}, {})", name, index);
        match *self {
            CtxNodeKind::Var { ref var_name, ref var_type } => {
                match &var_name.text == name {
                    true  => {
                        println!("found it {:?}", var_type);
                        (0, Some((from_top, var_type.clone())))
                    },
                    false => (0, None),
                }
            },
            CtxNodeKind::Unit => (0, None),
            CtxNodeKind::Pair { ref head, ref tail } => {
                let head_from_top = from_top + from_index + 1;
                let (head_sub_indices, head_res) = head.lookup(name, head_from_top, 1, world);
                match head_res {
                    Some(x) => {
                        let tail_sub_indices = tail.sub_indices();
                        let sub_indices = head_sub_indices + tail_sub_indices + 2;
                        (sub_indices, Some(x))
                    },
                    None => {
                        let tail_from_top = from_top + from_index + 2;
                        let (tail_sub_indices, tail_res) = tail.lookup(name, tail_from_top, head_sub_indices, world);
                        let sub_indices = head_sub_indices + tail_sub_indices + 2;
                        let res = match tail_res {
                            Some((from_top, var_type)) => {
                                let var_type = bump_index(&var_type, 1, from_index + 1);
                                Some((from_top, var_type))
                            },
                            None => None,
                        };
                        (sub_indices, res)
                    },
                }
            },
        }
    }

    fn get_type(&self, world: &World) -> Term {
        match *self {
            CtxNodeKind::Var { ref var_type, .. } => var_type.clone(),
            CtxNodeKind::Unit => Term::new(TermKind::UnitType),
            CtxNodeKind::Pair { ref head, ref tail } => {
                let head_type = head.get_type(world);
                let tail_type = tail.get_type(world);
                let tail_type = head.deskew(&tail_type, world, 0);
                Term::new(TermKind::PairType {
                    head_type: head_type,
                    tail_type: tail_type,
                })
            },
        }
    }

    fn deskew(&self, term: &Term, world: &World, from_index: usize) -> Term {
        match *self {
            CtxNodeKind::Var { .. } => term.clone(),
            CtxNodeKind::Unit => term.clone(),
            CtxNodeKind::Pair { ref head, ref tail } => {
                let head_sub_indices = head.sub_indices();
                let term = tail.deskew(term, world, head_sub_indices);
                let term = head.deskew(&term, world, 1);
                let head_type = head.get_type(world);
                let tail_type = tail.get_type(world);
                let tail_type = head.deskew(&tail_type, world, 0);
                let head_type_bumped = bump_index(&head_type, 1, from_index);
                let tail_type_bumped = bump_index(&tail_type, 1, from_index + 1);
                let term = Term::new(TermKind::PairElim {
                    pair: Term::new(TermKind::Var(from_index)),
                    res: term,
                    head_type: head_type_bumped,
                    tail_type: tail_type_bumped,
                });
                term
            },
        }
    }
}
*/

