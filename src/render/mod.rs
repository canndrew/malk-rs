use std::iter;

use core::{Rank, RankKind, Term, Ctx, Name, bump_index, Origin};
use core::TermKind::*;

use self::rope::Rope;

use self::Precedence::*;

#[macro_use]
mod rope;

trait RenderResult: Sized {
    fn invalid() -> Result<Rope, Self>;
}

impl RenderResult for ! {
    fn invalid() -> Result<Rope, !> {
        Ok(rope!["<?>"])
    }
}

impl RenderResult for () {
    fn invalid() -> Result<Rope, ()> {
        Err(())
    }
}

pub fn render_rank(mut rank: &Rank) -> Option<String> {
    let mut len = 0;
    loop {
        match **rank {
            RankKind::Zero => break,
            RankKind::Succ(ref pred) => {
                len += 1;
                rank = pred;
            },
            _ => return None,
        }
    };
    let mut s = String::new();
    s.extend(iter::repeat('\'').take(len));
    Some(s)
}

pub fn render_term(ctx: &Ctx, term: &Term) -> Option<String> {
    let rope = match render_term_inner(ctx, term, Block) {
        Ok(rope) => rope,
        Err(()) => return None,
    };
    Some(rope.as_string())
}

pub fn debug_render_term(ctx: &Ctx, term: &Term) -> String {
    let Ok(rope) = render_term_inner::<!>(ctx, term, Block);
    rope.as_string()
}

// TODO: combine this with precedence in parsing
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Precedence {
    Block = 0,
    Operator = 1,
    App = 2,
    Enclosed = 3,
}

fn render_term_inner<R: RenderResult>(ctx: &Ctx, term: &Term, outer_precedence: Precedence) -> Result<Rope, R> {
    //println!("rendering ... {:?} under {:?}", term, ctx);
    let (ret, precedence) = match **term {
        MetaVar(..) => return RenderResult::invalid(),
        Level { ref rank } => {
            let rank = match render_rank(rank) {
                Some(r) => r,
                None => return RenderResult::invalid(),
            };
            (rope!["Level", rank], Enclosed)
        },
        LevelZero { ref rank } => {
            let rank = match render_rank(rank) {
                Some(r) => r,
                None => return RenderResult::invalid(),
            };
            (rope!["0lev", rank], Enclosed)
        },
        LevelSucc { ref pred } => {
            let mut prefix = 1;
            let mut lev = pred;
            loop {
                match **lev {
                    LevelZero { ref rank } => {
                        let rank = match render_rank(rank) {
                            Some(r) => r,
                            None => return RenderResult::invalid(),
                        };
                        break (rope![format!("{}lev{}", prefix, rank)], Enclosed);
                    },
                    LevelSucc { ref pred } => {
                        prefix += 1;
                        lev = pred;
                        continue;
                    },
                    _ => return RenderResult::invalid(),
                };
            }
        },
        LevelMax { ref a, ref b } => {
            let a = render_term_inner(ctx, a, Operator)?;
            let b = render_term_inner(ctx, b, Operator)?;
            (rope!["lmax {", a, ", ", b, "}"], App)
        },
        Type { ref rank, ref level } => {
            let rank = match render_rank(rank) {
                Some(r) => r,
                None => return RenderResult::invalid(),
            };
            match render_term_inner::<R>(ctx, level, Enclosed) {
                Ok(level) => (rope!["Type", rank, " '", level], App),
                Err(_) => (rope!["Type", rank], Enclosed),
            }
        },
        Var(i) => {
            match ctx.get_name(i) {
                Some(ref name) => (rope![name], Enclosed),
                None => return RenderResult::invalid(),
            }
        },
        FuncType { ref arg_name, ref arg_type, ref res_type } => {
            let arg_type_rendered_opt = render_term_inner::<R>(ctx, arg_type, Operator);
            let sub_ctx = Ctx::cons(arg_name.clone(), arg_type.clone(), ctx.clone());
            let res_type_rendered = render_term_inner(&sub_ctx, res_type, Operator)?;
            match *arg_name {
                Some(ref name) => match arg_type_rendered_opt {
                    Ok(arg_type_rendered) => (rope!["(", name, ": ", arg_type_rendered, ") -> ", res_type_rendered], Operator),
                    Err(_) => (rope![name, " -> ", res_type_rendered], Operator),
                },
                None => match arg_type_rendered_opt {
                    Ok(arg_type_rendered) => (rope!["(_: ", arg_type_rendered, ") -> ", res_type_rendered], Operator),
                    Err(_) => (rope!["_ -> ", res_type_rendered], Operator),
                },
            }
        },
        FuncTerm { ref arg_name, ref arg_type, ref res } => {
            return render_func_term_inner(ctx, arg_name, arg_type, res, outer_precedence);
        },
        FuncApp { ref func, ref arg } => {
            (rope![render_term_inner(ctx, func, App)?, " ", render_term_inner(ctx, arg, Enclosed)?], App)
        },
        UnitType => {
            (rope!["#{}"], Enclosed)
        },
        UnitTerm => {
            (rope!["{}"], Enclosed)
        },
        PairType { ref head_name, ref head_type, ref tail_type } => {

            let mut rope = Rope::new();
            rope.push("#{");

            let mut head_name = head_name;
            let mut head_type = head_type;
            let mut tail_type = tail_type;
            let mut ctx = ctx.clone();
            loop {
                let head_type_rendered = render_term_inner(&ctx, head_type, Operator)?;
                match *head_name {
                    Some(ref name) => rope.push(rope![name, ": ", head_type_rendered]),
                    None => rope.push(head_type_rendered),
                };
                ctx = Ctx::cons(head_name.clone(), head_type.clone(), ctx.clone());
                match **tail_type {
                    UnitType => break,
                    PairType { head_name: ref new_head_name, head_type: ref new_head_type, tail_type: ref new_tail_type } => {
                        rope.push(", ");
                        head_name = new_head_name;
                        head_type = new_head_type;
                        tail_type = new_tail_type;
                    },
                    _ => {
                        let tail_type_rendered = render_term_inner(&ctx, tail_type, Operator)?;
                        rope.push(rope!["... ", tail_type_rendered]);
                        break;
                    },
                }
            }
            rope.push("}");
            (rope, Enclosed)
        },
        PairTerm { ref head_name, ref head, ref tail } => {
            let mut rope = Rope::new();
            rope.push("{");

            let mut head_name = head_name;
            let mut head= head;
            let mut tail= tail;
            loop {
                let head_rendered = render_term_inner(ctx, head, Operator)?;
                match *head_name {
                    Some(ref name) => rope.push(rope![name, " = ", head_rendered]),
                    None => rope.push(head_rendered),
                };
                match **tail {
                    UnitTerm => break,
                    PairTerm { head_name: ref new_head_name, head: ref new_head, tail: ref new_tail } => {
                        rope.push(", ");
                        head_name = new_head_name;
                        head = new_head;
                        tail = new_tail;
                    },
                    _ => {
                        let tail_rendered = render_term_inner(ctx, tail, Operator)?;
                        rope.push(rope!["... ", tail_rendered]);
                        break;
                    },
                }
            }
            rope.push("}");
            (rope, Enclosed)
        },
        NeverType => {
            (rope!("#[]"), Enclosed)
        },
        //NeverElim { ref target_type, ref never } => {
        NeverElim { .. } => {
            return RenderResult::invalid(); // TODO: implement
        },
        IdentType { ref a, ref b } => {
            let a = render_term_inner(ctx, a, App)?;
            let b = render_term_inner(ctx, b, App)?;
            (rope![a, " #= ", b], Operator)
        },
        IdentTerm { ref x } => {
            let x = render_term_inner(ctx, x, App)?;
            (rope![x.clone(), " == ", x], Operator)
        },
        EitherType { ref left_name, ref left_type, ref right_type } => {
            let mut rope = Rope::new();
            rope.push("#[");

            let mut left_name = left_name;
            let mut left_type = left_type;
            let mut right_type = right_type;
            loop {
                let left_type_rendered = render_term_inner(ctx, left_type, Operator)?;
                match *left_name {
                    Some(ref name) => rope.push(rope![name, ": ", left_type_rendered]),
                    None => rope.push(left_type_rendered),
                };
                match **right_type {
                    NeverType => break,
                    EitherType { left_name: ref new_left_name, left_type: ref new_left_type, right_type: ref new_right_type } => {
                        rope.push(", ");
                        left_name = new_left_name;
                        left_type = new_left_type;
                        right_type = new_right_type;
                    },
                    _ => {
                        let right_type_rendered = render_term_inner(ctx, right_type, Operator)?;
                        rope.push(rope![", .. ", right_type_rendered]);
                        break;
                    },
                }
            }
            rope.push("]");
            (rope, Enclosed)
        },
        EitherLeft { ref left_name, ref left } => {
            let left = render_term_inner(ctx, left, Operator)?;
            match *left_name {
                Some(ref name) => (rope!["[", name, " = ", left, "]"], Enclosed),
                None => (rope!["[", left, "]"], Enclosed),
            }
        },
        EitherRight { ref right } => {
            let right = render_term_inner(ctx, right, Operator)?;
            (rope!["[... ", right, "]"], Enclosed)
        },
        PairElim { .. } |
        EitherElim { .. } |
        IdentElim { .. } => return RenderResult::invalid(),
        RecType { .. } => unimplemented!(),
    };
    //println!("   => success");
    match precedence < outer_precedence {
        true => Ok(rope!["(", ret, ")"]),
        false => Ok(ret),
    }
}

fn render_func_term_inner<R: RenderResult>(ctx: &Ctx, arg_name: &Option<Name>, arg_type: &Term, res: &Term, outer_precedence: Precedence) -> Result<Rope, R> {
    match **arg_type {
        EitherType { ref left_name, ref left_type, ref right_type } => {
            match **res {
                EitherElim { ref either, ref left_res, ref right_res, .. } => {
                    match **either {
                        Var(0) => {
                            let inner = render_enum_elim_inner(ctx, left_name, left_type, right_type, left_res, right_res, arg_type.origin.clone())?;
                            return Ok(rope!["[ ", inner, " ]"]);
                        }
                        _ => (),
                    }
                },
                _ => (),
            }
        },
        _ => (),
    };

    let arg_type_rendered_opt = render_term_inner::<R>(ctx, arg_type, Operator);
    let sub_ctx = Ctx::cons(arg_name.clone(), arg_type.clone(), ctx.clone());
    let res_rendered = render_term_inner(&sub_ctx, res, Operator)?;
    let ret = match *arg_name {
        Some(ref name) => match arg_type_rendered_opt {
            Ok(arg_type_rendered) => rope!["(", name, ": ", arg_type_rendered, ") => ", res_rendered],
            Err(_) => rope![name, " => ", res_rendered],
        },
        None => match arg_type_rendered_opt {
            Ok(arg_type_rendered) => rope!["(_: ", arg_type_rendered, ") => ", res_rendered],
            Err(_) => rope!["_ => ", res_rendered],
        },
    };
    println!("precedence == {:#?} .. {}", outer_precedence, (Operator <= outer_precedence));
    match Operator < outer_precedence {
        true => Ok(rope!["(", ret, ")"]),
        false => Ok(ret),
    }
}

fn render_enum_elim_inner<R: RenderResult>(
            ctx: &Ctx, 
            left_name: &Option<Name>,
            left_type: &Term,
            right_type: &Term,
            left_res: &Term,
            right_res: &Term,
            either_origin: Origin,
    ) -> Result<Rope, R>
{
    let ty = Term::new(EitherType {
        left_name: left_name.clone(),
        left_type: left_type.clone(),
        right_type: right_type.clone(),
    }, either_origin);
    let ctx = Ctx::cons(None, ty, ctx.clone());
    let left_type = bump_index(left_type, 0);
    let right_type = bump_index(right_type, 0);
    let left_rendered = render_func_term_inner(&ctx, left_name, &left_type, left_res, Operator)?;
    let mut ret = match *left_name {
        Some(ref name) => {
            rope![name, " = ", left_rendered, ","]
        },
        None => {
            rope![left_rendered, ","]
        },
    };
    match *right_type {
        EitherType { ref left_name, ref left_type, ref right_type } => {
            let origin = right_type.origin.clone();
            match **right_res {
                EitherElim { ref either, ref left_res, ref right_res, .. } => {
                    match **either {
                        Var(0) => {
                            let rest = render_enum_elim_inner(&ctx, left_name, left_type, right_type, left_res, right_res, origin)?;
                            ret.push(rest);
                            return Ok(ret);
                        },
                        _ => (),
                    }
                },
                _ => (),
            }
        },
        NeverType => {
            match **right_res {
                NeverElim { ref never, .. } => {
                    match **never {
                        Var(0) => {
                            return Ok(ret);
                        },
                        _ => (),
                    }
                },
                _ => (),
            }
        },
        _ => (),
    };
    let right_rendered = render_func_term_inner(&ctx, &None, &right_type, right_res, Operator)?;
    ret.push("... ");
    ret.push(right_rendered);
    Ok(ret)
}



/*
fn render_elim_inner(ctx: &Ctx, term: &Term, arg_index: usize, arg_type: &Term) -> Result<Rope, ()> {
    match **term {
        EitherElim {

        },
    }
}
*/



/*
pub fn render_rank(f: &mut fmt::Formatter, rank: &Rank) -> fmt::Result {
    match **rank {
        RankKind::Zero => Ok(()),
        RankKind::Succ(ref pred) => {
            write!(f, "'")?;
            render_rank(f, pred)
        },
        RankKind::Max(..) => panic!("pretty sure i don't need this"),
        RankKind::MetaVar(ref i) => {
            write!(f, "??{:x}", i.id())
        },
    }
}

pub struct RenderRank<'r>(pub &'r Rank);

impl<'r> fmt::Display for RenderRank<'r> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let RenderRank(r) = *self;
        render_rank(f, r)
    }
}

pub struct Render<'c, 't>(pub &'c Ctx, pub &'t Term);

impl<'c, 't> fmt::Display for Render<'c, 't> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Render(c, t) = *self;
        render_term(f, c, t)
    }
}

pub fn render_term(f: &mut fmt::Formatter, ctx: &Ctx, term: &Term) -> fmt::Result {
    match **term {
        MetaVar(ref i) => write!(f, "?{:x}", i.id()),
        Level { ref rank } => {
            write!(f, "Level{}", RenderRank(rank))
        },
        LevelZero { ref rank } => {
            write!(f, "0lev{}", RenderRank(rank))
        },
        LevelSucc { ref pred } => {
            let mut prefix = 1;
            let mut lev = pred;
            loop {
                match **lev {
                    LevelZero { ref rank } => {
                        write!(f, "{}lev{}", prefix, RenderRank(rank))?;
                        break
                    },
                    LevelSucc { ref pred } => {
                        prefix += 1;
                        lev = pred;
                        continue;
                    },
                    _ => (),
                };
                write!(f, "{}lev + {}", prefix, Render(ctx, lev))?;
                break;
            }
            Ok(())
        },
        LevelMax { ref a, ref b } => {
            write!(f, "lmax {{{}, {}}}", Render(ctx, a), Render(ctx, b))
        },
        Type { ref rank, ref level } => {
            write!(f, "Type{} '{}", RenderRank(rank), Render(ctx, level))
        },
        Var(i) => {
            match ctx.get_name(i) {
                Some(ref name) => {
                    write!(f, "{}", name)
                },
                None => {
                    let ty = ctx.type_of_var(i).unwrap();
                    write!(f, "(↑{}: {})", i, Render(ctx, &ty))
                },
            }
        },
        FuncType { ref arg_name, ref arg_type, ref res_type } => {
            let sub_ctx = Ctx::cons(arg_name.clone(), arg_type.clone(), ctx.clone());
            match *arg_name {
                Some(ref name) => write!(f, "({}: {}) -> {}", name, Render(ctx, arg_type), Render(&sub_ctx, res_type)),
                None => write!(f, "(_: {}) -> {}", Render(ctx, arg_type), Render(&sub_ctx, res_type)),
            }
        },
        FuncTerm { ref arg_name, ref arg_type, ref res } => {
            let sub_ctx = Ctx::cons(arg_name.clone(), arg_type.clone(), ctx.clone());
            match *arg_name {
                Some(ref name) => write!(f, "({}: {}) => {}", name, Render(ctx, arg_type), Render(&sub_ctx, res)),
                None => write!(f, "(_: {}) => {}", Render(ctx, arg_type), Render(&sub_ctx, res)),
            }
        },
        FuncApp { ref func, ref arg } => {
            write!(f, "{} {}", Render(ctx, func), Render(ctx, arg))
        },
        UnitType => {
            write!(f, "#{{}}")
        },
        UnitTerm => {
            write!(f, "{{}}")
        },
        PairType { ref head_name, ref head_type, ref tail_type } => {
            fn render_tail(f: &mut fmt::Formatter, ctx: &Ctx, term: &Term) -> fmt::Result {
                match **term {
                    UnitType => Ok(()),
                    PairType { ref head_name, ref head_type, ref tail_type } => {
                        match *head_name {
                            Some(ref name) => write!(f, ", {}: {}", name, Render(ctx, head_type))?,
                            None => write!(f, ", {}", Render(ctx, head_type))?,
                        };
                        let tail_ctx = Ctx::cons(head_name.clone(), head_type.clone(), ctx.clone());
                        render_tail(f, &tail_ctx, tail_type)
                    },
                    _ => {
                        write!(f, ", .. {}", Render(ctx, term))
                    },
                }
            }

            match *head_name {
                Some(ref name) => write!(f, "#{{{}: {}", name, Render(ctx, head_type))?,
                None => write!(f, "#{{{}", Render(ctx, head_type))?,
            };
            let tail_ctx = Ctx::cons(head_name.clone(), head_type.clone(), ctx.clone());
            render_tail(f, &tail_ctx, tail_type)?;
            write!(f, "}}")
        },
        PairTerm { ref head_name, ref head, ref tail } => {
            fn render_tail(f: &mut fmt::Formatter, ctx: &Ctx, term: &Term) -> fmt::Result {
                match **term {
                    UnitTerm => Ok(()),
                    PairTerm { ref head_name, ref head, ref tail } => {
                        match *head_name {
                            Some(ref name) => write!(f, ", {} = {}", name, Render(ctx, head))?,
                            None => write!(f, ", {}", Render(ctx, head))?,
                        };
                        render_tail(f, ctx, tail)
                    },
                    _ => {
                        write!(f, ", .. {}", Render(ctx, term))
                    },
                }
            }
            
            match *head_name{ 
                Some(ref name) => write!(f, "{{{} = {}", name, Render(ctx, head))?,
                None => write!(f, "{{{}", Render(ctx, head))?,
            };
            render_tail(f, ctx, tail)?;
            write!(f, "}}")
        },
        NeverType => {
            write!(f, "#[]")
        },
        NeverElim { ref target_type, ref never } => {
            unimplemented!()
        },
        _ => unimplemented!()
    }
}
*/

