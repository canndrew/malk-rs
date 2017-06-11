use typecheck::Context;
use core::{Term, TermKind, Type, TypeKind, Intro, IntroKind, Elim, ElimKind, Form, FormKind};

pub fn print_ctx(ctx: &Context) -> u32 {
    match *ctx {
        Context::Empty => {
            print!("cnil");
            0
        },
        Context::Cons(ref cn) => {
            let mut depth = print_ctx(&cn.tail);
            for i in (1..(cn.step)).rev() {
                let ty = ctx.core_ctx().lookup(i);
                print!(";");
                print_type(&ty, depth);
                depth = depth + 1;
            }
            print!("; {}: ", cn.var_name.text);
            let ty = ctx.core_ctx().lookup(0);
            print_type(&ty, depth);
            depth + 1
        },
    }
}

fn print_name(depth: u32) {
    let c = (depth + ('a' as u32)) as u8 as char;
    print!("{}", c);
}

pub fn print_type(ty: &Type, depth: u32) {
    match *ty.kind() {
        TypeKind::Elim(ref elim) => {
            print_elim(elim, depth);
        },
        TypeKind::Form(ref form) => {
            print_form(form, depth);
        }
    }
}

pub fn print_intro(intro: &Intro, depth: u32) {
    match *intro.kind() {
        IntroKind::Unit => {
            print!("{{}}");
        },
        IntroKind::Pair { ref head, ref tail } => {
            let mut head = head;
            let mut tail = tail;
            print!("{{");
            loop {
                print_term(head, depth);
                match *tail.kind() {
                    TermKind::Intro(ref sub_intro) => {
                        match *sub_intro.kind() {
                            IntroKind::Unit => break,
                            IntroKind::Pair { head: ref new_head, tail: ref new_tail } => {
                                head = new_head;
                                tail = new_tail;
                                print!(", ");
                                continue;
                            }
                            _ => (),
                        }
                    },
                    _ => (),
                };
                print!(", ... ");
                print_term(tail, depth);
                break;
            }
            println!("}}");
        },
        IntroKind::Lambda { ref body } => {
            print!("(");
            print_name(depth);
            print!(") => (");
            print_term(body, depth + 1);
            print!(")");
        },
        IntroKind::Left { ref val } => {
            print!("[");
            print_term(val, depth);
            print!("]");
        },
        IntroKind::Right { ref val } => {
            print!("[... ");
            print_term(val, depth);
            print!("]");
        },
    }
}

pub fn print_elim(elim: &Elim, depth: u32) {
    match *elim.kind() {
        ElimKind::Var(d) => {
            print_name(depth - d - 1);
        },
        ElimKind::Abort { ref never } => {
            print!("[] ");
            print_elim(never, depth);
        },
        ElimKind::App { ref function, ref arg } => {
            print_elim(function, depth);
            print!("(");
            print_term(arg, depth);
            print!(")");
        },
        ElimKind::Split { ref pair, ref res } => {
            print_elim(pair, depth);
            print!(" in {{");
            print_name(depth);
            print!(", ... ");
            print_name(depth + 1);
            print!("}} => (");
            print_term(res, depth + 2);
            print!(")");
        },
        ElimKind::Case { ref either, ref on_left, ref on_right } => {
            print_elim(either, depth);
            print!(" in [ ");
            print_name(depth);
            print!(" => ");
            print_term(on_left, depth + 1);
            print!(", ... ");
            print_name(depth + 1);
            print!(" => ");
            print_term(on_right, depth + 2);
            print!("]");
        },
    }
}

pub fn print_form(form: &Form, depth: u32) {
    match *form.kind() {
        FormKind::Type { ref level } => {
            print!("Type {}lev", level);
        },
        FormKind::Unit => print!("#{{}}"),
        FormKind::Never => print!("#[]"),
        FormKind::Func { ref arg, ref res } => {
            print!("(");
            print_name(depth);
            print!(": ");
            print_type(arg, depth);
            print!(") -> (");
            print_type(res, depth + 1);
            print!(")");
        },
        FormKind::Pair { ref head, ref tail } => {
            let mut head = head;
            let mut tail = tail;
            let mut depth = depth;
            print!("#{{");
            loop {
                print_name(depth);
                print!(": ");
                print_type(head, depth);
                match *tail.kind() {
                    TypeKind::Form(ref sub_form) => {
                        match *sub_form.kind() {
                            FormKind::Unit => break,
                            FormKind::Pair { head: ref new_head, tail: ref new_tail } => {
                                head = new_head;
                                tail = new_tail;
                                depth += 1;
                                print!(", ");
                                continue;
                            },
                            _ => (),
                        }
                    },
                    _ => (),
                }
                print!(", ... ");
                print_type(tail, depth + 1);
                break;
            }
            print!("}}");
        },
        FormKind::Either { ref left, ref right } => {
            print!("#[[");
            print_type(left, depth);
            print!(", ... ");
            print_type(right, depth);
            print!("]]");
        },
        FormKind::Equality { ref a, ref b, .. } => {
            print_term(a, depth);
            print!(" #= ");
            print_term(b, depth);
        }
    }
}

pub fn print_term(term: &Term, depth: u32) {
    match *term.kind() {
        TermKind::Intro(ref intro) => {
            print_intro(intro, depth);
        },
        TermKind::Elim(ref elim) => {
            print_elim(elim, depth);
        },
        TermKind::Form(ref form) => {
            print_form(form, depth);
        },
    }
}

