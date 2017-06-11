use std::collections::HashMap;

use render;
use core;
use parse;
use construct;
use render::debug_render_term;
use parse::{Syntax, Expr, BlockPrecedenceExprKind};

pub struct Document {
    text: String,
    parsed: Option<Expr<BlockPrecedenceExprKind>>,
    stamp: usize,
}

#[derive(Debug, Clone)]
pub struct Diff {
    pub start_pos: usize,
    pub remove_len: usize,
    pub insert_text: String,
}

impl Document {
    pub fn load(text: String) -> Document {
        println!("In Document::load");
        let mut ret = Document {
            text: text,
            parsed: None,
            stamp: 0,
        };
        ret.reparse();

        ret
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    pub fn normalise(&mut self, pos: usize) -> Option<Diff> {
        fn sub_syns_at_pos(syn: &Syntax, pos: usize) -> Vec<&Syntax> {
            if syn.span().start > pos || syn.span().end <= pos {
                println!("outside of span!");
                return vec![];
            }

            let mut v = vec![];
            for s in syn.sub_syntax() {
                v = sub_syns_at_pos(s, pos);
                if !v.is_empty() {
                    break;
                }
            }
            v.push(syn);
            v
        }

        let expr = match self.parsed {
            Some(ref expr) => expr,
            None => return None,
        };
        println!("got the top expr: {:?}", expr);
        let sub_syns: Vec<&Syntax> = sub_syns_at_pos(expr, pos);

        println!("got {} sub_syns", sub_syns.len());
        
        for sub_syn in sub_syns {
            let old_span = sub_syn.span();
            let term = match sub_syn.term() {
                Some(ref term) => term.clone(),
                None => {
                    println!("expression has no term!");
                    continue;
                }
            };
            let ctx = match sub_syn.ctx() {
                Some(ref ctx) => ctx.clone(),
                None => {
                    println!("expression has no ctx!");
                    continue;
                }
            };

            let world = core::term::World {
                intrinsics: HashMap::new(),
            };
            let normalised_term = core::ops::normalise(&term, &world);
            if normalised_term == term {
                println!("normalised term is the same!: {:?}", term);
                continue
            }
            
            let rendered = match render::render_term(&ctx, &normalised_term) {
                Some(rendered) => rendered,
                None => {
                    println!("term is unrenderable:");
                    println!("{:?}", normalised_term);
                    println!("{:?}", ctx);
                    continue;
                },
            };

            // TODO: indentation.
            let new_text = rendered;

            return Some(Diff {
                start_pos: old_span.start,
                remove_len: old_span.end - old_span.start,
                insert_text: new_text,
            });
        }
        println!("tried all terms");
        None
    }

    fn reparse(&mut self) {
        self.parsed = None;

        println!("about to parse");
        let mut parsed = match parse::grammar::doc(&self.text) {
            Ok(parsed) => parsed,
            Err(e) => {
                println!("error parsing file: {:?}", e);
                return;
            },
        };
        println!("finished parsing");

        match construct::build_block_expr(&core::Ctx::nil(), &mut parsed) {
            Ok(term) => {
                println!("Successfully built term: {}", debug_render_term(&core::Ctx::nil(), &term));
            },
            Err(e) => {
                println!("Failed to build term for file!: {}", e);
            },
        };

        self.parsed = Some(parsed);
    }

    pub fn apply_diff(&mut self, diff: Diff, stamp: usize) {
        /*
        let mut new_text = String::with_capacity(self.text.len());
        for chunk in self.text.split(&diff.old_match) {
            new_text.push_str(chunk);
            new_text.push_str(&diff.new_text);
        }
        new_text.shrink_to_fit();
        */

        //assert_eq!(stamp, self.stamp + 1);
        self.stamp = stamp;

        let mut new_text = String::with_capacity(diff.insert_text.len() + self.text.len() - diff.remove_len);
        new_text.push_str(&self.text[.. diff.start_pos]);
        new_text.push_str(&diff.insert_text);
        new_text.push_str(&self.text[diff.start_pos + diff.remove_len ..]);

        self.text = new_text;
        self.reparse();

        println!("{:?}", diff);
        println!("#####");
        println!("{}", self.text);
        println!("#####");
    }
}


