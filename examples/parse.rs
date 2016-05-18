extern crate malkc;
extern crate env_logger;

use std::env;
use std::fs::File;
use std::io::Read;
use std::str;
use std::fmt;
use malkc::{parse, lexer};

fn main() {
    env_logger::init().unwrap();

    let fname = match env::args().nth(1) {
        Some(s) => s,
        None => {
            println!("Expected a file name argument");
            return;
        },
    };

    let mut file = match File::open(fname) {
        Ok(f) => f,
        Err(e) => {
            println!("Error opening file: {}", e);
            return;
        },
    };

    let mut data = Vec::new();
    match file.read_to_end(&mut data) {
        Ok(_) => (),
        Err(e) => {
            println!("Error reading from file: {}", e);
            return;
        },
    };

    let src = match str::from_utf8(&data) {
        Ok(src) => src,
        Err(e) => {
            println!("Invalid utf-8 in file: {}", e);
            return;
        },
    };

    let tokens = match lexer::lex(&src) {
        Ok(tokens) => tokens,
        Err(e) => {
            println!("Error lexing file: {}", e);
            return;
        },
    };

    let expr = match parse::parse_expr(&tokens) {
        Ok(expr) => expr,
        Err(e) => {
            println!("Error parsing file: {}", e);
            return;
        },
    };

    println!("Success");
    println!("{}", Displayer(expr));
}

struct Displayer(parse::Expr);

impl fmt::Display for Displayer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Displayer(ref expr) = *self;
        parse::pprint_expr(expr, f)
    }
}

