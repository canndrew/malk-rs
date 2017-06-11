#![feature(never_type)]
#![feature(box_syntax)]
#![feature(plugin)]
#![feature(question_mark_carrier)]
#![plugin(peg_syntax_ext)]

//#[macro_use]
//extern crate quick_error;
//#[macro_use]
//extern crate unwrap;
//#[macro_use]
//extern crate log;

extern crate num;

/*
macro_rules! catch {
    ($e:expr) => (
        {
            use std::ops::Carrier;

            #[cfg(debug_assertions)]
            let r = (|| Debug::from_success($e))();

            #[cfg(not(debug_assertions))]
            let r = Debug::from_error(());

            r
        }
    )
}
*/

pub mod parse;
pub mod core;
pub mod render;
pub mod construct;
pub mod debug;

