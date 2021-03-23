//#![allow(incomplete_features)]
//#![allow(stable_features)]
//#![feature(str_strip)]
//#![feature(weak_into_raw)]
//#![feature(bindings_after_at)]
//#![feature(or_patterns)]
//#![feature(let_chains)]

#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate maplit;
#[macro_use]
extern crate log;
#[macro_use]
extern crate codespan_derive;

#[macro_use]
mod util;

mod ctx;
mod lexer;
mod parser;
mod ux;

use ux::repl;

fn main() {
    repl();
}
