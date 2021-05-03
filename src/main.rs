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
mod runtime;
mod ux;

use ux::repl;

fn main() {
    repl();
}
