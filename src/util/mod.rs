#[macro_use]
mod result;

mod span;
mod term_input;

pub use crate::util::{
    result::{Context, Expect, PError, PResult},
    span::Span,
    term_input::TermChars,
};
