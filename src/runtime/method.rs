use std::{cmp::Ordering, fmt::Display};

use crate::{
    parser::ast::{Expr, Pattern, PatternKind},
    util::{pretty::Delimited, Id, Span, P},
};

use super::{
    gesture::{try_destructure_multiple, Binding, EvalError},
    value::Value,
    Env, Runtime,
};

pub trait Specificity {
    fn cmp_specificity(&self, other: &Self) -> Option<Ordering>;
}

#[derive(Debug)]
pub struct Method {
    pub span: Span,
    pub pats: Vec<Pattern>,
    pub env_id: Id<Env>,
    pub result: P<Expr>,
}

impl Method {
    pub fn new(span: Span, env_id: Id<Env>, pats: Vec<Pattern>, result: P<Expr>) -> Method {
        assert!(pats.len() > 0); // Patterns must be non-empty.

        Method {
            span,
            pats,
            env_id,
            result,
        }
    }

    pub fn has_spread(&self) -> bool {
        match &self.pats.split_last().unwrap().0.kind {
            PatternKind::Spread(_) => true,
            _ => false,
        }
    }

    fn matching_len(&self) -> usize {
        if self.has_spread() {
            self.pats.len() - 1
        } else {
            self.pats.len()
        }
    }
}

impl Specificity for Method {
    fn cmp_specificity(&self, other: &Method) -> Option<Ordering> {
        // TODO: deal with varargs...

        // Handle fixed length case.
        match self.pats.len().cmp(&other.pats.len()) {
            Ordering::Equal => self
                .pats
                .iter()
                .zip(&other.pats)
                .map(|(a, b)| a.cmp_specificity(b))
                .reduce(|acc, ord| match (acc, ord) {
                    // If a pair is equal, don't consider it. Only consider gt/lt relationships,
                    // which must all be in the same direction.
                    (Some(Ordering::Equal), _) | (_, Some(Ordering::Equal)) => acc,
                    _ => {
                        if acc == ord {
                            acc
                        } else {
                            None
                        }
                    }
                })
                .unwrap_or(Some(Ordering::Equal)),
            ord => Some(ord),
        }
    }
}

impl Display for Method {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Delimited("", " ", "", &self.pats).fmt(f)
    }
}

pub struct MethodTable {
    methods: Vec<Method>,
}

impl MethodTable {
    pub fn new() -> MethodTable {
        MethodTable { methods: vec![] }
    }

    pub fn methods(&self) -> &[Method] {
        &self.methods
    }

    pub fn insert_method(&mut self, method: Method) -> Result<(), EvalError> {
        // TODO: handle method overriding
        // match None {
        //     Some(i) => {
        //         let old_method = std::mem::replace(&mut self.methods[i], method);
        //         Err(EvalError::Msg(format!(
        //             "Method {} is not unique and will overwrite existing method {} in scope",
        //             self.methods[i], old_method,
        //         )))
        //     }
        //     None => {
        //         self.methods.push(method);
        //         self.methods
        //             .sort_unstable_by(|a, b| b.cmp_specificity(a).unwrap_or(Ordering::Equal));
        //         Ok(())
        //     }
        // }

        self.methods.push(method);
        self.methods
            .sort_unstable_by(|a, b| b.cmp_specificity(a).unwrap_or(Ordering::Equal));
        Ok(())
    }
}
