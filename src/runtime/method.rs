use std::{cmp::Ordering, fmt::Display};

use crate::{
    parser::{
        ast::{Expr, Pattern, PatternKind},
        prec::Poset,
    },
    util::{pretty::Delimited, Id, Span, P},
};

use super::{gesture::EvalError, Env, Runtime};

pub trait Specificity {
    fn cmp_specificity(&self, other: &Self, subtypes: &Poset<String>) -> Option<Ordering>;
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
        matches!(
            self.pats.last(),
            Some(Pattern {
                kind: PatternKind::Spread(_),
                ..
            })
        )
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
    fn cmp_specificity(&self, other: &Method, poset: &Poset<String>) -> Option<Ordering> {
        self.pats.cmp_specificity(&other.pats, poset)
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

    pub fn insert_method(
        &mut self,
        method: Method,
        poset: &Poset<String>,
    ) -> Result<(), EvalError> {
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
            .sort_unstable_by(|a, b| b.cmp_specificity(a, poset).unwrap_or(Ordering::Equal));
        Ok(())
    }
}
