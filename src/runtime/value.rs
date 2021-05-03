use crate::{
    parser::ast::Expr,
    util::{
        pretty::{Delimited, Pretty, PrettyPrint},
        Id, P,
    },
};

use super::{Env, Runtime};

#[derive(Debug, Clone)]
pub enum Value {
    Nothing,
    Num(f64),
    Str(String),
    Atom(String),
    List(Vec<Value>),
    Lazy(Id<Env>, P<Vec<Expr>>),
}

impl Eq for Value {}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nothing, Value::Nothing) => true,
            (Value::Num(x), Value::Num(y)) => x == y,
            (Value::Str(x), Value::Str(y)) => x == y,
            (Value::Atom(x), Value::Atom(y)) => x == y,
            (Value::List(xs), Value::List(ys)) => xs.iter().eq(ys),
            (Value::Lazy(_, _), Value::Lazy(_, _)) => false, // TODO: lazy value equality??? for now always false
            _ => false,
        }
    }
}

impl PrettyPrint<Runtime> for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter, ctx: &Runtime) -> std::fmt::Result {
        match self {
            Value::Nothing => write!(f, "nothing"),
            Value::Num(n) => write!(f, "{}", n),
            Value::Str(s) => write!(f, "\"{}\"", s.replace('"', "\\\"")),
            Value::Atom(a) => write!(f, ":{}", a),
            Value::List(xs) => Delimited("[", ", ", "]", xs).fmt(f, ctx),
            Value::Lazy(_, exprs) => write!(f, "<Lazy>"), // TODO: Delimited("{", "; ", "}", exprs).fmt(f, ctx),
        }
    }
}
