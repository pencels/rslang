use crate::{
    parser::{ast::Expr, prec::Poset},
    util::{
        pretty::{Delimited, Pretty, PrettyPrint},
        Id, P,
    },
};

use std::cmp::Ordering;

use super::{Env, Runtime};

#[derive(Debug, Clone)]
pub enum Value {
    Nothing,
    Num(f64),
    Str(String),
    Atom(String),
    List(Vec<Value>),
    Lazy(Id<Env>, P<Vec<Expr>>),
    Struct(String, Vec<Value>),
}

pub fn type_of(value: &Value) -> String {
    match value {
        Value::Nothing => "Nothing",
        Value::Num(_) => "Num",
        Value::Str(_) => "Str",
        Value::Atom(ty) => ty,
        Value::List(_) => "List",
        Value::Lazy(_, _) => "Lazy",
        Value::Struct(ty, _) => ty,
    }
    .to_owned()
}

pub fn is_subtype_of(lhs: &String, rhs: &String, poset: &Poset<String>) -> bool {
    lhs == rhs || matches!(poset.cmp(lhs, rhs), Some(Ordering::Less))
}

pub fn is_instance_of(value: &Value, ty: &String, runtime: &Runtime) -> bool {
    match value {
        Value::Nothing => ty == "Nothing",
        Value::Num(_) => ty == "Num",
        Value::Str(_) => ty == "Str",
        Value::Atom(atom_ty) => {
            if runtime.empty_variants.contains(atom_ty) {
                is_subtype_of(atom_ty, ty, &runtime.subtypes) || ty == "Atom"
            } else {
                ty == "Atom"
            }
        }
        Value::List(_) => ty == "List",
        Value::Lazy(_, _) => ty == "Lazy",
        Value::Struct(struct_ty, _) => is_subtype_of(struct_ty, ty, &runtime.subtypes),
    }
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
            Value::Atom(a) => {
                if a.contains(|c: char| c.is_whitespace() || c == ':') {
                    write!(f, ":`{}`", a)
                } else if a.starts_with(|c: char| c.is_uppercase())
                    && !a.contains(|c: char| !c.is_alphanumeric())
                {
                    write!(f, "{}", a)
                } else {
                    write!(f, ":{}", a)
                }
            }
            Value::List(xs) => Delimited("[", ", ", "]", xs).fmt(f, ctx),
            Value::Lazy(_, exprs) => write!(f, "<Lazy>"), // TODO: Delimited("{", "; ", "}", exprs).fmt(f, ctx),
            Value::Struct(ty, args) => {
                write!(f, "{}", ty)?;
                if !args.is_empty() {
                    Delimited(" ", " ", "", args).fmt(f, ctx)?;
                }
                Ok(())
            }
        }
    }
}
