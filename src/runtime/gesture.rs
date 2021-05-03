// Gesture Treewalk Interpreter (TM)

use std::{fmt::Display, sync::Arc};

use super::{value::Value, Env, Method, Runtime};
use crate::{
    parser::ast::{Expr, ExprKind, Pattern, PatternKind, Spanned, StringPart},
    util::{
        pretty::{Delimited, Pretty},
        Id,
    },
};

#[derive(Debug)]
pub enum EvalError {
    Msg(String),
    Panic(String),
}

pub fn eval(runtime: &mut Runtime, env_id: Id<Env>, expr: &Expr) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Nothing => Ok(Value::Nothing),
        ExprKind::Num(n) => match n.parse::<f64>() {
            Ok(f) => Ok(Value::Num(f)),
            Err(p) => Err(EvalError::Msg(p.to_string())),
        },
        ExprKind::Str(s) => Ok(Value::Str(s.clone())),
        ExprKind::InterpolatedStr(parts) => {
            let mut acc = String::new();
            for part in parts {
                match part {
                    StringPart::Str(_, s) => acc.push_str(s),
                    StringPart::Expr(expr) => {
                        let value = eval(runtime, env_id, expr)?;
                        let as_str_args = &[
                            value,
                            Value::Atom("as".to_string()),
                            Value::Atom("Str".to_string()),
                        ];
                        match call(runtime, env_id, as_str_args)? {
                            Value::Str(s) => acc.push_str(&s),
                            v => {
                                return Err(EvalError::Msg(format!(
                                    "x :as :Str returned a non-Str value: {}",
                                    Pretty(v, runtime),
                                )))
                            }
                        }
                    }
                }
            }
            Ok(Value::Str(acc))
        }
        ExprKind::Atom(a) => Ok(Value::Atom(a.clone())),
        ExprKind::List(exprs) => {
            let mut values = Vec::new();
            for e in exprs {
                values.extend(eval_spread(runtime, env_id, e)?);
            }
            Ok(Value::List(values))
        }
        ExprKind::Id(x) => runtime.heap.get_var(env_id, x),
        ExprKind::TypeId(x) => runtime.heap.get_var(env_id, x),
        ExprKind::Let(pat, expr) => {
            let value = eval(runtime, env_id, &expr)?;
            destructure(runtime, env_id, &pat, value).map(|_| Value::Nothing)
        }
        ExprKind::Assign(lhs, rhs) => {
            let value = eval(runtime, env_id, &rhs)?;
            match &lhs.kind {
                ExprKind::Id(x) => {
                    runtime.heap.set_var(env_id, x.clone(), value)?;
                }
                _ => {
                    return Err(EvalError::Msg(format!(
                        "expr {:?} cannot be used on the left hand side of an assignment",
                        lhs
                    )));
                }
            }
            Ok(Value::Nothing)
        }
        ExprKind::PrefixOp(_, op, expr) => {
            let value = eval(runtime, env_id, expr)?;
            call(runtime, env_id, &[Value::Atom(op.clone()), value])
        }
        ExprKind::PostfixOp(_, op, expr) => {
            let value = eval(runtime, env_id, expr)?;
            call(runtime, env_id, &[value, Value::Atom(op.clone())])
        }
        ExprKind::BinOp(_, op, lhs, rhs) => {
            let lhs = eval(runtime, env_id, lhs)?;
            let rhs = eval(runtime, env_id, rhs)?;
            call(runtime, env_id, &[lhs, Value::Atom(op.clone()), rhs])
        }
        ExprKind::Spread(_) => {
            let args = eval_spread(runtime, env_id, expr)?;
            call(runtime, env_id, &args)
        }
        ExprKind::Call(callee, exprs) => {
            let mut args = Vec::new();
            args.extend(eval_spread(runtime, env_id, callee)?);
            for expr in exprs {
                args.extend(eval_spread(runtime, env_id, expr)?);
            }
            call(runtime, env_id, &args)
        }
        ExprKind::Group(exprs) => eval_sequence(runtime, env_id, exprs),
        ExprKind::Fn(span, pats, result) => {
            let env = runtime.heap.envs.get_mut(&env_id).unwrap();
            env.method_table.insert_method(Method {
                span: *span,
                env_id,
                pats: pats.clone(),
                result: result.clone(),
            })?;
            Ok(Value::Nothing)
        }
        ExprKind::Lambda(_, _) => Ok(Value::Nothing),
        ExprKind::Matchbox(_) => Ok(Value::Nothing),
        ExprKind::Lazy(exprs) => Ok(Value::Lazy(env_id, Arc::new(exprs.clone()))),
    }
}

/// Evaluates an expression, spreading values if there is a spread expr.
pub fn eval_spread(
    runtime: &mut Runtime,
    env_id: Id<Env>,
    expr: &Expr,
) -> Result<Vec<Value>, EvalError> {
    match &expr.kind {
        ExprKind::Spread(inner) => {
            let value = eval(runtime, env_id, inner)?;
            // TODO: iterator protocol
            match value {
                Value::List(values) => Ok(values),
                _ => Err(EvalError::Msg(format!("Cannot spread value: {:?}", value))),
            }
        }
        _ => Ok(vec![eval(runtime, env_id, expr)?]),
    }
}

pub fn destructure(
    runtime: &mut Runtime,
    env_id: Id<Env>,
    pat: &Pattern,
    value: Value,
) -> Result<(), EvalError> {
    if let Some(bindings) = try_destructure(runtime, pat, value.clone())? {
        apply_bindings(runtime, env_id, bindings)?;
    }
    Ok(())
}

pub enum Binding {
    Simple(String, Value),
    Strict(Option<String>, Value, bool),
}

fn apply_bindings(
    runtime: &mut Runtime,
    env_id: Id<Env>,
    bindings: Vec<Binding>,
) -> Result<(), EvalError> {
    for binding in bindings {
        match binding {
            Binding::Simple(name, value) => runtime.heap.define_var(env_id, name, value),
            Binding::Strict(name, value, full) => {
                let value = if full {
                    unravel(runtime, value)?
                } else {
                    unwrap(runtime, value)?
                };
                if let Some(name) = name {
                    runtime.heap.define_var(env_id, name, value);
                }
            }
        }
    }

    Ok(())
}

/*
pub fn lookup_method<'a>(
    runtime: &'a Runtime,
    args: &[Value],
) -> Result<(&'a Method, Vec<Binding>), Vec<&'a Method>> {
    let methods = &runtime.methods;
    let mut methods: Vec<_> = methods
        .iter()
        .filter_map(|m| match try_destructure_multiple(runtime, &m.pats, args) {
            Ok(bindings) => Some((m, bindings)),
            Err(_) => None,
        })
        .collect();

    if methods.len() == 1 {
        Ok(methods.into_iter().nth(0).unwrap())
    } else {
        methods.sort_unstable_by(|(a, _), (b, _)| {
            a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)
        });

        Err(methods.iter().map(|(m, _)| *m).collect())
    }
}
 */

fn mismatch<R>(expected: impl Display, got: &Value, runtime: &Runtime) -> Result<R, EvalError> {
    Err(EvalError::Msg(format!(
        "Expected {} but got {}",
        expected,
        Pretty(got, runtime),
    )))
}

pub fn try_destructure(
    runtime: &Runtime,
    pat: &Pattern,
    value: Value,
) -> Result<Option<Vec<Binding>>, EvalError> {
    Ok(match &pat.kind {
        PatternKind::Id(x) => Some(vec![Binding::Simple(x.clone(), value)]),
        PatternKind::TypeId(_) => todo!(),
        PatternKind::Ignore => None,
        PatternKind::Atom(pat_a) => match &value {
            Value::Atom(val_a) if pat_a == val_a => None,
            _ => return mismatch(pat_a, &value, runtime),
        },
        PatternKind::Str(pat_s) => match &value {
            Value::Str(val_s) if pat_s == val_s => None,
            _ => return mismatch(pat_s, &value, runtime),
        },
        PatternKind::Num(pat_n) => match &value {
            Value::Num(val_n) => match pat_n.parse::<f64>() {
                Ok(pat_n) if pat_n == *val_n => None,
                Ok(pat_n) => return mismatch(pat_n, &value, runtime),
                Err(_) => return Err(EvalError::Msg("ICE: non-parseable float".to_string())),
            },
            _ => return mismatch(pat_n, &value, runtime),
        },
        PatternKind::Nothing => match value {
            Value::Nothing => None,
            _ => return mismatch("nothing", &value, runtime),
        },
        PatternKind::List(pats) => match &value {
            Value::List(values) => Some(try_destructure_multiple(runtime, pats, values)?),
            _ => return mismatch("a list", &value, runtime),
        },
        PatternKind::Spread(_) => None,
        PatternKind::Strict { inner, full } => {
            let id = match &inner.kind {
                PatternKind::Id(x) => Some(x.clone()),
                _ => None,
            };
            Some(vec![Binding::Strict(id, value, *full)])
        }
        PatternKind::Type(var, Spanned(_, ty)) => {
            let matches = match value {
                Value::Nothing => ty == "Nothing",
                Value::Num(_) => ty == "Num",
                Value::Str(_) => ty == "Str",
                Value::Atom(_) => ty == "Atom",
                Value::List(_) => ty == "List",
                Value::Lazy(_, _) => ty == "Lazy",
            };
            if matches {
                var.as_ref()
                    .map(|Spanned(_, x)| vec![Binding::Simple(x.clone(), value)])
            } else {
                return mismatch(&ty, &value, runtime);
            }
        }
        PatternKind::Constructor(_, _) => todo!(),
    })
}

pub fn try_destructure_multiple(
    runtime: &Runtime,
    pats: &[Pattern],
    values: &[Value],
) -> Result<Vec<Binding>, EvalError> {
    let (length_match, spread_pat) = pats
        .last()
        .and_then(|p| match &p.kind {
            PatternKind::Spread(inner) => Some((pats.len() - 1 <= values.len(), Some(inner))),
            _ => None,
        })
        .unwrap_or((pats.len() == values.len(), None));

    if !length_match {
        return Err(EvalError::Msg("Pattern length doesn't match".to_string()));
    }

    let (values, rest_values) = match spread_pat {
        Some(_) => values.split_at(pats.len() - 1),
        None => (values, &[] as _),
    };

    let pats = match spread_pat {
        Some(_) => pats.split_last().unwrap().1,
        None => pats,
    };

    let mut bindings = Vec::new();
    for (pat, value) in pats.iter().zip(values) {
        if let Some(bs) = try_destructure(runtime, pat, value.clone())? {
            bindings.extend(bs);
        }
    }

    match spread_pat {
        Some(pat) => {
            if let Some(bs) = try_destructure(runtime, pat, Value::List(rest_values.to_vec()))? {
                bindings.extend(bs);
            }
        }
        _ => {}
    }

    Ok(bindings)
}

fn process_index(n: f64) -> Result<usize, EvalError> {
    if n.is_finite() && !n.is_nan() && n.trunc() == n {
        Ok(unsafe { n.to_int_unchecked::<usize>() })
    } else {
        Err(EvalError::Msg(format!("{} is not a valid index", n)))
    }
}

fn call(runtime: &mut Runtime, env_id: Id<Env>, args: &[Value]) -> Result<Value, EvalError> {
    // Native calls
    match &args[..] {
        [Value::List(vs), Value::Num(n)] => {
            let i = process_index(*n)?;
            return match vs.get(i) {
                Some(v) => Ok(v.clone()),
                None => Err(EvalError::Msg(format!(
                    "Index {} out of bounds of list with length {}",
                    n,
                    vs.len(),
                ))),
            };
        }
        [Value::Atom(a), Value::List(vs), v] if a == "__list_push__" => {
            let mut vs = vs.clone();
            vs.push(v.clone());
            return Ok(Value::List(vs));
        }
        [Value::Atom(a), Value::List(vs)] if a == "__list_len__" => {
            return Ok(Value::Num(vs.len() as f64));
        }
        [Value::Atom(a), Value::List(vs), Value::Num(n), x] if a == "__list_where__" => {
            let i = process_index(*n)?;
            let vs = vs
                .iter()
                .cloned()
                .enumerate()
                .map(|(idx, v)| if idx == i { x.clone() } else { v })
                .collect();
            return Ok(Value::List(vs));
        }
        [Value::Atom(a), v] if a == "__print__" => {
            println!("{}", Pretty(v, runtime));
            return Ok(Value::Nothing);
        }
        [Value::Atom(a), Value::Str(s)] if a == "__panic__" => {
            return Err(EvalError::Panic(s.clone()));
        }
        [Value::Atom(a), Value::Num(x), Value::Num(y)] if a == "__add__" => {
            return Ok(Value::Num(x + y));
        }
        [Value::Atom(a), Value::Num(x), Value::Num(y)] if a == "__sub__" => {
            return Ok(Value::Num(x - y));
        }
        [Value::Atom(a), x, y] if a == "__eq__" => {
            return Ok(Value::Atom(
                if x == y { "true" } else { "false" }.to_string(),
            ));
        }
        [Value::Atom(a), Value::Num(x), Value::Num(y)] if a == "__lt__" => {
            return Ok(Value::Atom(
                if x < y { "true" } else { "false" }.to_string(),
            ));
        }
        [Value::Atom(a), Value::Num(x)] if a == "__neg__" => {
            return Ok(Value::Num(-x));
        }
        [Value::Atom(a), v] if a == "__intrinsic_as_str__" => {
            return Ok(Value::Str(format!("{}", Pretty(v, runtime))));
        }
        _ => {}
    }

    let (env_id, bindings, result) = match runtime.lookup_method(env_id, args) {
        Ok((method, bindings)) => (method.env_id, bindings, method.result.clone()),
        Err(candidates) => {
            if candidates.len() == 0 {
                return Err(EvalError::Msg(format!(
                    "No methods found for args: {}",
                    Pretty(Delimited("", " ", "", args), runtime),
                )));
            } else {
                return Err(EvalError::Msg(format!(
                    "Ambiguous methods for args: {}\nCandidates:\n{}",
                    Pretty(Delimited("", " ", "", args), runtime),
                    Delimited("", "\n", "", &candidates),
                )));
            }
        }
    };

    let env_id = runtime.heap.alloc_child_env(env_id);
    apply_bindings(runtime, env_id, bindings)?;
    eval(runtime, env_id, &result)
}

pub fn eval_sequence(
    runtime: &mut Runtime,
    env_id: Id<Env>,
    exprs: &[Expr],
) -> Result<Value, EvalError> {
    let new_env_id = runtime.heap.alloc_child_env(env_id);
    if let Some((last, init)) = exprs.split_last() {
        for expr in init {
            let val = eval(runtime, new_env_id, expr)?;
            unravel(runtime, val)?;
        }
        Ok(eval(runtime, new_env_id, last)?)
    } else {
        Ok(Value::Nothing)
    }
}

pub fn unwrap(runtime: &mut Runtime, value: Value) -> Result<Value, EvalError> {
    match value {
        Value::Lazy(env_id, exprs) => eval_sequence(runtime, env_id, &exprs),
        _ => Ok(value),
    }
}

pub fn unravel(runtime: &mut Runtime, value: Value) -> Result<Value, EvalError> {
    let mut value = value;
    loop {
        match value {
            Value::Lazy(env_id, exprs) => {
                value = eval_sequence(runtime, env_id, &exprs)?;
            }
            _ => return Ok(value),
        }
    }
}

pub fn unravel_eval(
    runtime: &mut Runtime,
    env_id: Id<Env>,
    expr: &Expr,
) -> Result<Value, EvalError> {
    let value = eval(runtime, env_id, expr)?;
    unravel(runtime, value)
}
