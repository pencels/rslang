// Gesture Treewalk Interpreter (TM)

use std::{fmt::Display, sync::Arc};

use super::{
    make_bool_atom,
    value::{is_instance_of, is_subtype_of, Value},
    Env, Method, Runtime,
};
use crate::{
    parser::{
        ast::{Defn, DefnKind, Expr, ExprKind, Pattern, PatternKind, Spanned, StringPart},
        result::ParseResult,
        Parser,
    },
    util::{
        fresh_id,
        pretty::{Delimited, Pretty},
        Id, Span,
    },
};

#[derive(Debug)]
pub enum EvalError {
    Msg(String),
    Panic(String),
}

pub fn eval_defn(
    parser: &mut Parser,
    defn: &Defn,
    runtime: &mut Runtime,
    env_id: Id<Env>,
) -> ParseResult<()> {
    let Defn { kind, .. } = defn;
    match kind {
        DefnKind::Operator {
            fixity,
            assoc,
            ops,
            constraints,
        } => {
            parser.add_operator(fixity, assoc, ops, constraints)?;
        }
        DefnKind::Expr(expr) => match unravel_eval(runtime, env_id, expr) {
            Ok(Value::Nothing) => {}
            Ok(v) => println!("{}", Pretty(v, runtime)),
            Err(EvalError::Msg(msg)) => println!("Error: {}", msg),
            Err(EvalError::Panic(msg)) => println!("Panicked: {}", msg),
        },
        DefnKind::Struct {
            name,
            args,
            subtype,
        } => {
            let Spanned(span, name) = name;
            if let Some(Spanned(_, ty)) = subtype {
                runtime
                    .subtypes
                    .try_add_lt(name.clone(), ty.clone())
                    .unwrap();
            }

            // A bare struct name is essentially a no-op, as struct types are just atoms.
            if args.is_empty() {
                runtime.empty_variants.insert(name.clone());
                return Ok(());
            }

            // TODO: this is gross
            let mut pats = args.clone();
            pats.insert(
                0,
                Pattern {
                    span: *span,
                    kind: PatternKind::TypeId(name.clone()),
                },
            );
            let mut call_args: Vec<_> = args
                .iter()
                .map(|p| {
                    let kind = match &p.kind {
                        PatternKind::Id(x) => ExprKind::Id(x.clone()),
                        PatternKind::TypeId(x) => ExprKind::TypeId(x.clone()),
                        PatternKind::Ignore => ExprKind::Nothing,
                        PatternKind::Atom(x) => ExprKind::Atom(x.clone()),
                        PatternKind::Str(s) => ExprKind::Str(s.clone()),
                        PatternKind::Num(n) => ExprKind::Num(n.clone()),
                        PatternKind::Nothing => ExprKind::Nothing,
                        PatternKind::List(_) => todo!(),
                        PatternKind::Spread(_) => todo!(),
                        PatternKind::Strict { inner, full } => todo!(),
                        PatternKind::Type(Some(Spanned(_, x)), Spanned(_, ty)) => {
                            ExprKind::Id(x.clone())
                        }
                        PatternKind::Type(None, Spanned(_, ty)) => todo!(),
                        PatternKind::Constructor(_, _) => todo!(),
                    };

                    Expr { span: p.span, kind }
                })
                .collect();
            call_args.insert(
                0,
                Expr {
                    span: Span::dummy(),
                    kind: ExprKind::TypeId(name.clone()),
                },
            );

            let result = Arc::new(Expr {
                span: Span::dummy(),
                kind: ExprKind::Call(
                    Arc::new(Expr {
                        span: Span::dummy(),
                        kind: ExprKind::Atom("__make_struct__".to_string()),
                    }),
                    call_args,
                ),
            });

            // TODO: uhh figure out how to do errors here, this is a weird mixing of parse + eval errors
            let env = runtime.heap.envs.get_mut(&env_id).unwrap();
            env.method_table.insert_method(
                Method {
                    span: *span,
                    env_id,
                    pats,
                    result,
                },
                &runtime.subtypes,
            );
        }
    }

    Ok(())
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
        ExprKind::TypeId(x) => Ok(Value::Atom(x.clone())),
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
            env.method_table.insert_method(
                Method {
                    span: *span,
                    env_id,
                    pats: pats.clone(),
                    result: result.clone(),
                },
                &runtime.subtypes,
            )?;
            Ok(Value::Nothing)
        }
        ExprKind::Lambda(pats, result) => {
            let env = runtime.heap.envs.get_mut(&runtime.root_env).unwrap();

            let lambda_atom = format!("lambda-{}", fresh_id::<usize>());
            let lambda_atom_pat = Pattern {
                span: Span::dummy(),
                kind: PatternKind::Atom(lambda_atom.clone()),
            };
            let mut pats = pats.clone();
            pats.insert(0, lambda_atom_pat);

            env.method_table.insert_method(
                Method {
                    span: expr.span,
                    env_id,
                    pats,
                    result: result.clone(),
                },
                &runtime.subtypes,
            )?;
            Ok(Value::Atom(lambda_atom))
        }
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

#[derive(Debug)]
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
        PatternKind::TypeId(pat_x) => match &value {
            Value::Atom(val_x) if pat_x == val_x => None,
            _ => return mismatch(pat_x, &value, runtime),
        },
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
            Value::List(values) => Some(try_destructure_exact_length(runtime, pats, values)?),
            _ => return mismatch("a list", &value, runtime),
        },
        PatternKind::Spread(_) => None,
        PatternKind::Strict { inner, full } => match value {
            Value::Lazy(_, _) => {
                let id = match &inner.kind {
                    PatternKind::Id(x) => Some(x.clone()),
                    _ => None,
                };
                Some(vec![Binding::Strict(id, value, *full)])
            }
            _ => return mismatch("a lazy", &value, runtime),
        },
        PatternKind::Type(var, Spanned(_, ty)) => {
            if is_instance_of(&value, ty, runtime) {
                var.as_ref()
                    .map(|Spanned(_, x)| vec![Binding::Simple(x.clone(), value)])
            } else {
                return mismatch(&ty, &value, runtime);
            }
        }
        PatternKind::Constructor(Spanned(_, pat_ty), pats) => match &value {
            Value::Struct(ty, args) if pat_ty == ty => {
                Some(try_destructure_exact_length(runtime, pats, args)?)
            }
            _ => return mismatch(&pat_ty, &value, runtime),
        },
    })
}

pub fn try_destructure_exact_length(
    runtime: &Runtime,
    pats: &[Pattern],
    values: &[Value],
) -> Result<Vec<Binding>, EvalError> {
    let (bindings, _) = try_destructure_multiple(runtime, pats, values, false)?;
    Ok(bindings)
}

pub fn try_destructure_prefix<'a>(
    runtime: &Runtime,
    pats: &[Pattern],
    values: &'a [Value],
) -> Result<(Vec<Binding>, &'a [Value]), EvalError> {
    try_destructure_multiple(runtime, pats, values, true)
}

fn try_destructure_multiple<'a>(
    runtime: &Runtime,
    pats: &[Pattern],
    values: &'a [Value],
    allow_trailing: bool,
) -> Result<(Vec<Binding>, &'a [Value]), EvalError> {
    let (length_match, spread_pat) = pats
        .last()
        .and_then(|p| match &p.kind {
            PatternKind::Spread(inner) => Some((pats.len() - 1 <= values.len(), Some(inner))),
            _ => None,
        })
        .unwrap_or((
            if allow_trailing {
                pats.len() <= values.len()
            } else {
                pats.len() == values.len()
            },
            None,
        ));

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

    Ok((bindings, &values[pats.len()..]))
}

fn process_index(n: f64) -> Result<usize, EvalError> {
    if n.is_finite() && !n.is_nan() && n.trunc() == n {
        Ok(unsafe { n.to_int_unchecked::<usize>() })
    } else {
        Err(EvalError::Msg(format!("{} is not a valid index", n)))
    }
}

fn call<'a>(runtime: &mut Runtime, env_id: Id<Env>, args: &'a [Value]) -> Result<Value, EvalError> {
    // println!("call: {}", Pretty(Delimited("", " ", "", args), runtime));

    match call_with_trailing(runtime, env_id, args)? {
        (v, []) => Ok(v),
        (v, rest) => {
            let mut new_args = vec![v];
            new_args.extend_from_slice(rest);
            call(runtime, env_id, &new_args)
        }
    }
}

fn call_with_trailing<'a>(
    runtime: &mut Runtime,
    env_id: Id<Env>,
    args: &'a [Value],
) -> Result<(Value, &'a [Value]), EvalError> {
    // Native calls
    match &args[..] {
        [Value::List(vs), Value::Num(n), rest @ ..] => {
            let i = process_index(*n)?;
            return match vs.get(i) {
                Some(v) => Ok((v.clone(), rest)),
                None => Err(EvalError::Msg(format!(
                    "Index {} out of bounds of list with length {}",
                    n,
                    vs.len(),
                ))),
            };
        }
        [Value::Atom(a), Value::Atom(ty), args @ ..] if a == "__make_struct__" => {
            return Ok((Value::Struct(ty.clone(), args.to_vec()), &[]));
        }
        [Value::Atom(a), Value::List(vs), v, rest @ ..] if a == "__list_push__" => {
            let mut vs = vs.clone();
            vs.push(v.clone());
            return Ok((Value::List(vs), rest));
        }
        [Value::Atom(a), Value::List(vs), rest @ ..] if a == "__list_len__" => {
            return Ok((Value::Num(vs.len() as f64), rest));
        }
        [Value::Atom(a), Value::List(vs), Value::Num(n), x, rest @ ..] if a == "__list_where__" => {
            let i = process_index(*n)?;
            let vs = vs
                .iter()
                .cloned()
                .enumerate()
                .map(|(idx, v)| if idx == i { x.clone() } else { v })
                .collect();
            return Ok((Value::List(vs), rest));
        }
        [Value::Atom(a), v, rest @ ..] if a == "__print__" => {
            println!("{}", Pretty(v, runtime));
            return Ok((Value::Nothing, rest));
        }
        [Value::Atom(a), Value::Str(s), ..] if a == "__panic__" => {
            return Err(EvalError::Panic(s.clone()));
        }
        [Value::Atom(a), Value::Num(x), Value::Num(y), rest @ ..] if a == "__add__" => {
            return Ok((Value::Num(x + y), rest));
        }
        [Value::Atom(a), Value::Num(x), Value::Num(y), rest @ ..] if a == "__sub__" => {
            return Ok((Value::Num(x - y), rest));
        }
        [Value::Atom(a), x, y, rest @ ..] if a == "__eq__" => {
            return Ok((make_bool_atom(x == y), rest));
        }
        [Value::Atom(a), Value::Num(x), Value::Num(y), rest @ ..] if a == "__lt__" => {
            return Ok((make_bool_atom(x < y), rest));
        }
        [Value::Atom(a), Value::Num(x), rest @ ..] if a == "__neg__" => {
            return Ok((Value::Num(-x), rest));
        }
        [Value::Atom(a), v, rest @ ..] if a == "__intrinsic_as_str__" => {
            return Ok((Value::Str(format!("{}", Pretty(v, runtime))), rest));
        }
        [Value::Atom(a), lhs, Value::Atom(rhs), rest @ ..] if a == "__is_instance_of__" => {
            return Ok((make_bool_atom(is_instance_of(lhs, rhs, runtime)), rest));
        }
        [Value::Atom(a), Value::Atom(lhs), Value::Atom(rhs), rest @ ..]
            if a == "__is_subtype_of__" =>
        {
            return Ok((
                make_bool_atom(is_subtype_of(lhs, rhs, &runtime.subtypes)),
                rest,
            ));
        }
        _ => {}
    }

    let (env_id, bindings, result, rest) = match runtime.lookup_method(env_id, args) {
        Ok((method, bindings, rest)) => (method.env_id, bindings, method.result.clone(), rest),
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
    Ok((eval(runtime, env_id, &result)?, rest))
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
