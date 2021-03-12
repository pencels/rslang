use crate::{lexer::Token, value::Value};

#[derive(Debug)]
pub enum ExprKind<'a> {
    Nothing,
    Num(f64),
    Str(String),
    Bool(bool),
    Atom(String),
    List(&'a [Expr<'a>]),

    Id(String),
    TypeId(String),

    Let(&'a Pattern<'a>, &'a Expr<'a>),
    Assign(&'a Expr<'a>, &'a Expr<'a>),
    BinOp(Token, &'a Expr<'a>, &'a Expr<'a>),
    Call(&'a Expr<'a>, &'a [Expr<'a>]),
    Sequence(&'a [Expr<'a>]),

    Fn(&'a Expr<'a>, &'a [Pattern<'a>], &'a Expr<'a>),
    Lambda(&'a [Pattern<'a>], &'a Expr<'a>),
    Matchbox(&'a [Expr<'a>]),
    MatchboxRow(&'a [Pattern<'a>], Option<&'a Expr<'a>>, &'a Expr<'a>),
}

#[derive(Debug)]
pub struct Expr<'a> {
    kind: ExprKind<'a>,
}

#[derive(Debug)]
pub enum PatternKind<'a> {
    Id(String),
    Ignore,
    Literal(Value),
    List(&'a [Pattern<'a>]),
    Strict(&'a Pattern<'a>),
    Type(TypeExpr),
    Constructor(String, &'a [Pattern<'a>]),
}

#[derive(Debug)]
pub struct Pattern<'a> {
    kind: PatternKind<'a>,
}

#[derive(Debug)]
pub enum TypeExprKind {
    Type(String, Vec<TypeArgExpr>),
}

#[derive(Debug)]
pub struct TypeExpr {
    kind: TypeExprKind,
}

#[derive(Debug)]
pub enum TypeArgExprKind {
    Generic(String),
    Type(TypeExpr),
}

#[derive(Debug)]
pub struct TypeArgExpr {
    kind: TypeArgExprKind,
}
