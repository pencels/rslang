use crate::{
    lexer::token::Token,
    util::{Span, P},
};

use super::prec::{Associativity, Fixity};

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub span: Span,
    pub item: T,
}

impl<T> Spanned<T> {
    pub fn new(span: Span, item: T) -> Spanned<T> {
        Spanned { span, item }
    }
}

#[derive(Debug)]
pub enum DefnKind {
    Operator {
        fixity: Spanned<Fixity>,
        assoc: Option<Spanned<Associativity>>,
        ops: Vec<Token>,
        constraints: Vec<PrecConstraint>,
    },
    Struct {
        name: Token,
        args: Vec<Token>,
    },
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub struct PrecConstraint {
    pub op: Token,
    pub kind: Spanned<PrecConstraintKind>,
}

#[derive(Debug, Copy, Clone)]
pub enum PrecConstraintKind {
    Above,
    With,
    Below,
}

pub struct Defn {
    pub kind: DefnKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ExprKind {
    Nothing,
    Num(String),
    Str(String),
    InterpolatedStr(Vec<StringPart>),
    Atom(String),
    List(Vec<Expr>),

    Id(String),
    TypeId(String),

    Let(Pattern, P<Expr>),
    Assign(P<Expr>, P<Expr>),
    PrefixOp(Token, P<Expr>),
    PostfixOp(Token, P<Expr>),
    BinOp(Token, P<Expr>, P<Expr>),
    Spread(Token),
    Call(P<Expr>, Vec<Expr>),
    Group(Vec<Expr>),

    Fn(Option<String>, Span, Vec<Pattern>, P<Expr>),
    Lambda(Vec<Pattern>, P<Expr>),
    Matchbox(Vec<MatchboxRow>),
    Lazy(Vec<Expr>),
}

#[derive(Debug)]
pub enum StringPart {
    Str(Span, String),
    Expr(Expr),
}

#[derive(Debug)]
pub struct MatchboxRow {
    pub span: Span,
    pub params: Vec<Pattern>,
    pub guard: Option<P<Expr>>,
    pub result: P<Expr>,
}

#[derive(Debug)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug)]
pub enum PatternKind {
    Id(String),
    TypeId(String),
    Ignore,
    Atom(String),
    Str(String),
    Num(String),
    Nothing,
    List(Vec<Pattern>),
    Spread(Token),
    Strict { inner: P<Pattern>, full: bool },
    Type(Option<Token>, Token),
    Constructor(P<Pattern>, Vec<Pattern>),
}

#[derive(Debug)]
pub struct Pattern {
    pub kind: PatternKind,
    pub span: Span,
}
