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
        name: String,
        name_span: Span,
        args: Vec<Spanned<String>>,
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
    Bool(bool),
    Atom(String),
    List(Vec<Expr>),

    Id(String),
    TypeId(String),

    Let(Pattern, P<Expr>),
    Assign(P<Expr>, P<Expr>),
    PrefixOp(Token, P<Expr>),
    PostfixOp(Token, P<Expr>),
    BinOp(Token, P<Expr>, P<Expr>),
    Call(P<Expr>, Vec<Expr>),
    Sequence(Vec<Expr>),

    Fn(P<Expr>, Vec<Pattern>, P<Expr>),
    Lambda(Vec<Pattern>, P<Expr>),
    Matchbox(Vec<Expr>),
    MatchboxRow(Vec<Pattern>, Option<P<Expr>>, P<Expr>),
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum PatternKind {
    Id(String),
    Ignore,
    Literal(Token),
    List(P<Pattern>),
    Strict { id: Token, full: bool },
    Type(String),
    Constructor(String, Vec<Pattern>),
}

#[derive(Debug)]
pub struct Pattern {
    pub kind: PatternKind,
    pub span: Span,
}
