use std::{cmp::Ordering, fmt::Display};

use crate::{
    lexer::token::Token,
    runtime::method::Specificity,
    util::{pretty::Delimited, Span, P},
};

use super::prec::{Associativity, Fixity};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spanned<T>(pub Span, pub T);

impl<T> Spanned<T> {
    pub fn new(span: Span, item: T) -> Spanned<T> {
        Spanned(span, item)
    }

    pub fn span(&self) -> Span {
        self.0
    }

    pub fn item(&self) -> &T {
        &self.1
    }

    pub fn take_item(self) -> T {
        self.1
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
        name: Spanned<String>,
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

#[derive(Debug, Clone)]
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
    PrefixOp(Span, String, P<Expr>),
    PostfixOp(Span, String, P<Expr>),
    BinOp(Span, String, P<Expr>, P<Expr>),
    Spread(P<Expr>),
    Call(P<Expr>, Vec<Expr>),
    Group(Vec<Expr>),

    Fn(Span, Vec<Pattern>, P<Expr>),
    Lambda(Vec<Pattern>, P<Expr>),
    Matchbox(Vec<MatchboxRow>),
    Lazy(Vec<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Expr { kind, .. } = self;
        match kind {
            ExprKind::Nothing => write!(f, "nothing")?,
            ExprKind::Num(n) => write!(f, "{}", n)?,
            ExprKind::Str(s) => write!(f, "{}", s)?,
            ExprKind::InterpolatedStr(_) => {}
            ExprKind::Atom(_) => {}
            ExprKind::List(_) => {}
            ExprKind::Id(x) => write!(f, "{}", x)?,
            ExprKind::TypeId(_) => {}
            ExprKind::Let(_, _) => {}
            ExprKind::Assign(_, _) => {}
            ExprKind::PrefixOp(_, _, _) => {}
            ExprKind::PostfixOp(_, _, _) => {}
            ExprKind::BinOp(_, _, _, _) => {}
            ExprKind::Spread(_) => {}
            ExprKind::Call(_, _) => {}
            ExprKind::Group(_) => {}
            ExprKind::Fn(_, _, _) => {}
            ExprKind::Lambda(_, _) => {}
            ExprKind::Matchbox(_) => {}
            ExprKind::Lazy(_) => {}
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum StringPart {
    Str(Span, String),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub struct MatchboxRow {
    pub span: Span,
    pub params: Vec<Pattern>,
    pub guard: Option<P<Expr>>,
    pub result: P<Expr>,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, Clone)]
pub enum PatternKind {
    Id(String),
    TypeId(String),
    Ignore,
    Atom(String),
    Str(String),
    Num(String),
    Nothing,
    List(Vec<Pattern>),
    Spread(P<Pattern>),
    Strict { inner: P<Pattern>, full: bool },
    Type(Option<Spanned<String>>, Spanned<String>),
    Constructor(P<Pattern>, Vec<Pattern>),
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub kind: PatternKind,
    pub span: Span,
}

impl Specificity for Pattern {
    fn cmp_specificity(&self, other: &Self) -> Option<Ordering> {
        match &self.kind {
            // Id and Ignore are irrefutable patterns. They are the least-specific kind of pattern,
            // and are equivalent to each other in specificity.
            PatternKind::Id(_) | PatternKind::Ignore => match &other.kind {
                PatternKind::Id(_) | PatternKind::Ignore => Some(Ordering::Equal),
                _ => Some(Ordering::Less),
            },
            // TypeId, Atom, Num, and Nothing each refer to a singleton value. They are the
            // most-specific kind of pattern, and are equivalent to each other in specificity.
            PatternKind::TypeId(_)
            | PatternKind::Atom(_)
            | PatternKind::Str(_)
            | PatternKind::Num(_)
            | PatternKind::Nothing => match &other.kind {
                PatternKind::TypeId(_)
                | PatternKind::Atom(_)
                | PatternKind::Str(_)
                | PatternKind::Num(_)
                | PatternKind::Nothing => Some(Ordering::Equal),
                _ => Some(Ordering::Greater),
            },
            PatternKind::List(pats) => match &other.kind {
                PatternKind::Id(_) | PatternKind::Ignore => Some(Ordering::Greater),
                PatternKind::TypeId(_)
                | PatternKind::Atom(_)
                | PatternKind::Str(_)
                | PatternKind::Num(_)
                | PatternKind::Nothing => Some(Ordering::Equal),
                PatternKind::List(other_pats) => {
                    // TODO: varargs, length, inner patterns
                    Some(Ordering::Equal)
                }
                PatternKind::Spread(_) => None,
                PatternKind::Strict { .. } => todo!(),
                PatternKind::Type(_, _) => Some(Ordering::Greater),
                PatternKind::Constructor(_, _) => todo!(),
            },
            PatternKind::Spread(_) => None, // XXX: Should not directly compare spreads...
            // Strict patterns are effectively the same as a Lazy type pattern.
            PatternKind::Strict { .. } => match &other.kind {
                PatternKind::Id(_) | PatternKind::Ignore => Some(Ordering::Greater),
                PatternKind::TypeId(_)
                | PatternKind::Atom(_)
                | PatternKind::Str(_)
                | PatternKind::Num(_)
                | PatternKind::Nothing => Some(Ordering::Less),
                PatternKind::Type(_, Spanned(_, ty)) if ty == "Lazy" => Some(Ordering::Equal),
                PatternKind::Type(_, _) => None, // Lazy does not have a relationship with any other types.
                _ => None,
            },
            PatternKind::Type(_, Spanned(_, ty)) => match &other.kind {
                PatternKind::Id(_) | PatternKind::Ignore => Some(Ordering::Greater),
                PatternKind::TypeId(_)
                | PatternKind::Atom(_)
                | PatternKind::Str(_)
                | PatternKind::Num(_)
                | PatternKind::Nothing => Some(Ordering::Less),
                PatternKind::List(_) => Some(Ordering::Less),
                PatternKind::Spread(_) => Some(Ordering::Less), // TODO: spreads
                PatternKind::Strict { .. } => todo!(),
                PatternKind::Type(_, Spanned(_, other_ty)) => {
                    if ty == other_ty {
                        Some(Ordering::Equal)
                    } else {
                        None // TODO: subtyping???
                    }
                }
                PatternKind::Constructor(_, _) => todo!(),
            },
            PatternKind::Constructor(_, _) => match &other.kind {
                PatternKind::Id(_) | PatternKind::Ignore => Some(Ordering::Greater),
                PatternKind::TypeId(_)
                | PatternKind::Atom(_)
                | PatternKind::Str(_)
                | PatternKind::Num(_)
                | PatternKind::Nothing => Some(Ordering::Less),
                PatternKind::List(_) => todo!(),
                PatternKind::Spread(_) => todo!(),
                PatternKind::Strict { .. } => todo!(),
                PatternKind::Type(_, _) => todo!(),
                PatternKind::Constructor(_, _) => todo!(),
            },
        }
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            PatternKind::Id(s) | PatternKind::Num(s) | PatternKind::TypeId(s) => write!(f, "{}", s),
            PatternKind::Atom(s) => write!(f, ":{}", s),
            PatternKind::Str(s) => write!(f, "\"{}\"", s.replace('"', "\\\"")),
            PatternKind::Ignore => write!(f, "_"),
            PatternKind::Nothing => write!(f, "nothing"),
            PatternKind::List(pats) => Delimited("[", ", ", "]", pats).fmt(f),
            PatternKind::Spread(inner) => {
                write!(f, "{}", &inner)?;
                write!(f, "...")
            }
            PatternKind::Strict { inner, full } => {
                write!(f, "{{")?;
                if *full {
                    write!(f, "!")?;
                }
                write!(f, "{}", inner)?;
                write!(f, "}}")
            }
            PatternKind::Type(x, Spanned(_, ty)) => {
                if let Some(Spanned(_, x)) = x {
                    write!(f, "{}", x)?;
                }
                write!(f, "::")?;
                write!(f, "{}", ty)
            }
            PatternKind::Constructor(head, args) => {
                write!(f, "{}", head)?;
                Delimited("", " ", "", args).fmt(f)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn wtf() {
        let ignore = Pattern {
            span: Span::dummy(),
            kind: PatternKind::Ignore,
        };
        let num = Pattern {
            span: Span::dummy(),
            kind: PatternKind::Num("".to_string()),
        };

        assert_eq!(ignore.cmp_specificity(&num), Some(Ordering::Less));
    }
}
