use crate::util::Span;
use std::hash::Hash;

#[derive(Debug, Clone, Eq)]
pub struct Token {
    pub span: Span,
    pub ty: TokenType,
}

impl Token {
    pub fn new(span: Span, ty: TokenType) -> Token {
        Token { span, ty }
    }

    pub fn new_dummy(ty: TokenType) -> Token {
        Token {
            span: Span::dummy(),
            ty,
        }
    }
}

impl Hash for Token {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ty.repr().hash(state)
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.ty.repr().eq(other.ty.repr())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenType {
    Nothing,
    Num(String),
    Str(String),
    Atom(String),

    Id(String),
    TypeId(String),

    Op(String),
    BottomOp,
    CallOp,
    PrefixOp,
    PostfixOp,
    TopOp,

    InterpolateBegin(String),
    InterpolateContinue(String),
    InterpolateEnd(String),

    Let,
    When,
    Operator,
    Fn,
    Struct, // TODO: this is for basic tagged structural data so that we have Cons and Nil, etc - replace with Type later

    LParen,
    RParen,
    LSquare,
    RSquare,
    LCurly,
    RCurly,

    Underscore,
    ColonColon,
    Backslash,
    Arrow,
    Ellipsis,

    Eq,

    Comma,
    Semicolon,
    Newline,
    Eof,
}

impl TokenType {
    pub fn repr(&self) -> &str {
        match self {
            TokenType::Nothing => "`Nothing",
            TokenType::Num(_) => "`Num",
            TokenType::Str(_) => "`Str",
            TokenType::Atom(_) => "`Atom",
            TokenType::Id(_) => "`Id",
            TokenType::TypeId(_) => "`TypeId",
            TokenType::Op(op) => op,
            TokenType::InterpolateBegin(_) => "`InterpolateBegin",
            TokenType::InterpolateContinue(_) => "`InterpolateContinue",
            TokenType::InterpolateEnd(_) => "`InterpolateEnd",
            TokenType::Let => "`Let",
            TokenType::When => "`When",
            TokenType::Operator => "`Operator",
            TokenType::Fn => "`Fn",
            TokenType::Struct => "`Struct",
            TokenType::LParen => "`LParen",
            TokenType::RParen => "`RParen",
            TokenType::LSquare => "`LSquare",
            TokenType::RSquare => "`RSquare",
            TokenType::LCurly => "`LCurly",
            TokenType::RCurly => "`RCurly",
            TokenType::Underscore => "`Underscore",
            TokenType::ColonColon => "::",
            TokenType::Backslash => "`Backslash",
            TokenType::Arrow => "->",
            TokenType::Ellipsis => "...",
            TokenType::Eq => "=",
            TokenType::Comma => "`Comma",
            TokenType::Semicolon => "`Semicolon",
            TokenType::Newline => "`Newline",
            TokenType::Eof => "`Eof",
            TokenType::BottomOp => "`Bottom",
            TokenType::PrefixOp => "`Prefix",
            TokenType::PostfixOp => "`Postfix",
            TokenType::TopOp => "`Top",
            TokenType::CallOp => "`Call",
        }
    }
}
