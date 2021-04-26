use crate::{parser::prec::PrecLevel, util::Span};

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    Nothing,
    Num(String),
    Str(String),
    Atom(String),

    Id(String),
    TypeId(String),

    Op(String),

    InterpolateBegin(String),
    InterpolateContinue(String),
    InterpolateEnd(String),

    Let,
    When,
    Operator,
    Fn,
    Struct,

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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TokenKind {
    Nothing,
    Num,
    Str,
    Atom,

    Id,
    TypeId,

    Op(String),

    InterpolateBegin,
    InterpolateContinue,
    InterpolateEnd,

    Let,
    When,
    Operator,
    Fn,
    Struct,

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
    pub fn prec(&self) -> PrecLevel {
        match self {
            TokenType::Nothing
            | TokenType::Num(_)
            | TokenType::Str(_)
            | TokenType::Atom(_)
            | TokenType::Id(_)
            | TokenType::TypeId(_)
            | TokenType::LParen
            | TokenType::LSquare
            | TokenType::LCurly
            | TokenType::Underscore
            | TokenType::InterpolateBegin(_) => PrecLevel::Call,
            TokenType::Op(op) => PrecLevel::Infix(op.clone()),
            TokenType::Ellipsis => PrecLevel::Prefix,
            _ => PrecLevel::Bottom,
        }
    }

    pub fn kind(&self) -> TokenKind {
        use TokenKind::*;
        match self {
            TokenType::Nothing => Nothing,
            TokenType::Num(_) => Num,
            TokenType::Str(_) => Str,
            TokenType::Atom(_) => Atom,
            TokenType::Id(_) => Id,
            TokenType::TypeId(_) => TypeId,
            TokenType::Op(s) => Op(s.clone()),
            TokenType::InterpolateBegin(_) => InterpolateBegin,
            TokenType::InterpolateContinue(_) => InterpolateContinue,
            TokenType::InterpolateEnd(_) => InterpolateEnd,
            TokenType::Let => Let,
            TokenType::When => When,
            TokenType::Operator => Operator,
            TokenType::Fn => Fn,
            TokenType::Struct => Struct,
            TokenType::LParen => LParen,
            TokenType::RParen => RParen,
            TokenType::LSquare => LSquare,
            TokenType::RSquare => RSquare,
            TokenType::LCurly => LCurly,
            TokenType::RCurly => RCurly,
            TokenType::Underscore => Underscore,
            TokenType::ColonColon => ColonColon,
            TokenType::Backslash => Backslash,
            TokenType::Arrow => Arrow,
            TokenType::Ellipsis => Ellipsis,
            TokenType::Eq => Eq,
            TokenType::Comma => Comma,
            TokenType::Semicolon => Semicolon,
            TokenType::Newline => Newline,
            TokenType::Eof => Eof,
        }
    }
}
