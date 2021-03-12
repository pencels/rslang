#[derive(Debug)]
pub enum Token {
    Nothing,
    Num(f64),
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
    Type,

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
