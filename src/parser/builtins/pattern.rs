use std::{cmp::Ordering, sync::Arc};

use crate::{
    lexer::token::TokenType,
    parser::{
        ast::{Pattern, PatternKind},
        parselet::{PostfixPatternAction, PrefixPatternAction},
        prec::PrecLevel,
        result::{ParseError, ParseResult},
    },
    util::P,
};

use super::{
    AtomParselet, CallParselet, GroupParselet, IdParselet, ListParselet, NothingParselet,
    NumParselet, SpreadParselet, StrParselet, TypeIdParselet,
};

impl PrefixPatternAction for IdParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
        enclosed: bool,
    ) -> ParseResult<Pattern> {
        let id = parser.eat_expect_id()?;

        match parser.peek_expect_ty()? {
            TokenType::ColonColon => {
                let col = parser.eat()?;
                if enclosed {
                    let ty = parser.eat_expect_type_id()?;
                    return Ok(Pattern {
                        span: id.span().unite(ty.span()),
                        kind: PatternKind::Type(Some(id), ty),
                    });
                } else {
                    return Err(ParseError::AmbiguousUnenclosedPattern { span: col.span });
                }
            }
            _ => {}
        }

        Ok(Pattern {
            span: id.span(),
            kind: PatternKind::Id(id.take_item()),
        })
    }
}

impl PrefixPatternAction for TypeIdParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
        _enclosed: bool,
    ) -> ParseResult<Pattern> {
        let id = parser.eat_expect_type_id()?;

        Ok(Pattern {
            span: id.span(),
            kind: PatternKind::TypeId(id.take_item()),
        })
    }
}

impl PrefixPatternAction for AtomParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
        _enclosed: bool,
    ) -> ParseResult<Pattern> {
        let id = parser.eat_expect_atom()?;

        let s = if let TokenType::Atom(s) = id.ty {
            s
        } else {
            unreachable!("ICE: AtomParselet not called with Atom token");
        };

        Ok(Pattern {
            span: id.span,
            kind: PatternKind::Atom(s),
        })
    }
}

pub struct TypeParselet;
impl PrefixPatternAction for TypeParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
        _enclosed: bool,
    ) -> ParseResult<Pattern> {
        let tok = parser.eat()?;
        let ty = parser.eat_expect_type_id()?;
        Ok(Pattern {
            span: tok.span.unite(ty.span()),
            kind: PatternKind::Type(None, ty),
        })
    }
}

pub struct IgnoreParselet;
impl PrefixPatternAction for IgnoreParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
        enclosed: bool,
    ) -> ParseResult<Pattern> {
        let id = parser.eat()?; // Eat the underscore

        match parser.peek_expect_ty()? {
            TokenType::ColonColon => {
                let col = parser.eat()?;
                if enclosed {
                    let ty = parser.eat_expect_type_id()?;
                    return Ok(Pattern {
                        span: id.span.unite(ty.span()),
                        kind: PatternKind::Type(None, ty),
                    });
                } else {
                    return Err(ParseError::AmbiguousUnenclosedPattern { span: col.span });
                }
            }
            _ => {}
        }

        Ok(Pattern {
            span: id.span,
            kind: PatternKind::Ignore,
        })
    }
}

impl PrefixPatternAction for NothingParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
        _enclosed: bool,
    ) -> ParseResult<Pattern> {
        let tok = parser.eat()?;
        Ok(Pattern {
            span: tok.span,
            kind: PatternKind::Nothing,
        })
    }
}

impl PrefixPatternAction for StrParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
        _enclosed: bool,
    ) -> ParseResult<Pattern> {
        let tok = parser.eat()?;

        let s = if let TokenType::Str(s) = tok.ty {
            s
        } else {
            unreachable!("StrParselet given non-Str to parse");
        };

        Ok(Pattern {
            span: tok.span,
            kind: PatternKind::Str(s),
        })
    }
}

impl PrefixPatternAction for NumParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
        _enclosed: bool,
    ) -> ParseResult<Pattern> {
        let tok = parser.eat()?;

        let n = if let TokenType::Num(n) = tok.ty {
            n
        } else {
            unreachable!("NumParselet given non-Num to parse");
        };

        Ok(Pattern {
            span: tok.span,
            kind: PatternKind::Num(n),
        })
    }
}

impl PrefixPatternAction for ListParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
        _enclosed: bool,
    ) -> ParseResult<Pattern> {
        let lsq = parser.eat()?; // Eat the [

        let (pats, end) = parser.parse_delimited(
            |parser| parser.parse_pattern(true),
            TokenType::Comma,
            TokenType::RSquare,
        )?;

        if let Some((_, init)) = pats.split_last() {
            if let Some(spread) = init
                .iter()
                .find(|pat| matches!(pat.kind, PatternKind::Spread(_)))
            {
                return Err(ParseError::SpreadNotAtEnd { span: spread.span });
            }
        }

        Ok(Pattern {
            span: lsq.span.unite(end.span),
            kind: PatternKind::List(pats),
        })
    }
}

impl PrefixPatternAction for SpreadParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
        _enclosed: bool,
    ) -> ParseResult<Pattern> {
        let ellipsis = parser.eat()?;
        let tok = parser.eat()?;

        let kind = match tok.ty {
            TokenType::Id(x) => PatternKind::Id(x),
            TokenType::Underscore => PatternKind::Ignore,
            _ => return Err(ParseError::OnlyIdOrIgnore { span: tok.span }),
        };

        let inner = Pattern {
            kind,
            span: tok.span,
        };

        Ok(Pattern {
            span: ellipsis.span.unite(tok.span),
            kind: PatternKind::Spread(Arc::new(inner)),
        })
    }
}

impl PrefixPatternAction for GroupParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
        _enclosed: bool,
    ) -> ParseResult<Pattern> {
        parser.eat()?; // Eat the (
        let inner = parser.parse_pattern(true)?;
        parser.eat_expect(TokenType::RParen)?;
        Ok(inner)
    }
}

pub struct StrictParselet;
impl PrefixPatternAction for StrictParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
        _enclosed: bool,
    ) -> ParseResult<Pattern> {
        let lcurly = parser.eat()?; // Eat the {

        let full = match parser.peek_expect_ty()? {
            TokenType::Op(ref s) if s == "!" => {
                parser.eat()?;
                true
            }
            _ => false,
        };

        let inner = parser.parse_pattern(true)?;
        let rcurly = parser.eat_expect(TokenType::RCurly)?;

        Ok(Pattern {
            span: lcurly.span.unite(rcurly.span),
            kind: PatternKind::Strict {
                inner: Arc::new(inner),
                full,
            },
        })
    }
}

impl PostfixPatternAction for CallParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
        lhs: P<Pattern>,
        _enclosed: bool,
    ) -> ParseResult<Pattern> {
        let mut args = Vec::new();

        loop {
            args.push(parser.parse_pattern(false)?);
            println!("{:?}", parser.peek_ty()?);
            match parser.peek_cmp_precedence(&PrecLevel::Call)? {
                Some(Ordering::Greater) | Some(Ordering::Equal) => {}
                _ => break,
            }
        }

        let span = if let Some(arg) = args.last() {
            lhs.span.unite(arg.span)
        } else {
            lhs.span
        };

        Ok(Pattern {
            span,
            kind: PatternKind::Constructor(lhs, args),
        })
    }
}
