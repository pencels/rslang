use std::sync::Arc;

use crate::{
    lexer::token::TokenType,
    parser::{
        ast::{Pattern, PatternKind},
        parselet::PatternAction,
        result::ParseResult,
    },
};

use super::{
    GroupParselet, IdParselet, ListParselet, NothingParselet, NumParselet, SpreadParselet,
    StrParselet,
};

impl PatternAction for IdParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Pattern> {
        let tok = parser.eat()?; // Blindly eat the identifier

        let id = parser.eat_expect_id()?;

        match parser.peek_expect_ty()? {
            TokenType::ColonColon => {
                let col = parser.eat()?;
                let ty = parser.eat_expect_type_id()?;
                return Ok(Pattern {
                    span: col.span.unite(ty.span),
                    kind: PatternKind::Type(Some(id), ty),
                });
            }
            _ => {}
        }

        let id = if let TokenType::Id(id) = id.ty {
            id
        } else {
            unreachable!("ICE: IdParselet not called with Id token")
        };

        Ok(Pattern {
            span: tok.span,
            kind: PatternKind::Id(id),
        })
    }
}

pub struct TypeParselet;
impl PatternAction for TypeParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Pattern> {
        let tok = parser.eat()?;
        let ty = parser.eat_expect_type_id()?;
        Ok(Pattern {
            span: tok.span.unite(ty.span),
            kind: PatternKind::Type(None, ty),
        })
    }
}

pub struct IgnoreParselet;
impl PatternAction for IgnoreParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Pattern> {
        let tok = parser.eat()?; // Eat the underscore

        Ok(Pattern {
            span: tok.span,
            kind: PatternKind::Ignore,
        })
    }
}

impl PatternAction for NothingParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Pattern> {
        let tok = parser.eat()?;
        Ok(Pattern {
            span: tok.span,
            kind: PatternKind::Nothing,
        })
    }
}

impl PatternAction for StrParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
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

impl PatternAction for NumParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
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

impl PatternAction for ListParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Pattern> {
        let lsq = parser.eat()?; // Eat the [

        let (pats, end) = parser.parse_delimited(
            |parser| parser.parse_pattern(),
            TokenType::Comma,
            TokenType::RSquare,
        )?;

        Ok(Pattern {
            span: lsq.span.unite(end.span),
            kind: PatternKind::List(pats),
        })
    }
}

impl PatternAction for SpreadParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Pattern> {
        let ellipsis = parser.eat()?;
        let id = parser.eat_expect_id()?;
        Ok(Pattern {
            span: ellipsis.span.unite(id.span),
            kind: PatternKind::Spread(id),
        })
    }
}

impl PatternAction for GroupParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Pattern> {
        parser.eat()?; // Eat the (
        let inner = parser.parse_pattern()?;
        parser.eat_expect(TokenType::RParen)?;
        Ok(inner)
    }
}

pub struct StrictParselet;
impl PatternAction for StrictParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Pattern> {
        let lcurly = parser.eat()?; // Eat the {

        let full = match parser.peek_expect_ty()? {
            TokenType::Op(ref s) if s == "!" => {
                parser.eat()?;
                true
            }
            _ => false,
        };

        let inner = parser.parse_pattern()?;
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

pub struct ConstructorParselet;
impl PatternAction for ConstructorParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Pattern> {
        todo!()
    }
}
