use std::{cmp::Ordering, sync::Arc};

use crate::{
    lexer::token::{Token, TokenType},
    parser::{
        ast::{Expr, ExprKind, MatchboxRow, Pattern, PatternKind, StringPart},
        parselet::{PostfixAction, PrefixAction},
        prec::PrecLevel,
        result::{ParseError, ParseResult},
        Parser,
    },
    util::P,
};

use super::{
    AtomParselet, CallParselet, GroupParselet, IdParselet, ListParselet, NothingParselet,
    NumParselet, SpreadParselet, StrParselet, TypeIdParselet,
};

impl PrefixAction for NothingParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Expr> {
        let tok = parser.eat_expect(TokenType::Nothing)?;
        Ok(Expr {
            span: tok.span,
            kind: ExprKind::Nothing,
        })
    }
}

impl PrefixAction for NumParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Expr> {
        let tok = parser.eat()?; // Eat num token

        if let TokenType::Num(n) = tok.ty {
            Ok(Expr {
                span: tok.span,
                kind: ExprKind::Num(n),
            })
        } else {
            unreachable!("ICE: NumParselet given non-Num token to parse");
        }
    }
}

impl PrefixAction for AtomParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Expr> {
        let tok = parser.eat()?; // Eat atom token

        if let TokenType::Atom(name) = tok.ty {
            Ok(Expr {
                span: tok.span,
                kind: ExprKind::Atom(name),
            })
        } else {
            unreachable!("ICE: AtomParselet given non-Atom token to parse");
        }
    }
}

impl PrefixAction for StrParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Expr> {
        let tok = parser.eat()?;

        if let TokenType::Str(s) = tok.ty {
            Ok(Expr {
                span: tok.span,
                kind: ExprKind::Str(s),
            })
        } else {
            unreachable!("ICE: StrParselet given non-Str token to parse");
        }
    }
}

pub struct InterpolatedStrParselet;
impl PrefixAction for InterpolatedStrParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Expr> {
        let mut components = Vec::new();

        let tok = parser.eat()?;
        if let TokenType::InterpolateBegin(s) = tok.ty {
            components.push(StringPart::Str(tok.span, s));
        }

        loop {
            components.push(StringPart::Expr(parser.parse_expression()?));
            let tok = parser.eat()?;
            match tok.ty {
                TokenType::InterpolateContinue(s) => {
                    components.push(StringPart::Str(tok.span, s));
                }
                TokenType::InterpolateEnd(s) => {
                    components.push(StringPart::Str(tok.span, s));
                    break;
                }
                _ => {}
            }
        }

        let span = if let Some(comp) = components.last() {
            let end_span = match comp {
                StringPart::Expr(e) => e.span,
                StringPart::Str(span, _) => *span,
            };
            tok.span.unite(end_span)
        } else {
            tok.span
        };

        Ok(Expr {
            span,
            kind: ExprKind::InterpolatedStr(components),
        })
    }
}

impl PrefixAction for ListParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Expr> {
        let lsq = parser.eat()?; // Eat the [

        let (exprs, end) = parser.parse_delimited(
            |parser| parser.parse_expression(),
            TokenType::Comma,
            TokenType::RSquare,
        )?;

        Ok(Expr {
            span: lsq.span.unite(end.span),
            kind: ExprKind::List(exprs),
        })
    }
}

impl PrefixAction for SpreadParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Expr> {
        let ellipsis = parser.eat()?;
        let id = parser.eat_expect_id()?;
        Ok(Expr {
            span: ellipsis.span.unite(id.span),
            kind: ExprKind::Spread(id),
        })
    }
}

impl PrefixAction for IdParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Expr> {
        let tok = parser.eat()?;
        if let TokenType::Id(n) = tok.ty {
            Ok(Expr {
                span: tok.span,
                kind: ExprKind::Id(n),
            })
        } else {
            unreachable!("ICE: IdParselet given non-Id token to parse");
        }
    }
}

impl PrefixAction for TypeIdParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Expr> {
        let tok = parser.eat()?;
        if let TokenType::TypeId(n) = tok.ty {
            Ok(Expr {
                span: tok.span,
                kind: ExprKind::TypeId(n),
            })
        } else {
            unreachable!("ICE: TypeIdParselet given non-TypeId token to parse");
        }
    }
}

pub struct LetParselet;
impl PrefixAction for LetParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Expr> {
        parser.eat_expect(TokenType::Let)?;

        let pat = parser.parse_pattern(true)?;
        parser.eat_expect(TokenType::Eq)?;
        parser.skip_newlines()?; // Allow newlines after '='.
        let expr = parser.parse_expression()?;

        Ok(Expr {
            span: pat.span.unite(expr.span),
            kind: ExprKind::Let(pat, Arc::new(expr)),
        })
    }
}

pub struct EqParselet;
impl PostfixAction for EqParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
        lhs: P<Expr>,
    ) -> ParseResult<Expr> {
        parser.eat_expect(TokenType::Eq)?;

        let rhs = parser.parse_expression()?;

        Ok(Expr {
            span: lhs.span.unite(rhs.span),
            kind: ExprKind::Assign(lhs, Arc::new(rhs)),
        })
    }
}

pub struct PrefixOpParselet;
impl PrefixAction for PrefixOpParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Expr> {
        let tok = parser.eat()?; // Blindly eat the operator
        let expr = parser.parse_expr(&PrecLevel::Prefix)?;

        Ok(Expr {
            span: tok.span.unite(expr.span),
            kind: ExprKind::PrefixOp(tok, Arc::new(expr)),
        })
    }
}

pub struct BinOpParselet;
impl PostfixAction for BinOpParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
        lhs: P<Expr>,
    ) -> ParseResult<Expr> {
        let tok = parser.eat()?; // Blindly eat the operator
        let rhs = parser.parse_expr(&tok.ty.prec())?;

        Ok(Expr {
            span: lhs.span.unite(rhs.span),
            kind: ExprKind::BinOp(tok, lhs, Arc::new(rhs)),
        })
    }
}

pub struct PostfixOpParselet;
impl PostfixAction for PostfixOpParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
        lhs: P<Expr>,
    ) -> ParseResult<Expr> {
        let tok = parser.eat()?; // Blindly eat the operator
        Ok(Expr {
            span: lhs.span.unite(tok.span),
            kind: ExprKind::PostfixOp(tok, lhs),
        })
    }
}

impl PostfixAction for CallParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
        lhs: P<Expr>,
    ) -> ParseResult<Expr> {
        let mut args = Vec::new();

        loop {
            args.push(parser.parse_expr(&PrecLevel::Call)?);
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

        Ok(Expr {
            span,
            kind: ExprKind::Call(lhs, args),
        })
    }
}

impl PrefixAction for GroupParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Expr> {
        let tok = parser.eat()?;
        parser.skip_newlines()?;

        let (exprs, end) = parser.parse_delimited(
            |parser| parser.parse_expression(),
            TokenType::Semicolon,
            TokenType::RParen,
        )?;

        Ok(Expr {
            span: tok.span.unite(end.span),
            kind: ExprKind::Group(exprs),
        })
    }
}

pub struct FnParselet;
impl FnParselet {
    fn parse_parenthesized_param(&self, parser: &mut Parser) -> ParseResult<Pattern> {
        parser.eat()?;
        let param = parser.parse_pattern(true)?;
        parser.eat_expect(TokenType::RParen)?;
        Ok(param)
    }

    fn parse_param(&self, parser: &mut Parser) -> ParseResult<Pattern> {
        match parser.peek_ty()? {
            Some(TokenType::LParen) => self.parse_parenthesized_param(parser),
            _ => parser.parse_pattern(false),
        }
    }
}

impl PrefixAction for FnParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Expr> {
        let f = parser.eat()?;
        let mut params = Vec::new();

        parser.skip_newlines()?; // Allow newlines after 'fn'
        let (name, name_span, min, max) = match parser.peek_ty()? {
            // fn (...) ...
            Some(TokenType::LParen) => {
                params.push(self.parse_parenthesized_param(parser)?);
                parser.skip_newlines()?;
                let tok = parser.eat()?;
                match tok.ty {
                    // fn (...) + ...
                    TokenType::Op(name) => (name, tok.span, 1, Some(2)),
                    // fn (...) :foo ...
                    TokenType::Atom(name) => (name, tok.span, 1, None),
                    _ => unreachable!(),
                }
            }
            // fn foo ...
            Some(TokenType::Id(_)) => {
                let id = parser.eat()?;
                parser.skip_newlines()?; // Allow newlines between fn name and params
                match parser.peek_ty()? {
                    Some(TokenType::Op(name)) => {
                        let tok = parser.eat()?;
                        let param = match id.ty {
                            TokenType::Id(name) => name,
                            _ => unreachable!(),
                        };
                        params.push(Pattern {
                            span: id.span,
                            kind: PatternKind::Id(param),
                        });
                        (name, tok.span, 1, Some(2))
                    }
                    Some(TokenType::Atom(name)) => {
                        let tok = parser.eat()?;
                        let param = match id.ty {
                            TokenType::Id(name) => name,
                            _ => unreachable!(),
                        };
                        params.push(Pattern {
                            span: id.span,
                            kind: PatternKind::Id(param),
                        });
                        (name, tok.span, 1, None)
                    }
                    _ => match id.ty {
                        TokenType::Id(name) => (name, id.span, 0, None),
                        _ => unreachable!(),
                    },
                }
            }
            // fn + ...
            Some(TokenType::Op(name)) => {
                let tok = parser.eat()?;
                (name, tok.span, 1, Some(1))
            }
            _ => {
                let tok = parser.eat()?;
                return Err(ParseError::Expected {
                    span: tok.span,
                    expected: "fn name or parameter".to_string(),
                    got: tok,
                });
            }
        };

        // Parse the rest of the parameters.
        loop {
            parser.skip_newlines()?;
            if parser.check(&TokenType::Eq)? {
                break;
            }
            params.push(self.parse_param(parser)?);
        }
        parser.eat()?; // Eat the =
        parser.skip_newlines()?;

        let num_params = params.len();
        if num_params < min || max.map_or(false, |max| num_params > max) {
            let last_param_span = params.last().map_or(name_span, |last| last.span);
            return Err(ParseError::InvalidNumberOfFnParams {
                name_span,
                last_param_span,
                min,
                max,
                given: num_params,
            });
        }

        let result = parser.parse_expression()?;

        Ok(Expr {
            span: f.span.unite(result.span),
            kind: ExprKind::Fn(name, name_span, params, Arc::new(result)),
        })
    }
}

pub struct LambdaParselet;
impl PrefixAction for LambdaParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Expr> {
        let tok = parser.eat()?; // Eat the '\'

        let mut params = Vec::new();
        params.push(parser.parse_pattern(false)?);

        loop {
            parser.skip_newlines()?;
            if parser.check(&TokenType::Arrow)? {
                break;
            }
            params.push(parser.parse_pattern(false)?);
        }
        parser.eat()?;
        parser.skip_newlines()?;

        let result = parser.parse_expression()?;

        Ok(Expr {
            span: tok.span.unite(result.span),
            kind: ExprKind::Lambda(params, Arc::new(result)),
        })
    }
}

pub struct BlockParselet;
impl BlockParselet {
    fn parse_matchbox_row(&self, parser: &mut Parser) -> ParseResult<MatchboxRow> {
        let when = parser.eat_expect(TokenType::When)?;

        let mut params = Vec::new();
        loop {
            parser.skip_newlines()?;
            match parser.peek_expect_ty()? {
                TokenType::Arrow => break,
                TokenType::Op(ref op) if op == "|" => break,
                _ => {}
            }
            params.push(parser.parse_pattern(false)?);
        }

        let sep = parser.eat()?;

        let guard = match sep.ty {
            TokenType::Arrow => None,
            _ => {
                parser.skip_newlines()?;
                let guard = parser.parse_expression()?;
                parser.skip_newlines()?;
                parser.eat_expect(TokenType::Arrow)?;
                Some(Arc::new(guard))
            }
        };
        parser.skip_newlines()?;

        let result = Arc::new(parser.parse_expression()?);

        Ok(MatchboxRow {
            span: when.span.unite(result.span),
            params,
            guard,
            result,
        })
    }
}

impl PrefixAction for BlockParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Expr> {
        let curly = parser.eat()?;

        parser.skip_newlines()?;
        match parser.peek_ty()? {
            Some(TokenType::When) => {
                let (rows, end) = parser.parse_delimited(
                    |parser| self.parse_matchbox_row(parser),
                    TokenType::Comma,
                    TokenType::RCurly,
                )?;

                Ok(Expr {
                    span: curly.span.unite(end.span),
                    kind: ExprKind::Matchbox(rows),
                })
            }
            _ => {
                let (exprs, end) = parser.parse_delimited(
                    |parser| parser.parse_expression(),
                    TokenType::Semicolon,
                    TokenType::RCurly,
                )?;

                Ok(Expr {
                    span: curly.span.unite(end.span),
                    kind: ExprKind::Lazy(exprs),
                })
            }
        }
    }
}
