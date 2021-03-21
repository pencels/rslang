use std::sync::Arc;

use crate::{
    lexer::token::TokenType,
    parser::{
        ast::{Expr, ExprKind},
        parselet::{PostfixAction, PrefixAction},
        result::ParseResult,
        Parser,
    },
    util::P,
};

use super::PREFIX_OP;

pub struct LetParselet;
impl PrefixAction for LetParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Expr> {
        parser.eat()?; // Blindly eat 'let'

        let pat = parser.parse_pattern()?;
        parser.eat_expect(TokenType::Eq)?;
        let expr = parser.parse_expression()?;

        Ok(Expr {
            span: pat.span.unite(expr.span),
            kind: ExprKind::Let(pat, Arc::new(expr)),
        })
    }
}

pub struct NumParselet;
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

pub struct PrefixOpParselet;
impl PrefixAction for PrefixOpParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Expr> {
        let tok = parser.eat()?; // Blindly eat the operator
        let expr = parser.parse_expr(&PREFIX_OP)?;

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
        let rhs = parser.parse_expr(&tok)?;

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
