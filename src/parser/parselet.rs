use crate::{
    parser::{ast::*, Parser},
    util::P,
};

use super::result::ParseResult;

pub trait PrefixAction: Send + Sync + 'static {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Expr>;
}

pub trait PostfixAction: Send + Sync + 'static {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
        lhs: P<Expr>,
    ) -> ParseResult<Expr>;
}

pub trait PatternAction: Send + Sync + 'static {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Pattern>;
}
pub trait DefnAction: Send + Sync + 'static {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Defn>;
}

/*
pub trait TyAction: Send + Sync + 'static {
    fn parse<'ctx, 'file>(&self, parser: &mut Parser<'ctx, 'file>) -> CResult<Id<PTy>>;
}
*/
