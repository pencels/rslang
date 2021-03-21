use crate::{
    lexer::token::TokenType,
    parser::{
        ast::{Pattern, PatternKind},
        parselet::PatternAction,
        result::ParseResult,
    },
};

pub struct IdParselet;
impl PatternAction for IdParselet {
    fn parse<'file, 'trie, 'prec>(
        &self,
        parser: &mut crate::parser::Parser<'file, 'trie, 'prec>,
    ) -> ParseResult<Pattern> {
        let tok = parser.eat()?; // Blindly eat the identifier

        let id = if let TokenType::Id(id) = tok.ty {
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
