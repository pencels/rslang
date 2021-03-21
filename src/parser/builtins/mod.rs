use std::{collections::HashMap, sync::Arc};

use super::parselet::{PatternAction, PostfixAction, PrefixAction};
use crate::lexer::token::{Token, TokenType};
use crate::parser::Associativity;

pub mod expr;
pub mod pattern;

lazy_static! {
    pub static ref BUILTIN_PREFIX_ACTIONS: HashMap<Token, Arc<dyn PrefixAction>> = {
        let mut actions = HashMap::new();

        actions.insert(
            Token::new_dummy(TokenType::Let),
            Arc::new(expr::LetParselet) as Arc<dyn PrefixAction>,
        );
        actions.insert(
            Token::new_dummy(TokenType::Num("".to_string())),
            Arc::new(expr::NumParselet) as Arc<dyn PrefixAction>,
        );

        actions
    };
    pub static ref BUILTIN_POSTFIX_ACTIONS: HashMap<Token, (Option<Associativity>, Arc<dyn PostfixAction>)> =
        HashMap::new();
    pub static ref BUILTIN_PATTERN_ACTIONS: HashMap<Token, Arc<dyn PatternAction>> = {
        let mut actions = HashMap::new();

        actions.insert(
            Token::new_dummy(TokenType::Id("".to_string())),
            Arc::new(pattern::IdParselet) as Arc<dyn PatternAction>,
        );

        actions
    };
    pub static ref TOP_OP: Token = Token::new_dummy(TokenType::TopOp);
    pub static ref PREFIX_OP: Token = Token::new_dummy(TokenType::PrefixOp);
    pub static ref POSTFIX_OP: Token = Token::new_dummy(TokenType::PostfixOp);
    pub static ref BOTTOM_OP: Token = Token::new_dummy(TokenType::BottomOp);
}
