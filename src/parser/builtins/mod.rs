use std::{collections::HashMap, sync::Arc};

use super::parselet::{PatternAction, PostfixAction, PrefixAction};
use crate::lexer::token::{Token, TokenType};
use crate::parser::prec::Associativity;

pub mod expr;
pub mod pattern;

lazy_static! {
    static ref CALL_ACTION: Arc<dyn PostfixAction> = Arc::new(expr::CallParselet);
}

macro_rules! builtin_prefix_actions {
    ($(($name:expr, $prefix:expr $(,$postfix:expr)?$(,)?)),*) => {
        lazy_static! {
            pub static ref BUILTIN_PREFIX_ACTIONS: HashMap<Token, Arc<dyn PrefixAction>> = {
                let mut h = hashmap!{};
                $({
                    let tok = Token::new_dummy($name);
                    let act = Arc::new($prefix);
                    assert!(!h.contains_key(&tok));
                    h.insert(tok, act as Arc<dyn PrefixAction>);
                })*
                h
            };

            pub static ref BUILTIN_POSTFIX_ACTIONS_FROM_PREFIX: HashMap<Token, (Option<Associativity>, Arc<dyn PostfixAction>)> = {
                let mut h = hashmap!{};
                $({
                    let tok = Token::new_dummy($name);
                    #[allow(unused_mut)]
                    let mut act = CALL_ACTION.clone();
                    $(act = Arc::new($postfix) as Arc<dyn PostfixAction>;)*
                    assert!(!h.contains_key(&tok));
                    h.insert(tok, (None, act));
                })*
                h
            };
        }
    };
    ($(($name:expr, $prefix:path $(,$postfix:expr)?$(,)?),)*) => {
        builtin_prefix_actions! { $(($name, $prefix $(, $postfix)?)),* }
    };
}

macro_rules! builtin_postfix_actions {
    ($(($name:expr, $assoc:expr, $postfix:expr $(,)?)),*) => {
        lazy_static! {
            pub static ref BUILTIN_POSTFIX_ACTIONS: HashMap<Token, (Option<Associativity>, Arc<dyn PostfixAction>)> = {
                let mut h = BUILTIN_POSTFIX_ACTIONS_FROM_PREFIX.clone();
                $({
                    let tok = Token::new_dummy($name);
                    let act = Arc::new($postfix);
                    assert!(!h.contains_key(&tok));
                    h.insert(tok, ($assoc, act as Arc<dyn PostfixAction>));
                })*
                h
            };
        }
    };
    ($(($name:expr, $assoc:expr, $postfix:expr $(,)?),)*) => {
        builtin_postfix_actions! { $(($name, $assoc, $postfix)),* }
    };
}

macro_rules! builtin_pattern_actions {
    ($(($name:expr, $pattern:expr $(,)?)),*) => {
        lazy_static! {
            pub static ref BUILTIN_PATTERN_ACTIONS: HashMap<Token, Arc<dyn PatternAction>> = {
                let mut h = hashmap!{};
                $({
                    let tok = Token::new_dummy($name);
                    let act = Arc::new($pattern);
                    assert!(!h.contains_key(&tok));
                    h.insert(tok, act as Arc<dyn PatternAction>);
                })*
                h
            };
        }
    };
    ($(($name:expr, $pattern:expr $(,)?),)*) => {
        builtin_pattern_actions! { $(($name, $pattern)),* }
    };
}

pub struct NothingParselet;
pub struct NumParselet;
pub struct AtomParselet;
pub struct StrParselet;
pub struct ListParselet;
pub struct GroupParselet;
pub struct IdParselet;
pub struct SpreadParselet;

builtin_prefix_actions! {
    (TokenType::Nothing, NothingParselet),
    (TokenType::Num("".to_string()), NumParselet),
    (TokenType::Atom("".to_string()), AtomParselet),
    (TokenType::Str("".to_string()), StrParselet),
    (TokenType::InterpolateBegin("".to_string()), expr::InterpolatedStrParselet),
    (TokenType::LSquare, ListParselet),
    (TokenType::LParen, GroupParselet),
    (TokenType::LCurly, expr::BlockParselet),
    (TokenType::Id("".to_string()), IdParselet),
    (TokenType::Let, expr::LetParselet),
    (TokenType::Fn, expr::FnParselet),
    (TokenType::Backslash, expr::LambdaParselet),
    (TokenType::Ellipsis, SpreadParselet),
}

builtin_postfix_actions! {
    (TokenType::Eq, Some(Associativity::Right), expr::EqParselet),
}

builtin_pattern_actions! {
    (TokenType::Id("".to_string()), IdParselet),
    (TokenType::Underscore, pattern::IgnoreParselet),
    (TokenType::Nothing, NothingParselet),
    (TokenType::Num("".to_string()), NumParselet),
    (TokenType::Str("".to_string()), StrParselet),
    (TokenType::LSquare, ListParselet),
    (TokenType::LCurly, pattern::StrictParselet),
    (TokenType::LParen, GroupParselet),
    (TokenType::Ellipsis, SpreadParselet),
    (TokenType::ColonColon, pattern::TypeParselet),
}

lazy_static! {
    pub static ref TOP_OP: Token = Token::new_dummy(TokenType::TopOp);
    pub static ref PREFIX_OP: Token = Token::new_dummy(TokenType::PrefixOp);
    pub static ref POSTFIX_OP: Token = Token::new_dummy(TokenType::PostfixOp);
    pub static ref CALL_OP: Token = Token::new_dummy(TokenType::CallOp);
    pub static ref BOTTOM_OP: Token = Token::new_dummy(TokenType::BottomOp);
}
