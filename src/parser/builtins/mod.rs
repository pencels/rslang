use std::{collections::HashMap, sync::Arc};

use super::parselet::{PostfixAction, PostfixPatternAction, PrefixAction, PrefixPatternAction};
use crate::ctx::{PostfixActionTable, PrefixActionTable};
use crate::lexer::token::TokenKind;
use crate::parser::prec::Associativity;

pub mod expr;
pub mod pattern;

lazy_static! {
    static ref CALL_ACTION: Arc<dyn PostfixAction> = Arc::new(CallParselet);
    static ref CALL_PATTERN_ACTION: Arc<dyn PostfixPatternAction> = Arc::new(CallParselet);
}

macro_rules! builtin_prefix_actions {
    ($(($name:expr, $prefix:expr $(,$postfix:expr)?$(,)?)),*) => {
        lazy_static! {
            pub static ref BUILTIN_PREFIX_ACTIONS: PrefixActionTable<dyn PrefixAction> = {
                let mut h = hashmap!{};
                $({
                    let ty = $name;
                    let act = Arc::new($prefix);
                    assert!(!h.contains_key(&ty));
                    h.insert(ty, (None, act as Arc<dyn PrefixAction>));
                })*
                h
            };

            pub static ref BUILTIN_POSTFIX_ACTIONS_FROM_PREFIX: PostfixActionTable<dyn PostfixAction> = {
                let mut h = hashmap!{};
                $({
                    let ty = $name;
                    #[allow(unused_mut)]
                    let mut act = CALL_ACTION.clone();
                    $(act = Arc::new($postfix) as Arc<dyn PostfixAction>;)*
                    assert!(!h.contains_key(&ty));
                    h.insert(ty, (None, Some(Associativity::Left), act));
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
            pub static ref BUILTIN_POSTFIX_ACTIONS: PostfixActionTable<dyn PostfixAction> = {
                let mut h = BUILTIN_POSTFIX_ACTIONS_FROM_PREFIX.clone();
                $({
                    let ty = $name;
                    let act = Arc::new($postfix);
                    assert!(!h.contains_key(&ty));
                    h.insert(ty, (None, $assoc, act as Arc<dyn PostfixAction>));
                })*
                h
            };
        }
    };
    ($(($name:expr, $assoc:expr, $postfix:expr $(,)?),)*) => {
        builtin_postfix_actions! { $(($name, $assoc, $postfix)),* }
    };
}

macro_rules! builtin_prefix_pattern_actions {
    ($(($name:expr, $prefix:expr $(,$postfix:expr)?$(,)?)),*) => {
        lazy_static! {
            pub static ref BUILTIN_PREFIX_PATTERN_ACTIONS: PrefixActionTable<dyn PrefixPatternAction> = {
                let mut h = hashmap!{};
                $({
                    let ty = $name;
                    let act = Arc::new($prefix);
                    assert!(!h.contains_key(&ty));
                    h.insert(ty, (None, act as Arc<dyn PrefixPatternAction>));
                })*
                h
            };

            pub static ref BUILTIN_POSTFIX_PATTERN_ACTIONS_FROM_PREFIX: PostfixActionTable<dyn PostfixPatternAction> = {
                let mut h = hashmap!{};
                $({
                    let ty = $name;
                    #[allow(unused_mut)]
                    let mut act = CALL_PATTERN_ACTION.clone();
                    $(act = Arc::new($postfix) as Arc<dyn PostfixPatternAction>;)*
                    assert!(!h.contains_key(&ty));
                    h.insert(ty, (None, None, act));
                })*
                h
            };
        }
    };
    ($(($name:expr, $prefix:path $(,$postfix:expr)?$(,)?),)*) => {
        builtin_prefix_pattern_actions! { $(($name, $prefix $(, $postfix)?)),* }
    };
}

macro_rules! builtin_postfix_pattern_actions {
    ($(($name:expr, $assoc:expr, $postfix:expr $(,)?)),*) => {
        lazy_static! {
            pub static ref BUILTIN_POSTFIX_PATTERN_ACTIONS: PostfixActionTable<dyn PostfixPatternAction> = {
                let mut h = BUILTIN_POSTFIX_PATTERN_ACTIONS_FROM_PREFIX.clone();
                $({
                    let ty = $name;
                    let act = Arc::new($postfix);
                    assert!(!h.contains_key(&ty));
                    h.insert(ty, (None, $assoc, act as Arc<dyn PostfixPatternAction>));
                })*
                h
            };
        }
    };
    ($(($name:expr, $assoc:expr, $postfix:expr $(,)?),)*) => {
        builtin_postfix_pattern_actions! { $(($name, $assoc, $postfix)),* }
    };
}

pub struct NothingParselet;
pub struct NumParselet;
pub struct AtomParselet;
pub struct StrParselet;
pub struct ListParselet;
pub struct GroupParselet;
pub struct IdParselet;
pub struct TypeIdParselet;
pub struct SpreadParselet;
pub struct CallParselet;

builtin_prefix_actions! {
    (TokenKind::Nothing, NothingParselet),
    (TokenKind::Num, NumParselet),
    (TokenKind::Atom, AtomParselet),
    (TokenKind::Str, StrParselet),
    (TokenKind::InterpolateBegin, expr::InterpolatedStrParselet),
    (TokenKind::LSquare, ListParselet),
    (TokenKind::LParen, GroupParselet),
    (TokenKind::LCurly, expr::BlockParselet),
    (TokenKind::Id, IdParselet),
    (TokenKind::TypeId, TypeIdParselet),
    (TokenKind::Let, expr::LetParselet),
    (TokenKind::Fn, expr::FnParselet),
    (TokenKind::Backslash, expr::LambdaParselet),
    (TokenKind::Ellipsis, SpreadParselet),
}

builtin_postfix_actions! {
    (TokenKind::Eq, Some(Associativity::Right), expr::EqParselet),
}

builtin_prefix_pattern_actions! {
    (TokenKind::Id, IdParselet),
    (TokenKind::TypeId, TypeIdParselet),
    (TokenKind::Underscore, pattern::IgnoreParselet),
    (TokenKind::Nothing, NothingParselet),
    (TokenKind::Num, NumParselet),
    (TokenKind::Str, StrParselet),
    (TokenKind::LSquare, ListParselet),
    (TokenKind::LCurly, pattern::StrictParselet),
    (TokenKind::LParen, GroupParselet),
    (TokenKind::Ellipsis, SpreadParselet),
    (TokenKind::ColonColon, pattern::TypeParselet),
}

builtin_postfix_pattern_actions! {}
