use std::{collections::HashMap, sync::Arc};

use codespan_reporting::files::SimpleFiles;

use crate::{
    lexer::{token::TokenKind, OperatorTrie},
    parser::{
        builtins,
        parselet::{PostfixAction, PostfixPatternAction, PrefixAction, PrefixPatternAction},
        prec::{Associativity, Poset, PrecLevel},
    },
    util::Span,
};

pub type PrefixActionTable<T> = HashMap<TokenKind, (Option<Span>, Arc<T>)>;
pub type PostfixActionTable<T> = HashMap<TokenKind, (Option<Span>, Option<Associativity>, Arc<T>)>;

pub struct SlangContext {
    pub trie: OperatorTrie,
    pub poset: Poset<PrecLevel>,
    pub prefix_actions: PrefixActionTable<dyn PrefixAction>,
    pub postfix_actions: PostfixActionTable<dyn PostfixAction>,
    pub prefix_pattern_actions: PrefixActionTable<dyn PrefixPatternAction>,
    pub postfix_pattern_actions: PostfixActionTable<dyn PostfixPatternAction>,
    pub files: SimpleFiles<String, String>,
}

impl SlangContext {
    pub fn new() -> SlangContext {
        let mut poset = Poset::new();

        // Add in control operators.
        poset
            .try_add_lt(PrecLevel::Bottom, PrecLevel::Call)
            .unwrap();
        poset
            .try_add_lt(PrecLevel::Call, PrecLevel::Prefix)
            .unwrap();
        poset
            .try_add_lt(PrecLevel::Prefix, PrecLevel::Postfix)
            .unwrap();

        SlangContext {
            trie: OperatorTrie::new(),
            poset,
            prefix_actions: builtins::BUILTIN_PREFIX_ACTIONS.clone(),
            postfix_actions: builtins::BUILTIN_POSTFIX_ACTIONS.clone(),
            prefix_pattern_actions: builtins::BUILTIN_PREFIX_PATTERN_ACTIONS.clone(),
            postfix_pattern_actions: builtins::BUILTIN_POSTFIX_PATTERN_ACTIONS.clone(),
            files: SimpleFiles::new(),
        }
    }
}
