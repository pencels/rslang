use std::{collections::HashMap, sync::Arc};

use codespan_reporting::files::SimpleFiles;

use crate::{
    lexer::{token::TokenKind, OperatorTrie},
    parser::{
        builtins,
        parselet::{PostfixAction, PostfixPatternAction, PrefixAction, PrefixPatternAction},
        prec::{Associativity, Poset, PrecLevel},
    },
    runtime::{Env, Runtime},
    util::{Id, Span},
};

pub type PrefixActionTable<T> = HashMap<TokenKind, (Option<Span>, Arc<T>)>;
pub type PostfixActionTable<T> = HashMap<TokenKind, (Option<Span>, Option<Associativity>, Arc<T>)>;

pub struct SlangContext {
    pub runtime: Runtime,
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

        let mut trie = OperatorTrie::new();

        trie.insert_operator("<:");

        SlangContext {
            runtime: Runtime::new(),
            trie,
            poset,
            prefix_actions: builtins::BUILTIN_PREFIX_ACTIONS.clone(),
            postfix_actions: builtins::BUILTIN_POSTFIX_ACTIONS.clone(),
            prefix_pattern_actions: builtins::BUILTIN_PREFIX_PATTERN_ACTIONS.clone(),
            postfix_pattern_actions: builtins::BUILTIN_POSTFIX_PATTERN_ACTIONS.clone(),
            files: SimpleFiles::new(),
        }
    }
}
