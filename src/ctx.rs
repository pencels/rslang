use std::{collections::HashMap, sync::Arc};

use codespan_reporting::files::SimpleFiles;

use crate::{
    lexer::{token::Token, OperatorTrie},
    parser::{
        builtins::{self, BOTTOM_OP, POSTFIX_OP, PREFIX_OP, TOP_OP},
        parselet::{PatternAction, PostfixAction, PrefixAction},
        prec::{Associativity, Poset},
    },
};

pub struct SlangContext {
    pub trie: OperatorTrie,
    pub poset: Poset<Token>,
    pub prefix_actions: HashMap<Token, Arc<dyn PrefixAction>>,
    pub postfix_actions: HashMap<Token, (Option<Associativity>, Arc<dyn PostfixAction>)>,
    pub pattern_actions: HashMap<Token, Arc<dyn PatternAction>>,
    pub files: SimpleFiles<String, String>,
}

impl SlangContext {
    pub fn new() -> SlangContext {
        let mut poset = Poset::new();

        // Add in control operators.
        poset
            .try_add_lt(BOTTOM_OP.clone(), PREFIX_OP.clone())
            .unwrap();
        poset
            .try_add_lt(PREFIX_OP.clone(), POSTFIX_OP.clone())
            .unwrap();
        poset
            .try_add_lt(POSTFIX_OP.clone(), TOP_OP.clone())
            .unwrap();

        SlangContext {
            trie: OperatorTrie::new(),
            poset,
            prefix_actions: builtins::BUILTIN_PREFIX_ACTIONS.clone(),
            postfix_actions: builtins::BUILTIN_POSTFIX_ACTIONS.clone(),
            pattern_actions: builtins::BUILTIN_PATTERN_ACTIONS.clone(),
            files: SimpleFiles::new(),
        }
    }
}
