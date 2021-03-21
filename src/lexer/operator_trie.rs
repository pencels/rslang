use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct OperatorTrie {
    /// Map from char to children nodes with that char as their prefix.
    children: HashMap<char, OperatorTrie>,
    /// Marks a node as an end of a possible operator.
    pub end: bool,
}

impl OperatorTrie {
    pub fn new() -> OperatorTrie {
        OperatorTrie {
            children: hashmap! {},
            end: false,
        }
    }

    /// Inserts an operator string into the trie.
    pub fn insert_operator(&mut self, operator: &str) {
        let mut node = self;
        for ch in operator.chars() {
            node = node
                .children
                .entry(ch)
                .or_insert_with(|| OperatorTrie::new());
        }
        node.end = true;
    }

    /// Gets the direct child of the node associated with the given char.
    pub fn get_child(&self, c: char) -> Option<&OperatorTrie> {
        self.children.get(&c)
    }

    /// Finds a node in the trie by following the chars in the given string.
    pub fn find(&self, operator: &str) -> Option<&OperatorTrie> {
        let mut node = self;
        for ch in operator.chars() {
            match node.children.get(&ch) {
                Some(child_node) => node = child_node,
                None => return None,
            }
        }
        Some(node)
    }

    pub fn contains(&self, operator: &str) -> bool {
        self.find(operator).is_some()
    }

    /// Merges another trie's operators into this one.
    pub fn merge_with(&mut self, other: &OperatorTrie) {
        for (c, other_t) in &other.children {
            if let Some(my_t) = self.children.get_mut(c) {
                my_t.end |= other_t.end;
                my_t.merge_with(other_t);
            } else {
                self.children.insert(*c, other_t.clone());
            }
        }
    }
}
