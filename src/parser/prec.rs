use std::{
    borrow::Borrow,
    cmp::Ordering,
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
};

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Fixity {
    Prefix,
    Infix,
    Postfix,
}

#[derive(Debug, Copy, Clone)]
pub enum Associativity {
    Left,
    Right,
    NonAssoc,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TryAddEqError {
    Cycle,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TryAddLtError {
    Cycle,
}

pub type TryAddGtError = TryAddLtError;

#[derive(Debug)]
pub struct Poset<T> {
    index: HashMap<T, usize>,
    next_index: usize,
    less_than: HashMap<usize, HashSet<usize>>,
}

impl<T> Poset<T> {
    pub fn new() -> Poset<T> {
        Poset {
            index: HashMap::new(),
            next_index: 0,
            less_than: HashMap::new(),
        }
    }

    fn derive_index(&mut self, x: T) -> usize
    where
        T: Eq + Hash,
    {
        if let Some(&i) = self.index.get(&x) {
            i
        } else {
            let ind = self.next_index;
            self.next_index += 1;
            self.index.insert(x, ind);
            ind
        }
    }

    /// Gets members of the poset that have the given index.
    fn of_index(&self, i: usize) -> Vec<&T> {
        self.index
            .iter()
            .filter_map(|(k, v)| if i == *v { Some(k) } else { None })
            .collect()
    }

    /// Show the chain `a` < ... < `b`.
    fn build_order_chain<Q: ?Sized>(&self, a: &Q, b: &Q) -> Vec<Vec<&T>>
    where
        T: Borrow<Q>,
        T: Eq + Hash,
        Q: Eq + Hash,
    {
        let chain = Vec::new();
        if let (Some(&a), Some(&b)) = (self.index.get(a), self.index.get(b)) {
            let x = self.of_index(a);
            todo!()
        } else {
            chain
        }
    }

    /// Adds a `lhs == rhs` relationship to the poset.
    pub fn try_add_eq(&mut self, lhs: T, rhs: T) -> Result<(), TryAddEqError>
    where
        T: Eq + Hash,
    {
        // Check if either side has an index already, insert objects as the same index.
        match (self.index.get(&lhs), self.index.get(&rhs)) {
            (Some(&l), Some(&r)) if l != r => {
                // If lhs < rhs or lhs > rhs already, panic because this would cause a cycle.
                let lhs_lt_rhs = self
                    .less_than
                    .get(&l)
                    .map(|gt| gt.contains(&r))
                    .unwrap_or(false);

                if lhs_lt_rhs {
                    return Err(TryAddEqError::Cycle);
                }

                let lhs_gt_rhs = self
                    .less_than
                    .get(&r)
                    .map(|gt| gt.contains(&l))
                    .unwrap_or(false);

                if lhs_gt_rhs {
                    return Err(TryAddEqError::Cycle);
                }

                // Unify the indices of lhs and rhs.
                // Choose `l` as the one and true index. Remap rhs => l.
                self.index.insert(rhs, l);

                // Go through and replace all instances of `r` with `l` in the less_than relation.
                let gt_r = self.less_than.remove(&r);
                if let Some(gt_l) = self.less_than.get_mut(&l) {
                    if let Some(gt_r) = gt_r {
                        gt_l.extend(gt_r.iter());
                    }
                }

                for gt in self.less_than.values_mut() {
                    if gt.contains(&r) {
                        gt.remove(&r);
                        gt.insert(l);
                    }
                }
            }
            (Some(&i), _) => {
                self.index.insert(rhs, i);
            }
            (_, Some(&i)) => {
                self.index.insert(lhs, i);
            }
            (None, None) => {
                let i = self.derive_index(lhs);
                self.index.insert(rhs, i);
            }
        }

        Ok(())
    }

    /// Adds a `lhs < rhs` relationship to the poset.
    pub fn try_add_lt(&mut self, lhs: T, rhs: T) -> Result<(), TryAddLtError>
    where
        T: Eq + Hash,
    {
        let l = self.derive_index(lhs);
        let r = self.derive_index(rhs);

        // If the poset already knows lhs > rhs, adding would cause a cycle.
        if let Some(true) = self.less_than.get(&r).map(|gt| gt.contains(&l)) {
            return Err(TryAddLtError::Cycle);
        }

        let greater_than_lhs = self.less_than.entry(l).or_insert_with(|| HashSet::new());

        // Add lhs < rhs direct relationship.
        greater_than_lhs.insert(r);

        // Build transitive relationships by induction.
        // For all z where z < lhs, add relationship z < rhs.
        // (z < lhs /\ lhs < rhs -> z < rhs)
        for greater_than_z in self.less_than.values_mut() {
            if greater_than_z.contains(&l) {
                greater_than_z.insert(r);
            }
        }

        // For all z where rhs < z, add relationship lhs < z.
        // (rhs < z /\ lhs < rhs -> lhs < z)
        let zs = self
            .less_than
            .get(&r)
            .map_or_else(|| HashSet::new(), |s| s.clone());
        if let Some(s) = self.less_than.get_mut(&l) {
            s.extend(zs.iter());
        }

        Ok(())
    }

    /// Adds a `lhs > rhs` relationship to the poset.
    pub fn try_add_gt(&mut self, lhs: T, rhs: T) -> Result<(), TryAddGtError>
    where
        T: Eq + Hash,
    {
        self.try_add_lt(rhs, lhs)
    }

    /// Compares `lhs` and `rhs` to determine an ordering. If either `lhs` or `rhs` were never
    /// added to the set, returns `None`.
    pub fn cmp<Q: ?Sized>(&self, lhs: &Q, rhs: &Q) -> Option<Ordering>
    where
        T: Borrow<Q>,
        T: Eq + Hash,
        Q: Eq + Hash,
    {
        if let (Some(l), Some(r)) = (self.index.get(lhs), self.index.get(rhs)) {
            if l == r {
                return Some(Ordering::Equal);
            }

            if let Some(lhs_less_than) = self.less_than.get(l) {
                if lhs_less_than.contains(r) {
                    return Some(Ordering::Less);
                }
            }

            if let Some(rhs_less_than) = self.less_than.get(r) {
                if rhs_less_than.contains(l) {
                    return Some(Ordering::Greater);
                }
            }

            None
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn empty() {
        let poset: Poset<char> = Poset::new();
        assert_eq!(poset.cmp(&'a', &'b'), None);
    }

    #[test]
    fn lt() {
        let mut poset = Poset::new();
        poset.try_add_lt('a', 'b').unwrap();
        assert_eq!(poset.cmp(&'a', &'b'), Some(Ordering::Less));
    }

    #[test]
    fn gt() {
        let mut poset = Poset::new();
        poset.try_add_gt('a', 'b').unwrap();
        assert_eq!(poset.cmp(&'a', &'b'), Some(Ordering::Greater));
    }

    #[test]
    fn transitive() {
        let mut poset = Poset::new();
        poset.try_add_lt('a', 'b').unwrap();
        poset.try_add_lt('b', 'c').unwrap();
        assert_eq!(poset.cmp(&'a', &'c'), Some(Ordering::Less));
        assert_eq!(poset.cmp(&'c', &'a'), Some(Ordering::Greater));
    }

    #[test]
    fn transitive_rev() {
        let mut poset = Poset::new();
        poset.try_add_lt('b', 'c').unwrap();
        poset.try_add_lt('a', 'b').unwrap();
        assert_eq!(poset.cmp(&'a', &'c'), Some(Ordering::Less));
        assert_eq!(poset.cmp(&'c', &'a'), Some(Ordering::Greater));
    }

    #[test]
    fn unrelated() {
        let mut poset = Poset::new();
        poset.try_add_lt('a', 'b').unwrap();
        poset.try_add_lt('x', 'y').unwrap();
        assert_eq!(poset.cmp(&'a', &'y'), None);
    }

    #[test]
    fn cycle() {
        let mut poset = Poset::new();
        poset.try_add_lt('a', 'b').unwrap();
        poset.try_add_lt('b', 'c').unwrap();
        assert_eq!(poset.try_add_lt('c', 'a'), Err(TryAddLtError::Cycle));
        assert_eq!(poset.try_add_gt('a', 'c'), Err(TryAddGtError::Cycle));
    }

    #[test]
    fn eq_transitivity() {
        let mut poset = Poset::new();
        poset.try_add_lt('a', 'b').unwrap();
        poset.try_add_eq('b', 'x').unwrap();
        poset.try_add_eq('x', 'z').unwrap();
        poset.try_add_lt('x', 'y').unwrap();
        assert_eq!(poset.cmp(&'a', &'y'), Some(Ordering::Less));
        assert_eq!(poset.cmp(&'y', &'a'), Some(Ordering::Greater));
        assert_eq!(poset.cmp(&'b', &'x'), Some(Ordering::Equal));
        assert_eq!(poset.cmp(&'b', &'z'), Some(Ordering::Equal));
    }

    #[test]
    fn eq_cycle() {
        let mut poset = Poset::new();
        poset.try_add_lt('a', 'b').unwrap();
        poset.try_add_lt('b', 'c').unwrap();
        assert_eq!(poset.try_add_eq('a', 'c'), Err(TryAddEqError::Cycle));
    }

    #[test]
    fn eq_idempotent() {
        let mut poset = Poset::new();
        poset.try_add_lt('a', 'b').unwrap();
        poset.try_add_eq('a', 'a').unwrap();
        assert_eq!(poset.cmp(&'a', &'b'), Some(Ordering::Less));
    }
}
