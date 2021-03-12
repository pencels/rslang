use std::cmp::{max, min};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        assert!(start <= end);
        Span { start, end }
    }

    pub fn unite(self, next: Span) -> Span {
        Span {
            start: min(self.start, next.start),
            end: max(self.end, next.end),
        }
    }

    pub fn dummy() -> Span {
        Span { start: 0, end: 0 }
    }
}
