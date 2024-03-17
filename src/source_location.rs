use serde::Serialize;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Serialize)]
pub struct SourceLocation {
    pub line: u32,
    pub col: u32,
}

impl SourceLocation {
    fn invalid() -> SourceLocation {
        SourceLocation { line: 0, col: 0 }
    }
}

#[derive(Clone, Copy, Debug, Serialize)]
pub struct SourceSpan {
    pub start: SourceLocation,
    pub end: SourceLocation,
}

impl SourceSpan {
    pub fn single(loc: SourceLocation) -> SourceSpan {
        SourceSpan {
            start: loc,
            end: SourceLocation {
                line: loc.line,
                col: loc.col + 1,
            },
        }
    }

    pub fn extend(&self, other: &SourceSpan) -> SourceSpan {
        debug_assert!(self.start <= other.start);
        debug_assert!(self.end <= other.end);

        SourceSpan {
            start: self.start,
            end: other.end,
        }
    }
}
