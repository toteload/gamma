use std::clone::Clone;
use std::str::Chars;

#[derive(PartialEq, Debug, Clone, Copy)]
pub struct SourceLocation {
    pub line: u32,
    pub col: u32,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub struct SourceSpan {
    start: SourceLocation,
    end: SourceLocation, // inclusive
}

impl SourceSpan {
    fn new(start: SourceLocation, end: SourceLocation) -> SourceSpan {
        SourceSpan { start, end }
    }

    fn single(loc: SourceLocation) -> SourceSpan {
        SourceSpan {
            start: loc,
            end: loc,
        }
    }

    pub fn invalid() -> SourceSpan {
        SourceSpan::single(SourceLocation::invalid())
    }
}

impl SourceLocation {
    fn new(line: u32, col: u32) -> SourceLocation {
        SourceLocation { line, col }
    }

    pub fn invalid() -> SourceLocation {
        SourceLocation { line: 0, col: 0 }
    }
}

enum CodeChar {
    EndOfSource {
        offset: usize,
    },
    Char {
        ch: char,
        loc: SourceLocation,
        offset: usize,
    },
}

struct CodeCharIterator<'a> {
    it: Chars<'a>,
    loc: SourceLocation,
    last_loc: SourceLocation,
    offset: usize,
    has_sent_end: bool,
    peeked: Option<CodeChar>,
    peeked_loc: SourceLocation,
}

impl<'a> CodeCharIterator<'a> {
    fn new(source: &'a str) -> Self {
        CodeCharIterator {
            it: source.chars(),
            loc: SourceLocation { line: 1, col: 1 },
            last_loc: SourceLocation { line: 1, col: 1 },
            offset: 0,
            has_sent_end: false,
            peeked: None,
            peeked_loc: SourceLocation { line: 1, col: 1 },
        }
    }

    fn peek(&mut self) -> Option<&CodeChar> {
        if self.peeked.is_none() {
            let last_loc = self.last_loc;
            self.peeked = self.next();
            self.peeked_loc = self.last_loc;
            self.last_loc = last_loc;
        }

        self.peeked.as_ref()
    }
}

impl<'a> Iterator for CodeCharIterator<'a> {
    type Item = CodeChar;

    fn next(&mut self) -> Option<CodeChar> {
        if self.peeked.is_some() {
            self.last_loc = self.peeked_loc;
            return self.peeked.take();
        }

        if let Some(c) = self.it.next() {
            let cc = Some(CodeChar::Char {
                ch: c,
                loc: self.loc,
                offset: self.offset,
            });

            self.last_loc = self.loc;

            self.offset += c.len_utf8();

            if c == '\n' {
                self.loc.line += 1;
                self.loc.col = 1;
            } else {
                self.loc.col += 1;
            }

            cc
        } else if !self.has_sent_end {
            self.has_sent_end = true;
            Some(CodeChar::EndOfSource {
                offset: self.offset,
            })
        } else {
            None
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum TokenKind<'a> {
    BraceOpen,
    BraceClose,
    Ident(&'a str),
    StringLit(&'a str),
}

#[derive(Debug, PartialEq)]
pub struct Token<'a> {
    kind: TokenKind<'a>,
    loc: SourceSpan,
}

pub struct Tokenizer<'a> {
    source: &'a str,
    it: CodeCharIterator<'a>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a str) -> Tokenizer {
        Tokenizer {
            source,
            it: CodeCharIterator::new(source),
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(cc) = self.it.peek() {
            match cc {
                CodeChar::EndOfSource { .. } => break,
                CodeChar::Char { ch, .. } => {
                    if !ch.is_whitespace() {
                        break;
                    }
                }
            }

            self.it.next();
        }
    }

    fn last_location(&self) -> SourceLocation {
        self.it.last_loc
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
        self.skip_whitespace();

        if let Some(cc) = self.it.next() {
            match cc {
                CodeChar::EndOfSource { .. } => None,
                CodeChar::Char { ch, loc, offset } => match ch {
                    '(' => Some(Token {
                        kind: TokenKind::BraceOpen,
                        loc: SourceSpan::single(loc),
                    }),
                    ')' => Some(Token {
                        kind: TokenKind::BraceClose,
                        loc: SourceSpan::single(loc),
                    }),
                    '"' => {
                        // Advance until we find another '"' or the end of the source.
                        while let Some(CodeChar::Char { ch, .. }) = self.it.peek() {
                            if *ch == '"' {
                                break;
                            }

                            self.it.next();
                        }

                        let Some(CodeChar::Char { ch: '"', offset: end, .. }) = self.it.next() else {
                            todo!()
                        };

                        Some(Token {
                            kind: TokenKind::StringLit(&self.source[offset..end]),
                            loc: SourceSpan {
                                start: loc,
                                end: self.last_location(),
                            },
                        })
                    }
                    _ => {
                        let end = {
                            // Consume iterator until you hit whitespace or
                            // a brace.
                            while let Some(CodeChar::Char { ch, .. }) = self.it.peek() {
                                if ch.is_whitespace() || ch == &'(' || ch == &')' {
                                    break;
                                }

                                self.it.next();
                            }

                            if let Some(cc) = self.it.peek() {
                                match cc {
                                    CodeChar::EndOfSource { offset } => offset,
                                    CodeChar::Char { offset, .. } => offset,
                                }
                            } else {
                                unreachable!();
                            }
                        };

                        Some(Token {
                            kind: TokenKind::Ident(&self.source[offset..*end]),
                            loc: SourceSpan {
                                start: loc,
                                end: self.last_location(),
                            },
                        })
                    }
                },
            }
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnmatchedClosingBrace(SourceLocation),
    UnmatchedOpeningBrace,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for ParseError {}

#[derive(Debug)]
pub enum Tree<T> {
    Branch(Vec<Tree<T>>),
    Leaf(T),
}

impl<T> Tree<T> {
    pub fn map<U, F: Fn(&T) -> U>(&self, f: F) -> Tree<U> {
        // I use this inner function, because the recursion requires the function `f` to be
        // borrowed. It cannot be moved multiple times. I don't want to pass in a borrowed
        // funcion to `map`, so this was the solution.
        fn inner<T, U, F: Fn(&T) -> U>(t: &Tree<T>, f: &F) -> Tree<U> {
            use Tree::*;

            match t {
                Branch(xs) => Branch(xs.into_iter().map(|x| inner(x, f)).collect()),
                Leaf(x) => Leaf(f(x)),
            }
        }

        inner(self, &f)
    }

    fn zip<U: Clone>(self, other: Tree<U>) -> Option<Tree<(T, U)>> {
        use Tree::*;

        match (self, other) {
            (Branch(xs), Branch(ys)) if xs.len() == ys.len() => {
                let mut zs = Vec::new();
                for (x, y) in xs.into_iter().zip(ys) {
                    if let Some(z) = x.zip(y) {
                        zs.push(z);
                    } else {
                        return None;
                    }
                }

                Some(Branch(zs))
            }
            (Leaf(x), Leaf(y)) => Some(Leaf((x, y))),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tree_map() {
        use Tree::*;

        let t: Tree<i32> = Branch(vec![Leaf(123), Leaf(456), Branch(vec![Leaf(789)])]);

        let s = t.map(|&x| x as f32);

        let Branch(xs) = s else { panic!("Should be a list") };

        assert!(matches!(
            xs.as_slice(),
            [Leaf(123.0), Leaf(456.0), Branch(_)]
        ));

        let [_, _, Branch(xs)] = xs.as_slice() else { panic!(); };

        assert!(matches!(xs.as_slice(), [Leaf(789.0)]));
    }
}

#[derive(Clone)]
pub struct SExpr<'a> {
    pub span: SourceSpan,
    pub kind: SExprKind<'a>,
}

#[derive(Clone)]
pub enum SExprKind<'a> {
    List(Vec<SExpr<'a>>),
    Ident(&'a str),
    LitString(&'a str),
}

pub fn parse(source: &str) -> Result<SExpr, ParseError> {
    let mut stack = vec![(SourceSpan::invalid(), Vec::new())];
    let mut tokenizer = Tokenizer::new(source);

    for tok in tokenizer {
        match tok.kind {
            TokenKind::BraceOpen => {
                stack.push((tok.loc, Vec::new()));
            }
            TokenKind::Ident(id) => {
                stack.last_mut().unwrap().1.push(SExpr {
                    span: tok.loc,
                    kind: SExprKind::Ident(id),
                });
            }
            TokenKind::StringLit(_) => todo!(),
            TokenKind::BraceClose => {
                if stack.len() == 1 {
                    return Err(ParseError::UnmatchedClosingBrace(tok.loc.start));
                }

                if let Some((span, elems)) = stack.pop() {
                    stack.last_mut().unwrap().1.push(SExpr {
                        span: SourceSpan::new(span.start, tok.loc.end),
                        kind: SExprKind::List(elems),
                    });
                } else {
                    unreachable!()
                }
            }
        }
    }

    if stack.len() > 1 {
        return Err(ParseError::UnmatchedOpeningBrace);
    }

    Ok(SExpr {
        span: SourceSpan::invalid(),
        kind: SExprKind::List(stack.pop().unwrap().1),
    })
}
