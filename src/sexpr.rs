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
    fn single(loc: SourceLocation) -> SourceSpan {
        SourceSpan {
            start: loc,
            end: loc,
        }
    }
}

impl SourceLocation {
    fn new(line: u32, col: u32) -> SourceLocation {
        SourceLocation { line, col }
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
    Atom(&'a str),
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
                            kind: TokenKind::Atom(&self.source[offset..*end]),
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
    UnmatchedOpeningBrace(SourceLocation),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for ParseError {}

#[derive(Debug)]
pub enum Tree<T> {
    List(Vec<Tree<T>>),
    Atom(T),
}

impl<T> Tree<T> {
    pub fn map<U, F: Fn(&T) -> U>(&self, f: F) -> Tree<U> {
        fn inner<T, U, F: Fn(&T) -> U>(t: &Tree<T>, f: &F) -> Tree<U> {
            use Tree::*;

            match t {
                List(xs) => List(xs.into_iter().map(|x| inner(x, f)).collect()),
                Atom(x) => Atom(f(x)),
            }
        }

        inner(self, &f)
    }

    fn zip_with<U: Clone>(self, other: Tree<U>) -> Tree<(T, U)> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tree_map() {
        use Tree::*;

        let t: Tree<i32> = List(vec![Atom(123), Atom(456), List(vec![Atom(789)])]);

        let s = t.map(|&x| x as f32);

        let List(xs) = s else { panic!("Should be a list") };

        assert!(matches!(xs.as_slice(), [Atom(123.0), Atom(456.0), List(_)]));

        let [_, _, List(xs)] = xs.as_slice() else { panic!(); };

        assert!(matches!(xs.as_slice(), [Atom(789.0)]));
    }
}

fn tree_zip<T: Clone, U: Clone>(a: Tree<T>, b: Tree<U>) -> Result<Tree<(T, U)>, ()> {
    match (a, b) {
        (Tree::Atom(x), Tree::Atom(y)) => Ok(Tree::Atom((x, y))),
        (Tree::List(xs), Tree::List(ys)) if xs.len() == ys.len() => {
            let list = xs
                .into_iter()
                .zip(ys)
                .map(|(x, y)| tree_zip(x, y))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Tree::List(list))
        }
        _ => Err(()),
    }
}

pub struct SExpr<'a> {
    pub loc: SourceSpan,
    pub kind: SExprKind<'a>,
}

struct SExprList<'a>(Vec<SExpr<'a>>);

impl<'a> SExprList<'a> {
    fn kinds(&self) -> Vec<&SExprKind<'a>> {
        self.0.iter().map(|x| &x.kind).collect()
    }
}

pub enum SExprKind<'a> {
    List(Vec<SExpr<'a>>),
    Atom(&'a str),
}

pub fn parse(source: &str) -> Result<SExpr, ParseError> {
    let mut stack = vec![(SourceLocation::new(1, 1), Vec::new())];
    let mut tokenizer = Tokenizer::new(source);

    while let Some(tok) = tokenizer.next() {
        match tok.kind {
            TokenKind::BraceOpen => {
                stack.push((tok.loc.start, Vec::new()));
            }
            TokenKind::Atom(id) => {
                stack.last_mut().unwrap().1.push(SExpr {
                    loc: tok.loc,
                    kind: SExprKind::Atom(id),
                });
            }
            TokenKind::BraceClose => {
                if stack.len() == 1 {
                    return Err(ParseError::UnmatchedClosingBrace(tok.loc.start));
                }

                if let Some((start, elems)) = stack.pop() {
                    stack.last_mut().unwrap().1.push(SExpr {
                        loc: SourceSpan {
                            start,
                            end: tok.loc.end,
                        },
                        kind: SExprKind::List(elems),
                    });
                } else {
                    unreachable!()
                }
            }
        }
    }

    if stack.len() > 1 {
        return Err(ParseError::UnmatchedOpeningBrace(stack.last().unwrap().0));
    }

    Ok(SExpr {
        loc: SourceSpan {
            start: SourceLocation::new(1, 1),
            end: tokenizer.last_location(),
        },
        kind: SExprKind::List(stack.pop().unwrap().1),
    })
}

/*
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenizer() {
        let source = "
(fn main ()
  32)";

        let mut tokenizer = Tokenizer::new(&source);

        assert_eq!(
            tokenizer.next(),
            Some(Token {
                loc: SourceSpan::single(SourceLocation { line: 2, col: 1 }),
                kind: TokenKind::BraceOpen
            })
        );

        assert_eq!(
            tokenizer.next(),
            Some(Token {
                loc: SourceSpan {
                    start: SourceLocation { line: 2, col: 2 },
                    end: SourceLocation { line: 2, col: 3 },
                },
                kind: TokenKind::Atom("fn")
            })
        );

        assert_eq!(
            tokenizer.next(),
            Some(Token {
                loc: SourceSpan {
                    start: SourceLocation { line: 2, col: 5 },
                    end: SourceLocation { line: 2, col: 8 },
                },
                kind: TokenKind::Atom("main")
            })
        );

        assert_eq!(
            tokenizer.next(),
            Some(Token {
                loc: SourceSpan::single(SourceLocation { line: 2, col: 10 }),
                kind: TokenKind::BraceOpen
            })
        );

        assert_eq!(
            tokenizer.next(),
            Some(Token {
                loc: SourceSpan::single(SourceLocation { line: 2, col: 11 }),
                kind: TokenKind::BraceClose
            })
        );

        assert_eq!(
            tokenizer.next(),
            Some(Token {
                loc: SourceSpan {
                    start: SourceLocation { line: 3, col: 3 },
                    end: SourceLocation { line: 3, col: 4 },
                },
                kind: TokenKind::Atom("32")
            })
        );

        assert_eq!(
            tokenizer.next(),
            Some(Token {
                loc: SourceSpan::single(SourceLocation { line: 3, col: 5 }),
                kind: TokenKind::BraceClose
            })
        );

        assert_eq!(tokenizer.next(), None);
    }

    #[test]
    fn parser() {
        let source = "(abc () 12345)";
        assert!(parse(&source).is_ok());

        use super::SExprNode::*;

        assert_eq!(
            parse(&source).unwrap(),
            SExpr {
                nodes: vec![
                    SExprNode::List { offset: 1, len: 1 },
                    SExprNode::List { offset: 1, len: 3 },
                    SExprNode::Atom("abc"),
                    SExprNode::List { offset: 2, len: 0 },
                    SExprNode::Atom("12345"),
                ],
                locs: vec![
                    SourceSpan {
                        start: SourceLocation { line: 0, col: 0 },
                        end: SourceLocation { line: 0, col: 0 }
                    },
                    SourceSpan {
                        start: SourceLocation { line: 1, col: 1 },
                        end: SourceLocation { line: 1, col: 14 }
                    },
                    SourceSpan {
                        start: SourceLocation { line: 1, col: 2 },
                        end: SourceLocation { line: 1, col: 4 }
                    },
                    SourceSpan {
                        start: SourceLocation { line: 1, col: 6 },
                        end: SourceLocation { line: 1, col: 7 }
                    },
                    SourceSpan {
                        start: SourceLocation { line: 1, col: 9 },
                        end: SourceLocation { line: 1, col: 13 }
                    },
                ],
            }
        );
    }

    #[test]
    fn unmatched_opening_brace() {
        let source = "(a b c d (())";

        let res = parse(&source);
        assert!(res.is_err());

        let err = res.unwrap_err();
        assert_eq!(
            err,
            ParseError::UnmatchedOpeningBrace(SourceLocation {
                line: 1,
                col: 1
            })
        );
    }

    #[test]
    fn unmatched_closing_brace() {
        let source = "(a b c d ())))))";

        let res = parse(&source);
        assert!(res.is_err());

        let err = res.unwrap_err();
        assert_eq!(
            err,
            ParseError::UnmatchedClosingBrace(SourceLocation {
                line: 1,
                col: 13
            })
        );
    }
}
*/
