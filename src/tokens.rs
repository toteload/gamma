use std::str::Chars;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct SourceLocation {
    pub line: u32,
    pub col: u32,
}

pub struct Tokenizer<'a> {
    it: CodeCharIterator<'a>,
    source: &'a str,
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a str) -> Tokenizer {
        Tokenizer {
            it: CodeCharIterator::new(source),
            source,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenKind<'a> {
    KeywordFn,
    Identifier(&'a str),
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    LiteralInt(&'a str),
}

impl<'a> TokenKind<'a> {
    pub fn as_str(&self) -> &'a str {
        use TokenKind::*;

        match self {
            Identifier(id) | LiteralInt(id) => id,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Token<'a> {
    pub loc: SourceLocation,
    pub kind: TokenKind<'a>,
}

struct CodeCharIterator<'a> {
    it: Chars<'a>,
    peeked: Option<CodeChar>,
    offset: usize,
    loc: SourceLocation,
}

#[derive(Copy, Clone)]
struct CodeChar {
    ch: char,
    loc: SourceLocation,
    offset: usize,
}

impl<'a> CodeCharIterator<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            it: source.chars(),
            peeked: None,
            offset: 0,
            loc: SourceLocation { line: 1, col: 1 },
        }
    }

    fn peek(&mut self) -> Option<CodeChar> {
        if self.peeked.is_none() {
            self.peeked = self.next()
        }

        self.peeked
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if !c.ch.is_whitespace() {
                break;
            }

            self.next();
        }
    }
}

impl<'a> Iterator for CodeCharIterator<'a> {
    type Item = CodeChar;

    fn next(&mut self) -> Option<CodeChar> {
        if self.peeked.is_some() {
            return self.peeked.take();
        }

        let res = if let Some(c) = self.it.next() {
            let res = Some(CodeChar {
                ch: c,
                loc: self.loc,
                offset: self.offset,
            });

            self.offset += c.len_utf8();

            if c == '\n' {
                self.loc.line += 1;
                self.loc.col = 1;
            } else {
                self.loc.col += 1;
            }

            res
        } else {
            None
        };

        self.peeked = None;

        res
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
        self.it.skip_whitespace();

        if let Some(cc) = self.it.next() {
            if cc.ch == '{' {
                return Some(Token {
                    loc: cc.loc,
                    kind: TokenKind::BraceOpen,
                });
            }
            if cc.ch == '}' {
                return Some(Token {
                    loc: cc.loc,
                    kind: TokenKind::BraceClose,
                });
            }
            if cc.ch == '(' {
                return Some(Token {
                    loc: cc.loc,
                    kind: TokenKind::ParenOpen,
                });
            }
            if cc.ch == ')' {
                return Some(Token {
                    loc: cc.loc,
                    kind: TokenKind::ParenClose,
                });
            }

            if cc.ch.is_ascii_digit() {
                let end = loop {
                    if let Some(cc) = self.it.peek() {
                        if !cc.ch.is_ascii_digit() {
                            break cc.offset;
                        }

                        self.it.next();
                    } else {
                        break self.it.offset;
                    }
                };

                return Some(Token {
                    loc: cc.loc,
                    kind: TokenKind::LiteralInt(&self.source[cc.offset..end]),
                });
            }

            fn is_identifier_start(c: char) -> bool {
                c.is_ascii_alphabetic() || c == '_'
            }

            fn is_identifier_char(c: char) -> bool {
                is_identifier_start(c) || c.is_ascii_digit()
            }

            if !is_identifier_start(cc.ch) {
                todo!();
            }

            let end = loop {
                if let Some(cc) = self.it.peek() {
                    if !is_identifier_char(cc.ch) {
                        break cc.offset;
                    } else {
                        self.it.next();
                    }
                } else {
                    break self.it.offset;
                }
            };

            let id = &self.source[cc.offset..end];

            Some(Token {
                loc: cc.loc,
                kind: match id {
                    "fn" => TokenKind::KeywordFn,
                    _ => TokenKind::Identifier(id),
                },
            })
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenizer_test() {
        let source = "
fn main() {
    32
} 128";
        let mut tokenizer = Tokenizer::new(&source);

        assert_eq!(
            tokenizer.next(),
            Some(Token {
                loc: SourceLocation { line: 2, col: 1 },
                kind: TokenKind::KeywordFn
            })
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token {
                loc: SourceLocation { line: 2, col: 4 },
                kind: TokenKind::Identifier(&source[4..8])
            })
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token {
                loc: SourceLocation { line: 2, col: 8 },
                kind: TokenKind::ParenOpen
            })
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token {
                loc: SourceLocation { line: 2, col: 9 },
                kind: TokenKind::ParenClose
            })
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token {
                loc: SourceLocation { line: 2, col: 11 },
                kind: TokenKind::BraceOpen
            })
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token {
                loc: SourceLocation { line: 3, col: 5 },
                kind: TokenKind::LiteralInt("32")
            })
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token {
                loc: SourceLocation { line: 4, col: 1 },
                kind: TokenKind::BraceClose
            })
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token {
                loc: SourceLocation { line: 4, col: 3 },
                kind: TokenKind::LiteralInt("128")
            })
        );
    }
}
