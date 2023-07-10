use crate::source_location::{SourceLocation, SourceSpan};
use crate::string_interner::{StringInterner, Symbol};
use std::iter::Peekable;
use std::str::Chars;
use serde::Serialize;

#[derive(Clone, Copy, Debug, Serialize)]
pub struct Token {
    pub span: SourceSpan,
    pub kind: TokenKind,
}

#[derive(strum_macros::IntoStaticStr, Clone, Copy, Debug, Hash, PartialEq, Eq, Serialize)]
pub enum TokenKind {
    KeywordFn,
    KeywordIf,
    KeywordElse,
    KeywordLoop,
    KeywordBreak,
    KeywordContinue,
    KeywordReturn,
    KeywordLet,
    KeywordSet,
    KeywordVoid,

    KeywordInt,

    Identifier(Symbol),

    IntLiteral(i64),
    BoolLiteral(bool),

    Arrow,

    BraceOpen,
    BraceClose,

    ParenOpen,
    ParenClose,

    Semicolon,
    Colon,

    Comma,

    Equals,

    Star,
    Minus,
    Plus,
    Div,

    CmpEq,
    CmpNe,
    CmpLt,
    CmpGt,
    CmpLe,
    CmpGe,

    Not,

    LogicalAnd,
    LogicalOr,

    BitwiseAnd,
    BitwiseOr,

    Xor,
}

pub struct Tokenizer<'a> {
    source: &'a str,
    iter: Peekable<Chars<'a>>,
    loc: SourceLocation,
    offset: usize,
    str_interner: &'a mut StringInterner,
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a str, str_interner: &'a mut StringInterner) -> Self {
        Tokenizer {
            source,
            iter: source.chars().peekable(),
            loc: SourceLocation { line: 1, col: 1 },
            offset: 0,
            str_interner,
        }
    }

    fn advance(&mut self) -> Option<(usize, SourceLocation, char)> {
        let c = self.iter.next()?;

        let offset = self.offset;
        let loc = self.loc;

        if c == '\n' {
            self.loc.line += 1;
            self.loc.col = 1;
        } else {
            self.loc.col += 1;
        }

        self.offset += c.len_utf8();

        Some((offset, loc, c))
    }

    fn skip_whitespace(&mut self) {
        loop {
            let Some(c) = self.iter.peek() else { break; };

            if !c.is_whitespace() {
                break;
            }

            self.advance();
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.skip_whitespace();

        let (offset, start, c) = self.advance()?;

        #[rustfmt::skip]
        let tok = match c {
            // Identifier or keyword.
            _ if c.is_ascii_alphabetic() || c == '_' => {
                fn is_identifier_char(c: char) -> bool {
                    c.is_ascii_alphanumeric() || c == '_'
                }

                let (mut end_offset, mut end_loc) = (offset + c.len_utf8(), start);
                loop {
                    let Some(a) = self.iter.peek() else { break; };

                    if !is_identifier_char(*a) {
                        break;
                    }

                    let b;
                    (_, end_loc, b) = self.advance().unwrap();
                    end_offset += b.len_utf8();
                }

                let identifier = &self.source[offset..end_offset];

                let span = SourceSpan {
                    start,
                    end: end_loc,
                };

                match identifier {
                    "fn"       => Token { span, kind: TokenKind::KeywordFn, },
                    "if"       => Token { span, kind: TokenKind::KeywordIf, },
                    "else"     => Token { span, kind: TokenKind::KeywordElse, },
                    "loop"     => Token { span, kind: TokenKind::KeywordLoop, },
                    "break"    => Token { span, kind: TokenKind::KeywordBreak, },
                    "continue" => Token { span, kind: TokenKind::KeywordContinue, },
                    "return"   => Token { span, kind: TokenKind::KeywordReturn, },
                    "let"      => Token { span, kind: TokenKind::KeywordLet, },
                    "set"      => Token { span, kind: TokenKind::KeywordSet, },
                    "void"     => Token { span, kind: TokenKind::KeywordVoid, },
                    "int"      => Token { span, kind: TokenKind::KeywordInt, },
                    "false"    => Token { span, kind: TokenKind::BoolLiteral(false), },
                    "true"     => Token { span, kind: TokenKind::BoolLiteral(true), },
                    _ => {
                        let sym = self.str_interner.add(identifier);
                        Token {
                            span,
                            kind: TokenKind::Identifier(sym),
                        }
                    }
                }
            }

            _ if c.is_ascii_digit() => {
                let (mut end_offset, mut end_loc) = (offset + c.len_utf8(), start);
                loop {
                    let Some(d) = self.iter.peek() else { break; };

                    if !d.is_ascii_digit() {
                        break;
                    }

                    let b;
                    (_, end_loc, b) = self.advance().unwrap();
                    end_offset += b.len_utf8();
                }

                let number: i64 = self.source[offset..end_offset].parse().unwrap();

                let span = SourceSpan {
                    start,
                    end: end_loc,
                };

                Token {
                    span,
                    kind: TokenKind::IntLiteral(number),
                }
            }

            ';' => Token { span: SourceSpan::single(start), kind: TokenKind::Semicolon, },
            ':' => Token { span: SourceSpan::single(start), kind: TokenKind::Colon, },

            '(' => Token { span: SourceSpan::single(start), kind: TokenKind::ParenOpen, },
            ')' => Token { span: SourceSpan::single(start), kind: TokenKind::ParenClose, },
            '{' => Token { span: SourceSpan::single(start), kind: TokenKind::BraceOpen, },
            '}' => Token { span: SourceSpan::single(start), kind: TokenKind::BraceClose, },

            ',' => Token { span: SourceSpan::single(start), kind: TokenKind::Comma, },

            '=' => {
                if let Some('=') = self.iter.peek() {
                    let (_, end, _) = self.advance().unwrap();
                    Token {
                        span: SourceSpan { start, end },
                        kind: TokenKind::CmpEq,
                    }
                } else {
                    Token { span: SourceSpan::single(start), kind: TokenKind::Equals, }
                }
            },
            '*' => Token { span: SourceSpan::single(start), kind: TokenKind::Star, },
            '+' => Token { span: SourceSpan::single(start), kind: TokenKind::Plus, },
            '/' => Token { span: SourceSpan::single(start), kind: TokenKind::Div, },
            '^' => Token { span: SourceSpan::single(start), kind: TokenKind::Xor, },
            '>' => {
                if let Some('=') = self.iter.peek() {
                    let (_, end, _) = self.advance().unwrap();
                    Token {
                        span: SourceSpan { start, end },
                        kind: TokenKind::CmpGe,
                    }
                } else {
                    Token {
                        span: SourceSpan::single(start),
                        kind: TokenKind::CmpGt,
                    }
                }
            },
            '<' => {
                if let Some('=') = self.iter.peek() {
                    let (_, end, _) = self.advance().unwrap();
                    Token {
                        span: SourceSpan { start, end },
                        kind: TokenKind::CmpLe,
                    }
                } else {
                    Token {
                        span: SourceSpan::single(start),
                        kind: TokenKind::CmpLt,
                    }
                }
            },
            '-' => {
                if let Some('>') = self.iter.peek() {
                    let (_, end, _) = self.advance().unwrap();
                    Token {
                        span: SourceSpan { start, end },
                        kind: TokenKind::Arrow,
                    }
                } else {
                    Token {
                        span: SourceSpan::single(start),
                        kind: TokenKind::Minus,
                    }
                }
            }
            '!' => {
                if let Some('=') = self.iter.peek() {
                    let (_, end, _) = self.advance().unwrap();
                    Token {
                        span: SourceSpan { start, end },
                        kind: TokenKind::CmpNe,
                    }
                } else {
                    Token {
                        span: SourceSpan::single(start),
                        kind: TokenKind::Not,
                    }
                }
            },
            '&' => {
                if let Some('&') = self.iter.peek() {
                    let (_, end, _) = self.advance().unwrap();
                    Token {
                        span: SourceSpan { start, end },
                        kind: TokenKind::LogicalAnd,
                    }
                } else {
                    Token {
                        span: SourceSpan::single(start),
                        kind: TokenKind::BitwiseAnd,
                    }
                }
            }
            '|' => {
                if let Some('|') = self.iter.peek() {
                    let (_, end, _) = self.advance().unwrap();
                    Token {
                        span: SourceSpan { start, end },
                        kind: TokenKind::LogicalOr,
                    }
                } else {
                    Token {
                        span: SourceSpan::single(start),
                        kind: TokenKind::BitwiseOr,
                    }
                }
            }
            _ => todo!(),
        };

        Some(tok)
    }
}
