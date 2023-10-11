use crate::source_location::{SourceLocation, SourceSpan};
use crate::string_interner::{StringInterner, Symbol};
use serde::Serialize;
use std::iter::Peekable;
use std::str::Chars;

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

    KeywordCast,

    KeywordEnd,

    KeywordOr,
    KeywordAnd,
    KeywordNot,
    KeywordXor,

    KeywordBor,
    KeywordBand,

    // TODO call these keywords?
    Equal,
    NotEqual,
    LessEqual,
    Less,
    GreaterEqual,
    Greater,

    Identifier(Symbol),

    IntLiteral(i64),
    BoolLiteral(bool),

    BraceOpen,
    BraceClose,

    ParenOpen,
    ParenClose,

    Semicolon,
    Colon,

    Comma,

    EqualSign,

    Ampersand,
    At,

    Hat,
    Star,
    Minus,
    Plus,
    Div,
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
            let Some(c) = self.iter.peek() else {
                break;
            };

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
                    "cast"     => Token { span, kind: TokenKind::KeywordCast, },
                    "end"      => Token { span, kind: TokenKind::KeywordEnd, },

                    "and"      => Token { span, kind: TokenKind::KeywordAnd, },
                    "or"       => Token { span, kind: TokenKind::KeywordOr, },
                    "not"      => Token { span, kind: TokenKind::KeywordNot, },
                    "xor"      => Token { span, kind: TokenKind::KeywordXor, },
                    "bor"      => Token { span, kind: TokenKind::KeywordBor, },
                    "band"     => Token { span, kind: TokenKind::KeywordBand, },
                    "eq"       => Token { span, kind: TokenKind::Equal, },
                    "ne"       => Token { span, kind: TokenKind::NotEqual, },
                    "le"       => Token { span, kind: TokenKind::LessEqual, },
                    "lt"       => Token { span, kind: TokenKind::Less, },
                    "ge"       => Token { span, kind: TokenKind::GreaterEqual, },
                    "gt"       => Token { span, kind: TokenKind::Greater, },

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

            '=' => Token { span: SourceSpan::single(start), kind: TokenKind::EqualSign, },

            '*' => Token { span: SourceSpan::single(start), kind: TokenKind::Star, },
            '+' => Token { span: SourceSpan::single(start), kind: TokenKind::Plus, },
            '/' => Token { span: SourceSpan::single(start), kind: TokenKind::Div, },
            '-' => Token { span: SourceSpan::single(start), kind: TokenKind::Minus, },
            '^' => Token { span: SourceSpan::single(start), kind: TokenKind::Hat, },
            '&' => Token { span: SourceSpan::single(start), kind: TokenKind::Ampersand, },
            '@' => Token { span: SourceSpan::single(start), kind: TokenKind::At, },

            _ => todo!("Character '{}' not recognized", c),
        };

        Some(tok)
    }
}

#[cfg(test)]
mod tests {
    use super::Tokenizer;
    use crate::string_interner::StringInterner;
    use insta::assert_ron_snapshot;

    #[test]
    fn tokenizer_creates_the_expected_tokens() {
        let source = "fn if else + - * ; : { } ( ) void let voidlet 4687 continue return-,=       
            int eq / ne ge le lt gt true false loop break bool int void end";

        let mut symbols = StringInterner::new();
        let tokenizer = Tokenizer::new(source, &mut symbols);

        let tokens = tokenizer.collect::<Vec<_>>();

        assert_ron_snapshot!("tokens_snapshot", tokens);
    }
}
