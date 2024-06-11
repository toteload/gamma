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

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, Serialize)]
pub enum TokenKind {
    // Item
    KeywordFn,
    KeywordExternalFn,
    KeywordLayout,

    // Statement
    KeywordIf,
    KeywordElse,
    KeywordLoop,
    KeywordBreak,
    KeywordContinue,
    KeywordReturn,
    KeywordLet,
    KeywordSet,
    KeywordEnd,

    KeywordCast,

    // Logical operator
    KeywordOr,
    KeywordAnd,
    KeywordNot,

    // Bitwise operator
    KeywordBor,
    KeywordBand,
    KeywordXor,

    // START - Currently unused
    KeywordEq,
    KeywordNe,
    KeywordLe,
    KeywordLt,
    KeywordGe,
    KeywordGt,
    // END

    // Literal
    IntLiteral(i64),
    BoolLiteral(bool),

    // Arithmetic operators
    KeywordRem, // Remainder
    Star,
    Minus,
    Plus,
    Div,

    // (De)referencing
    Ampersand,
    At,

    // Unorganized below
    // -----------------

    // TODO call these keywords?
    Equal,
    NotEqual,
    LessEqual,
    Less,
    GreaterEqual,
    Greater,

    Identifier(Symbol),
    Label(Symbol),

    LineComment(Symbol),

    BraceOpen,
    BraceClose,

    ParenOpen,
    ParenClose,

    BracketOpen,
    BracketClose,

    Semicolon,
    Colon,

    Comma,
    Period,

    EqualSign,
    Hat,
}

pub struct Tokenizer<'a> {
    source: &'a str,
    iter: Peekable<Chars<'a>>,
    loc: SourceLocation,
    offset: usize,
    str_interner: &'a mut StringInterner,
    skip_comments: bool,
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a str, str_interner: &'a mut StringInterner, skip_comments: bool) -> Self {
        Tokenizer {
            source,
            iter: source.chars().peekable(),
            loc: SourceLocation { line: 1, col: 1 },
            offset: 0,
            str_interner,
            skip_comments,
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
        while let Some(c) = self.iter.peek() {
            if !c.is_whitespace() {
                break;
            }

            self.advance();
        }
    }

    fn read_until_end_of_line(&mut self) -> (usize, SourceLocation) {
        while let Some((offset, loc, c)) = self.advance() {
            if c == '\n' {
                return (offset, loc);
            }
        }

        (self.offset, self.loc)
    }

    // Return str offset and Location of the last character of the identifier
    fn read_identifier(&mut self) -> (usize, SourceLocation) {
        while let Some(c) = self.iter.peek() {
            if !is_identifier_rest_char(*c) {
                break;
            }

            self.advance();
        }

        (self.offset, self.loc)
    }

    fn read_while<P: Fn(char) -> bool>(&mut self, predicate: P) -> (usize, SourceLocation) {
        while let Some(c) = self.iter.peek() {
            if !predicate(*c) {
                break;
            }

            self.advance();
        }

        (self.offset, self.loc)
    }
}

fn is_identifier_start_char(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_identifier_rest_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.skip_whitespace();

        let (offset, start, c) = self.advance()?;

        #[rustfmt::skip]
        let tok = match c {
            // Identifier or keyword.
            _ if is_identifier_start_char(c) => {
                let (end_offset, end_loc) = self.read_identifier();

                let identifier = &self.source[offset..end_offset];

                let span = SourceSpan {
                    start,
                    end: end_loc,
                };

                match identifier {
                    "external_fn" => Token { span, kind: TokenKind::KeywordExternalFn, },
                    "fn"       => Token { span, kind: TokenKind::KeywordFn, },
                    "layout"   => Token { span, kind: TokenKind::KeywordLayout, },
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
                    "rem"      => Token { span, kind: TokenKind::KeywordRem, },
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

            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' => 'number_block: {
                if c == '0' && self.iter.peek().map_or(false, |&c| c == 'x') {
                    self.advance();
                    let (end_offset, end) = self.read_while(|c| c.is_ascii_hexdigit());

                    let val: i64 = i64::from_str_radix(&self.source[offset+2..end_offset], 16).unwrap();
                    let span = SourceSpan { start, end };
                    break 'number_block Token {
                        span,
                        kind: TokenKind::IntLiteral(val),
                    };
                }

                if c == '-' {
                    if self.iter.peek().map_or(false, |c| c.is_ascii_digit()) {
                        // negative number
                        let (end_offset, end) = self.read_while(|c| c.is_ascii_digit());

                let number: i64 = self.source[offset..end_offset].parse().unwrap();
                let span = SourceSpan { start, end };

                break 'number_block Token {
                    span,
                    kind: TokenKind::IntLiteral(number),
                };

                    } else {
                        break 'number_block Token { span: SourceSpan::single(start), kind: TokenKind::Minus, };
                    }
                }

                let (end_offset, end) = self.read_while(|c| c.is_ascii_digit());

                let number: i64 = self.source[offset..end_offset].parse().unwrap();
                let span = SourceSpan { start, end };

                Token {
                    span,
                    kind: TokenKind::IntLiteral(number),
                }
            }

            '#' => {
                let (end_offset, end) = self.read_until_end_of_line();
                if self.skip_comments {
                    return self.next();
                }

                let comment = &self.source[offset..end_offset];
                let sym = self.str_interner.add(comment);
                Token {
                    span: dbg!(SourceSpan { start, end }),
                    kind: TokenKind::LineComment(sym),
                }
            }

            ';' => Token { span: SourceSpan::single(start), kind: TokenKind::Semicolon, },
            ':' => Token { span: SourceSpan::single(start), kind: TokenKind::Colon, },

            '(' => Token { span: SourceSpan::single(start), kind: TokenKind::ParenOpen, },
            ')' => Token { span: SourceSpan::single(start), kind: TokenKind::ParenClose, },
            '{' => Token { span: SourceSpan::single(start), kind: TokenKind::BraceOpen, },
            '}' => Token { span: SourceSpan::single(start), kind: TokenKind::BraceClose, },
            '[' => Token { span: SourceSpan::single(start), kind: TokenKind::BracketOpen, },
            ']' => Token { span: SourceSpan::single(start), kind: TokenKind::BracketClose, },

            ',' => Token { span: SourceSpan::single(start), kind: TokenKind::Comma, },
            '.' => Token { span: SourceSpan::single(start), kind: TokenKind::Period, },

            '=' => Token { span: SourceSpan::single(start), kind: TokenKind::EqualSign, },

            '*' => Token { span: SourceSpan::single(start), kind: TokenKind::Star, },
            '+' => Token { span: SourceSpan::single(start), kind: TokenKind::Plus, },
            '/' => Token { span: SourceSpan::single(start), kind: TokenKind::Div, },
            '^' => Token { span: SourceSpan::single(start), kind: TokenKind::Hat, },
            '&' => Token { span: SourceSpan::single(start), kind: TokenKind::Ampersand, },

            '@' =>
                match self.iter.peek() {
                    Some(d) if is_identifier_start_char(*d) => {
                        let (end_offset, end) = self.read_identifier();
                        let identifier = &self.source[offset..end_offset];
                        let span = SourceSpan { start, end };
                        let sym = self.str_interner.add(identifier);
                        Token {
                            span,
                            kind: TokenKind::Label(sym),
                        }
                    },
                    _ => Token { span: SourceSpan::single(start), kind: TokenKind::At, },
                },

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
        let source = "fn if else + - * / : { } ( ) [ ] let voidlet 4687 continue return , =       
eq ne ge le lt gt true false loop break bool void end";

        let mut symbols = StringInterner::new();
        let tokenizer = Tokenizer::new(source, &mut symbols, true);

        let tokens = tokenizer.collect::<Vec<_>>();

        assert_ron_snapshot!("tokens_snapshot", tokens);
    }
}
