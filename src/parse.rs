use crate::ast;
use crate::ast::*;
use crate::string_interner::{StringInterner, Symbol};
use std::iter::Peekable;
use std::str::Chars;

#[derive(Clone, Copy, Debug)]
pub struct SourceSpan {
    start: SourceLocation,
    end: SourceLocation,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd)]
pub struct SourceLocation {
    line: u32,
    col: u32,
}

impl SourceSpan {
    fn single(loc: SourceLocation) -> SourceSpan {
        SourceSpan {
            start: loc,
            end: loc,
        }
    }

    fn extend(&self, other: &SourceSpan) -> SourceSpan {
        assert!(self.start <= other.start);
        assert!(self.end <= other.end);

        SourceSpan {
            start: self.start,
            end: other.end,
        }
    }
}

impl SourceLocation {
    fn invalid() -> SourceLocation {
        SourceLocation { line: 0, col: 0 }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Token {
    pub span: SourceSpan,
    pub kind: TokenKind,
}

impl Token {
    fn new(span: SourceSpan, kind: TokenKind) -> Token {
        todo!()
    }
}

#[derive(strum_macros::IntoStaticStr, Clone, Copy, Debug)]
pub enum TokenKind {
    KeywordFn,
    KeywordIf,
    KeywordElse,
    KeywordLoop,
    KeywordBreak,
    KeywordContinue,
    KeywordReturn,
    KeywordLet,
    KeywordVoid,
    KeywordInt,

    Identifier(Symbol),

    IntLiteral(i64),

    Arrow,

    BraceOpen,
    BraceClose,

    ParenOpen,
    ParenClose,

    Semicolon,
    Colon,

    Comma,

    EqualSign,
    Star,
    Minus,
    Plus,
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
                    "void"     => Token { span, kind: TokenKind::KeywordVoid, },
                    "int"      => Token { span, kind: TokenKind::KeywordInt, },
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
            _ => todo!(),
        };

        Some(tok)
    }
}

impl<'a> TokenIterator for Tokenizer<'a> {}

type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedEnd,
    UnexpectedToken(SourceSpan, &'static str),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for ParseError {}

pub fn parse(source: &str, str_interner: &mut StringInterner) -> Result<Vec<Item>> {
    let mut tokenizer = Tokenizer::new(source, str_interner).peekable();

    let mut items = Vec::new();
    while tokenizer.peek().is_some() {
        items.push(parse_item(&mut tokenizer)?);
    }

    Ok(items)
}

type TokenStream<I> = Peekable<I>;
trait TokenIterator: Iterator<Item = Token> {}

macro_rules! expect_token {
    ($tok:expr, $p:pat) => {
        if let Some(tok) = $tok {
            let token_matches_pattern = matches!(tok.kind, $p);
            if !token_matches_pattern {
                Err(ParseError::UnexpectedToken(tok.span, tok.kind.into()))
            } else {
                Ok(tok)
            }
        } else {
            Err(ParseError::UnexpectedEnd)
        }
    };
}

fn parse_type<I: TokenIterator>(tokens: &mut TokenStream<I>) -> Result<Type> {
    use TokenKind::*;

    let ty = expect_token!(tokens.next(), (Identifier(_) | KeywordInt | KeywordVoid))?;

    let kind = match ty.kind {
        Identifier(sym) => TypeKind::Identifier(sym),
        KeywordInt => TypeKind::Int,
        KeywordVoid => TypeKind::Void,
        _ => unreachable!(),
    };

    Ok(Type {
        attr: Attributes::from_span(ty.span),
        kind,
    })
}

fn parse_expression_element<I: TokenIterator>(tokens: &mut TokenStream<I>) -> Result<Expr> {
    use TokenKind::*;

    let tok = expect_token!(tokens.next(), _)?;

    let e = match tok.kind {
        Minus => {
            let e = parse_expression(tokens)?.into();
            Expr::new(
                tok.span,
                ExprKind::UnaryOp {
                    op: UnaryOpKind::Negate,
                    e,
                },
            )
        }
        ParenOpen => {
            let inner = parse_expression(tokens)?;
            expect_token!(tokens.next(), ParenClose)?;
            inner
        }
        IntLiteral(x) => Expr::new(tok.span, ExprKind::IntLiteral(x)),
        Identifier(sym) => {
            if let Some(Token {
                kind: ParenOpen, ..
            }) = tokens.peek()
            {
                tokens.next();

                let mut args = Vec::new();
                loop {
                    let a = expect_token!(tokens.peek(), _)?;
                    if matches!(a.kind, ParenClose) {
                        tokens.next();
                        break;
                    }

                    let arg = parse_expression(tokens)?;
                    args.push(arg);

                    let a = expect_token!(tokens.peek(), (Comma | ParenClose))?;
                    if matches!(a.kind, Comma) {
                        tokens.next();
                    }
                }

                Expr::new(tok.span, ExprKind::Call { sym, args })
            } else {
                Expr::new(tok.span, ExprKind::Identifier(sym))
            }
        }

        _ => todo!(),
    };

    Ok(e)
}

fn parse_expression<I: TokenIterator>(tokens: &mut TokenStream<I>) -> Result<Expr> {
    let lhs = parse_expression_element(tokens)?;
    parse_binop_expression(tokens, 0, lhs)
}

fn parse_binop_expression<I: TokenIterator>(
    tokens: &mut TokenStream<I>,
    precedence: u32,
    mut lhs: Expr,
) -> Result<Expr> {
    use TokenKind::*;

    fn is_binary_op(tok_kind: TokenKind) -> bool {
        use TokenKind::*;
        matches!(tok_kind, Plus | Star | Minus)
    }

    fn binary_op_precedence(tok_kind: TokenKind) -> u32 {
        use TokenKind::*;

        match tok_kind {
            Plus => 10,
            Minus => 10,
            Star => 20,
            _ => panic!("If this gets triggered, then you probably are recognizing a token as a binary operator when it should not be."),
        }
    }

    fn op_token_to_opkind(tok_kind: TokenKind) -> BinaryOpKind {
        match tok_kind {
            Plus => BinaryOpKind::Add,
            Minus => BinaryOpKind::Sub,
            _ => todo!(),
        }
    }

    loop {
        let Some(&tok) = tokens.peek() else { return Ok(lhs); };
        if !is_binary_op(tok.kind) {
            return Ok(lhs);
        }

        // Eat the operator
        tokens.next();

        let op_precedence = binary_op_precedence(tok.kind);

        if op_precedence < precedence {
            return Ok(lhs);
        }

        let mut rhs = parse_expression_element(tokens)?;

        // Check if we have another operator
        let peeked = tokens.peek();
        if peeked.is_none() || peeked.map(|tok| !is_binary_op(tok.kind)).unwrap() {
            let op = op_token_to_opkind(tok.kind);
            return Ok(Expr::new(
                tok.span,
                ExprKind::BinaryOp {
                    op,
                    lhs: lhs.into(),
                    rhs: rhs.into(),
                },
            ));
        };

        let Some(&next_op) = peeked else { unreachable!() };

        // Eat the operator
        tokens.next();

        let next_precedence = binary_op_precedence(next_op.kind);
        if next_precedence > op_precedence {
            rhs = parse_binop_expression(tokens, next_precedence, rhs)?;
        }

        let op = op_token_to_opkind(next_op.kind);
        lhs = Expr::new(
            tok.span,
            ExprKind::BinaryOp {
                op,
                lhs: lhs.into(),
                rhs: rhs.into(),
            },
        );
    }
}

fn parse_statement<I: TokenIterator>(tokens: &mut TokenStream<I>) -> Result<Statement> {
    use StatementKind::*;
    use TokenKind::*;

    let tok = expect_token!(tokens.next(), _)?;

    let statement = match tok.kind {
        KeywordLet => {
            let name = expect_token!(tokens.next(), Identifier(_))?;
            let Identifier(sym) = name.kind else { unreachable!(); };

            expect_token!(tokens.next(), Colon)?;

            let ty = parse_type(tokens)?;

            expect_token!(tokens.next(), EqualSign)?;

            let init: Box<ast::Expr> = parse_expression(tokens)?.into();

            Statement::new(tok.span.extend(&init.attr.span), Let { sym, ty, init })
        }
        KeywordLoop => todo!(),
        KeywordBreak => Statement::new(tok.span, Break),
        KeywordContinue => Statement::new(tok.span, Continue),
        KeywordReturn => {
            let tok2 = expect_token!(tokens.peek(), _)?;
            if matches!(tok2.kind, Semicolon) {
                Statement::new(tok.span, Return(None))
            } else {
                let return_value = parse_expression(tokens)?;
                Statement::new(
                    tok.span.extend(&return_value.attr.span),
                    Return(Some(return_value.into())),
                )
            }
        }
        BraceOpen => todo!(),
        _ => todo!(),
    };

    expect_token!(tokens.next(), Semicolon)?;

    Ok(statement)
}

fn parse_block<I: TokenIterator>(tokens: &mut TokenStream<I>) -> Result<Block> {
    use TokenKind::*;

    let open_brace = expect_token!(tokens.next(), BraceOpen)?;

    let mut statements = Vec::new();

    loop {
        let peeked = expect_token!(tokens.peek(), _)?;
        if matches!(peeked.kind, BraceClose) {
            break;
        }

        let statement = parse_statement(tokens)?;
        statements.push(statement);
    }

    let close_brace = expect_token!(tokens.next(), BraceClose)?;

    Ok(Block {
        attr: Attributes::from_span(open_brace.span.extend(&close_brace.span)),
        statements,
    })
}

fn parse_params<I: TokenIterator>(tokens: &mut TokenStream<I>) -> Result<Vec<Param>> {
    use TokenKind::*;

    expect_token!(tokens.next(), ParenOpen)?;

    let mut params = Vec::new();
    loop {
        let peeked = expect_token!(tokens.peek(), _)?;
        if matches!(peeked.kind, ParenClose) {
            break;
        }

        let name = expect_token!(tokens.next(), Identifier(_))?;
        let Identifier(sym) = name.kind else { unreachable!(); };
        expect_token!(tokens.next(), Colon)?;
        let ty = parse_type(tokens)?;

        params.push(Param {
            attr: Attributes::from_span(name.span.extend(&ty.attr.span)),
            name: sym,
            ty,
        });

        let peeked = expect_token!(tokens.peek(), (Comma | ParenClose))?;
        if matches!(peeked.kind, Comma) {
            tokens.next();
        }
    }

    expect_token!(tokens.next(), ParenClose)?;

    Ok(params)
}

fn parse_item<I: TokenIterator>(tokens: &mut TokenStream<I>) -> Result<Item> {
    use TokenKind::*;

    let fn_keyword = expect_token!(tokens.next(), KeywordFn)?;
    let name_token = expect_token!(tokens.next(), Identifier(_))?;
    let Identifier(name) = name_token.kind else { unreachable!() };

    let params = parse_params(tokens)?;

    expect_token!(tokens.next(), Arrow)?;

    let return_type = parse_type(tokens)?;

    let body: Box<Block> = parse_block(tokens)?.into();

    let item = Item {
        attr: Attributes::from_span(fn_keyword.span.extend(&body.attr.span)),
        kind: ItemKind::Function(Function {
            return_type,
            name,
            params,
            body,
        }),
    };

    Ok(item)
}
