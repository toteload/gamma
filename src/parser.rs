use crate::ast::*;
use crate::error::*;
use crate::source_location::SourceSpan;
use crate::string_interner::StringInterner;
use crate::tokenizer::{Token, TokenKind, Tokenizer};
use std::collections::HashMap;
use std::iter::Peekable;

type Result<T> = std::result::Result<T, Error>;

pub struct Parser<'a> {
    tokens: Peekable<Tokenizer<'a>>,
    id_generator: &'a mut NodeIdGenerator,

    // TODO: Use the `nohash-hasher` Hasher, if this turns out to be kinda slow.
    spans: &'a mut HashMap<NodeId, SourceSpan>,
}

macro_rules! expect_token {
    // The pattern matching code for `pattern` is taken from:
    // https://doc.rust-lang.org/src/core/macros/mod.rs.html#342
    ($token:expr, $(|)? $( $pattern:pat_param )|+ $( if $guard: expr )? $(,)?) => {
        if let Some(tok) = $token {
            let token_matches_pattern = matches!(tok.kind, $( $pattern )|+ $( if $guard )?);
            if !token_matches_pattern {
                Err(Error {
                    span: Some(tok.span),
                    info: vec![ErrorInfo::Text("Encountered unexpected token "), ErrorInfo::SourceText(tok.span)],
                })
            } else {
                Ok(tok)
            }
        } else {
            Err(Error {
                span: None,
                info: vec![ErrorInfo::Text("Unexpected end of source")],
            })
        }
    };
}

impl Parser<'_> {
    pub fn new<'a>(
        source: &'a str,
        string_interner: &'a mut StringInterner,
        id_generator: &'a mut NodeIdGenerator,
        spans: &'a mut HashMap<NodeId, SourceSpan>,
    ) -> Parser<'a> {
        Parser {
            tokens: Tokenizer::new(source, string_interner).peekable(),
            id_generator,
            spans,
        }
    }

    fn gen_node_id(&mut self) -> NodeId {
        self.id_generator.gen_id()
    }

    pub fn parse_items(&mut self) -> Result<Vec<Item>> {
        let mut items = Vec::new();

        loop {
            if self.tokens.peek().is_none() {
                break;
            }

            let item = self.parse_item()?;
            items.push(item)
        }

        Ok(items)
    }

    fn parse_name(&mut self) -> Result<Name> {
        use TokenKind::*;

        let name_token = expect_token!(self.tokens.next(), Identifier(_))?;

        let Identifier(sym) = name_token.kind else {
            unreachable!()
        };

        let id = self.id_generator.gen_id();

        let span = name_token.span;

        self.spans.insert(id, span);

        Ok(Name { id, sym })
    }

    fn parse_item(&mut self) -> Result<Item> {
        use TokenKind::*;

        let tok = expect_token!(self.tokens.next(), KeywordFn | KeywordExternalFn)?;
        let id = self.id_generator.gen_id();

        let kind = match tok.kind {
            KeywordFn => {
                let name = self.parse_name()?;

                let params = self.parse_params()?;

                expect_token!(self.tokens.next(), Colon)?;

                let return_type = self.parse_type()?;

                let body: Box<Block> = self.parse_block()?.into();

                let span = tok.span.extend(self.spans.get(&body.id).unwrap());

                self.spans.insert(id, span);

                ItemKind::Function {
                    return_type,
                    name,
                    params,
                    body,
                }
            }
            KeywordExternalFn => {
                let name = self.parse_name()?;

                let params = self.parse_params()?;

                expect_token!(self.tokens.next(), Colon)?;

                let return_type = self.parse_type()?;

                let span = tok.span.extend(self.spans.get(&return_type.id).unwrap());

                self.spans.insert(id, span);

                ItemKind::ExternalFunction {
                    return_type,
                    name,
                    params,
                }
            }
            _ => unreachable!(),
        };

        let item = Item { id, kind };

        Ok(item)
    }

    fn parse_array_type(&mut self) -> Result<(SourceSpan, TypeKind)> {
        use TokenKind::*;

        let Token { span: start, .. } = expect_token!(self.tokens.next(), BracketOpen)?;

        let Token {
            kind: IntLiteral(size),
            ..
        } = expect_token!(self.tokens.next(), IntLiteral(_))?
        else {
            unreachable!()
        };

        expect_token!(self.tokens.next(), BracketClose)?;

        let (end, base_type) = self.parse_base_type()?;

        let span = start.extend(&end);

        Ok((span, TypeKind::Array(size, Box::new(base_type))))
    }

    fn parse_pointer_type(&mut self) -> Result<(SourceSpan, TypeKind)> {
        use TokenKind::*;

        let Token { span: start, .. } = expect_token!(self.tokens.next(), Hat)?;

        let (end, base_type) = self.parse_base_type()?;

        let span = start.extend(&end);

        Ok((span, TypeKind::Pointer(Box::new(base_type))))
    }

    fn parse_base_type(&mut self) -> Result<(SourceSpan, TypeKind)> {
        use TokenKind::*;

        let Token { kind, span } =
            *expect_token!(self.tokens.peek(), Hat | BracketOpen | Identifier(_))?;

        match kind {
            Hat => self.parse_pointer_type(),
            BracketOpen => self.parse_array_type(),
            Identifier(sym) => {
                self.tokens.next();
                Ok((span, TypeKind::Identifier(sym)))
            }
            _ => unreachable!(),
        }
    }

    fn parse_type(&mut self) -> Result<Type> {
        let (span, kind) = self.parse_base_type()?;

        let id = self.gen_node_id();

        self.spans.insert(id, span);

        Ok(Type { id, kind })
    }

    fn parse_params(&mut self) -> Result<Vec<Param>> {
        use TokenKind::*;

        expect_token!(self.tokens.next(), ParenOpen)?;

        let mut params = Vec::new();
        loop {
            let peeked = expect_token!(self.tokens.peek(), _)?;
            if matches!(peeked.kind, ParenClose) {
                break;
            }

            let name = self.parse_name()?;

            expect_token!(self.tokens.next(), Colon)?;

            let ty = self.parse_type()?;

            let id = self.gen_node_id();
            let span = self
                .spans
                .get(&name.id)
                .unwrap()
                .extend(self.spans.get(&ty.id).unwrap());

            params.push(Param { id, name, ty });

            let peeked = expect_token!(self.tokens.peek(), Comma | ParenClose)?;
            if matches!(peeked.kind, Comma) {
                self.tokens.next();
            }
        }

        expect_token!(self.tokens.next(), ParenClose)?;

        Ok(params)
    }

    fn parse_block(&mut self) -> Result<Block> {
        use TokenKind::*;

        let mut statements = Vec::new();

        loop {
            let peeked = expect_token!(self.tokens.peek(), _)?;
            if matches!(peeked.kind, KeywordEnd) {
                break;
            }

            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        let end = expect_token!(self.tokens.next(), KeywordEnd)?;

        let id = self.gen_node_id();

        let span = match statements.as_slice() {
            [first, .., last] => {
                let first = self
                    .spans
                    .get(&first.id)
                    .expect("Statement should have a span saved");

                let last = self
                    .spans
                    .get(&last.id)
                    .expect("Statement should have a span saved");

                first.extend(&last)
            }
            [statement] => *self
                .spans
                .get(&statement.id)
                .expect("Statement should have a span saved"),

            // TODO(david) This should return an empty span, but that is currently not possible
            // because the end of a SourceSpan is inclusive...
            [] => SourceSpan::single(end.span.start),
        };

        self.spans.insert(id, span);

        Ok(Block { id, statements })
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        use StatementKind::*;
        use TokenKind::*;

        let id = self.gen_node_id();
        let span: SourceSpan;

        let tok = *expect_token!(self.tokens.peek(), _)?;

        let kind = match tok.kind {
            KeywordLet => {
                self.tokens.next();

                let name = self.parse_name()?;

                expect_token!(self.tokens.next(), Colon)?;

                let ty = self.parse_type()?;

                let peeked = expect_token!(self.tokens.peek(), _)?;

                if matches!(peeked.kind, EqualSign) {
                    // Eat the '='
                    self.tokens.next();

                    let val: Box<_> = self.parse_expression()?.into();

                    span = tok.span.extend(self.spans.get(&val.id).unwrap());

                    Let {
                        name,
                        ty,
                        init: Some(val),
                    }
                } else {
                    span = tok.span.extend(self.spans.get(&ty.id).unwrap());

                    Let {
                        name,
                        ty,
                        init: None,
                    }
                }
            }
            KeywordSet => {
                self.tokens.next();

                let dst: Box<_> = self.parse_expression()?.into();

                expect_token!(self.tokens.next(), EqualSign)?;

                let val: Box<_> = self.parse_expression()?.into();

                span = tok.span.extend(self.spans.get(&val.id).unwrap());

                Set { dst, val }
            }
            KeywordIf => {
                self.tokens.next();

                let cond: Box<_> = self.parse_expression()?.into();

                let then = self.parse_block()?;

                let peeked = expect_token!(self.tokens.peek(), _)?;

                let otherwise = if matches!(peeked.kind, KeywordElse) {
                    // Eat the 'else'
                    self.tokens.next();

                    let otherwise = self.parse_block()?;

                    span = tok.span.extend(self.spans.get(&otherwise.id).unwrap());

                    Some(otherwise)
                } else {
                    span = tok.span.extend(self.spans.get(&then.id).unwrap());
                    None
                };

                If {
                    cond,
                    then,
                    otherwise,
                }
            }
            KeywordBreak => {
                self.tokens.next();
                span = tok.span;
                Break
            }
            KeywordContinue => {
                self.tokens.next();
                span = tok.span;
                Continue
            }
            KeywordReturn => {
                self.tokens.next();

                let return_value = self.parse_expression()?;
                span = tok.span.extend(self.spans.get(&return_value.id).unwrap());
                Return(Some(return_value.into()))
            }
            KeywordLoop => {
                self.tokens.next();

                let body = self.parse_block()?;

                span = tok.span.extend(self.spans.get(&body.id).unwrap());

                Loop(body)
            }
            _ => {
                let e = self.parse_expression()?;
                span = *self.spans.get(&e.id).unwrap();
                Expr(e.into())
            }
        };

        self.spans.insert(id, span);

        Ok(Statement { id, kind })
    }

    fn parse_arguments(&mut self) -> Result<Vec<Expr>> {
        use TokenKind::*;

        let mut args = vec![];

        loop {
            let peeked = expect_token!(self.tokens.peek(), _)?;
            if matches!(peeked.kind, ParenClose) {
                break;
            }

            args.push(self.parse_expression()?);
        }

        Ok(args)
    }

    fn parse_expression(&mut self) -> Result<Expr> {
        use TokenKind::*;

        let id = self.gen_node_id();
        let span: SourceSpan;

        let tok = expect_token!(
            self.tokens.next(),
            IntLiteral(_) | BoolLiteral(_) | Identifier(_) | ParenOpen
        )?;

        let kind = match tok.kind {
            IntLiteral(x) => {
                span = tok.span;
                ExprKind::IntLiteral(x)
            }
            BoolLiteral(x) => {
                span = tok.span;
                ExprKind::BoolLiteral(x)
            }
            Identifier(sym) => {
                span = tok.span;
                ExprKind::Identifier(sym)
            }
            ParenOpen => {
                let start_span = tok.span;

                let tok = *expect_token!(self.tokens.peek(), _)?;

                if matches!(
                    tok.kind,
                    KeywordCast
                        | KeywordOr
                        | KeywordAnd
                        | KeywordXor
                        | Star
                        | Div
                        | Equal
                        | NotEqual
                        | Less
                        | Greater
                        | LessEqual
                        | GreaterEqual
                        | KeywordNot
                        | Minus
                        | Plus
                        | Ampersand
                        | At
                ) {
                    self.tokens.next();
                }

                let e = match tok.kind {
                    KeywordCast => {
                        let ty = self.parse_type()?;
                        let e = self.parse_expression()?;
                        ExprKind::Cast { ty, e: e.into() }
                    }

                    op @ (KeywordOr | KeywordAnd | KeywordXor | Star | Div | Equal | NotEqual
                    | Less | Greater | LessEqual | GreaterEqual | KeywordNot | Minus
                    | Plus | Ampersand | At) => {
                        let op = match op {
                            Equal => BuiltinOpKind::Equals,
                            Star => BuiltinOpKind::Mul,
                            Div => BuiltinOpKind::Div,
                            KeywordNot => BuiltinOpKind::Not,
                            Minus => BuiltinOpKind::Sub,
                            Plus => BuiltinOpKind::Add,
                            Ampersand => BuiltinOpKind::AddressOf,
                            At => BuiltinOpKind::At,
                            _ => todo!("Token {:?}", op),
                        };

                        ExprKind::BuiltinOp {
                            op,
                            args: self.parse_arguments()?,
                        }
                    }

                    // Function call
                    Identifier(sym) => ExprKind::Call {
                        name: self.parse_name()?,
                        args: self.parse_arguments()?,
                    },

                    _ => todo!(),
                };

                let end_token = expect_token!(self.tokens.next(), ParenClose)?;

                span = start_span.extend(&end_token.span);

                e
            }
            _ => todo!("Token \"{:?}\"", tok),
        };

        // TODO register the span for this expression
        self.spans.insert(id, span);

        Ok(Expr { id, kind })
    }
}
