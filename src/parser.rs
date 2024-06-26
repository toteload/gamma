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
    spans: &'a mut HashMap<NodeId, SourceSpan>,
}

macro_rules! expect_token {
    // The pattern matching code for `pattern` is taken from:
    // https://doc.rust-lang.org/src/core/macros/mod.rs.html#342
    ($phase:expr, $token:expr, $(|)? $( $pattern:pat_param )|+ $( if $guard: expr )? $(,)?) => {
        if let Some(tok) = $token {
            let token_matches_pattern = matches!(tok.kind, $( $pattern )|+ $( if $guard )?);
            if !token_matches_pattern {
                Err(Error {
                    source: ErrorSource::Span(tok.span),
                    info: vec![
                        ErrorInfo::Text("Encountered unexpected token "),
                        ErrorInfo::SourceText(tok.span),
                        ErrorInfo::Text(" while parsing "),
                        ErrorInfo::Text($phase),
                        ErrorInfo::Text(". Expected pattern "),
                        ErrorInfo::Text(stringify!( $( $pattern )|+ )),
                    ],
                })
            } else {
                Ok(tok)
            }
        } else {
            Err(Error {
                source: ErrorSource::Unspecified,
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
            tokens: Tokenizer::new(source, string_interner, true).peekable(),
            id_generator,
            spans,
        }
    }

    fn try_peek(&mut self) -> Result<Token> {
        self.tokens.peek().copied().ok_or_else(|| Error {
            source: ErrorSource::Unspecified,
            info: vec![ErrorInfo::Text("Unexpected end of source")],
        })
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

    fn parse_label(&mut self) -> Result<Label> {
        use TokenKind::*;

        let label_token = expect_token!("label", self.tokens.next(), Label(_))?;

        let Label(sym) = label_token.kind else {
            unreachable!()
        };

        let id = self.id_generator.gen_id();

        let span = label_token.span;

        self.spans.insert(id, span);

        Ok(Name { id, sym })
    }

    fn parse_name(&mut self) -> Result<Name> {
        use TokenKind::*;

        let name_token = expect_token!("name", self.tokens.next(), Identifier(_))?;

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

        let tok = expect_token!(
            "item",
            self.tokens.next(),
            KeywordFn | KeywordExternalFn | KeywordLayout
        )?;
        let id = self.id_generator.gen_id();

        let kind = match tok.kind {
            KeywordFn => {
                let name = self.parse_name()?;

                let params = self.parse_params()?;

                expect_token!(
                    "colon before return type of function",
                    self.tokens.next(),
                    Colon
                )?;

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

                expect_token!(
                    "color before return type of external function",
                    self.tokens.next(),
                    Colon
                )?;

                let return_type = self.parse_type()?;

                let span = tok.span.extend(self.spans.get(&return_type.id).unwrap());

                self.spans.insert(id, span);

                ItemKind::ExternalFunction {
                    return_type,
                    name,
                    params,
                }
            }
            KeywordLayout => {
                let name = self.parse_name()?;

                let mut fields = Vec::new();

                while !matches!(self.try_peek()?.kind, KeywordEnd) {
                    let name = self.parse_name()?;

                    expect_token!("color after layout field name", self.tokens.next(), Colon)?;

                    let Token {
                        kind: IntLiteral(offset),
                        ..
                    } = expect_token!("layout field offset", self.tokens.next(), IntLiteral(_))?
                    else {
                        unreachable!()
                    };

                    let offset = offset as u32;

                    let ty = self.parse_type()?;

                    fields.push(Field { name, offset, ty });
                }

                expect_token!("end keyword for layout", self.tokens.next(), KeywordEnd)?;

                ItemKind::Layout { name, fields }
            }
            _ => unreachable!(),
        };

        let item = Item { id, kind };

        Ok(item)
    }

    fn parse_array_type(&mut self) -> Result<(SourceSpan, TypeKind)> {
        use TokenKind::*;

        let Token { span: start, .. } = expect_token!(
            "opening bracket for array type",
            self.tokens.next(),
            BracketOpen
        )?;

        let Token {
            kind: IntLiteral(size),
            ..
        } = expect_token!("size of array type", self.tokens.next(), IntLiteral(_))?
        else {
            unreachable!()
        };

        expect_token!(
            "closing bracket for array type",
            self.tokens.next(),
            BracketClose
        )?;

        let (end, base_type) = self.parse_base_type()?;

        let span = start.extend(&end);

        Ok((span, TypeKind::Array(size, Box::new(base_type))))
    }

    fn parse_pointer_type(&mut self) -> Result<(SourceSpan, TypeKind)> {
        let Token { span: start, .. } =
            expect_token!("hat for pointer type", self.tokens.next(), TokenKind::Hat)?;

        let (end, base_type) = self.parse_base_type()?;

        let span = start.extend(&end);

        Ok((span, TypeKind::Pointer(Box::new(base_type))))
    }

    fn parse_base_type(&mut self) -> Result<(SourceSpan, TypeKind)> {
        use TokenKind::*;

        let Token { kind, span } = *expect_token!(
            "start of a type",
            self.tokens.peek(),
            Hat | BracketOpen | Identifier(_)
        )?;

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

        expect_token!(
            "opening parenthesis for function parameters",
            self.tokens.next(),
            ParenOpen
        )?;

        let mut params = Vec::new();
        while !matches!(self.try_peek()?.kind, ParenClose) {
            let name = self.parse_name()?;

            expect_token!(
                "colon delimiting parameter name and type",
                self.tokens.next(),
                Colon
            )?;

            let ty = self.parse_type()?;

            let id = self.gen_node_id();
            let span = self
                .spans
                .get(&name.id)
                .unwrap()
                .extend(self.spans.get(&ty.id).unwrap());

            params.push(Param { id, name, ty });

            if let Some(Token { kind: Comma, .. }) = self.tokens.peek() {
                self.tokens.next();
            }
        }

        expect_token!(
            "closing parenthesis for function parameters",
            self.tokens.next(),
            ParenClose
        )?;

        Ok(params)
    }

    fn parse_block(&mut self) -> Result<Block> {
        use TokenKind::*;

        let mut statements = Vec::new();

        while !matches!(self.try_peek()?.kind, KeywordEnd) {
            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        let end = expect_token!("end keyword for block", self.tokens.next(), KeywordEnd)?;

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

                first.extend(last)
            }
            [statement] => *self
                .spans
                .get(&statement.id)
                .expect("Statement should have a span saved"),
            [] => SourceSpan::empty_at(end.span.start),
        };

        self.spans.insert(id, span);

        Ok(Block { id, statements })
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        use StatementKind::*;
        use TokenKind::*;

        let id = self.gen_node_id();
        let span: SourceSpan;

        let tok = self.try_peek()?;

        let kind = match tok.kind {
            KeywordLet => {
                self.tokens.next();

                let name = self.parse_name()?;

                expect_token!(
                    "colon after name in let statement",
                    self.tokens.next(),
                    Colon
                )?;

                let ty = self.parse_type()?;

                let peeked = self.try_peek()?;

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

                expect_token!("= in set statement", self.tokens.next(), EqualSign)?;

                let val: Box<_> = self.parse_expression()?.into();

                span = tok.span.extend(self.spans.get(&val.id).unwrap());

                Set { dst, val }
            }
            KeywordIf => {
                self.tokens.next();

                let cond: Box<_> = self.parse_expression()?.into();

                let then = self.parse_block()?;

                let peeked = self.try_peek()?;

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

                let peeked = self.try_peek()?;

                let label = if matches!(peeked.kind, Label(_)) {
                    Some(self.parse_label()?)
                } else {
                    None
                };

                Break(label)
            }
            KeywordContinue => {
                self.tokens.next();
                span = tok.span;

                let peeked = self.try_peek()?;

                let label = if matches!(peeked.kind, Label(_)) {
                    Some(self.parse_label()?)
                } else {
                    None
                };

                Continue(label)
            }
            KeywordReturn => {
                self.tokens.next();

                let return_value = self.parse_expression()?;
                span = tok.span.extend(self.spans.get(&return_value.id).unwrap());
                Return(Some(return_value.into()))
            }
            KeywordLoop => {
                self.tokens.next();

                let peeked = self.try_peek()?;

                let label = if matches!(peeked.kind, Label(_)) {
                    Some(self.parse_label()?)
                } else {
                    None
                };

                let body = self.parse_block()?;

                span = tok.span.extend(self.spans.get(&body.id).unwrap());

                Loop(body, label)
            }
            _ => {
                let e = self.parse_expression()?;
                span = *self.spans.get(&e.id).unwrap();
                Expression(e.into())
            }
        };

        self.spans.insert(id, span);

        Ok(Statement { id, kind })
    }

    fn parse_arguments(&mut self) -> Result<Vec<Expression>> {
        use TokenKind::*;

        let mut args = vec![];

        while !matches!(self.try_peek()?.kind, ParenClose) {
            args.push(self.parse_expression()?);
        }

        Ok(args)
    }

    fn parse_accessor(&mut self) -> Result<Accessor> {
        let tok = self.try_peek()?;

        let id = self.id_generator.gen_id();

        if let TokenKind::FieldAccessor { field, full } = tok.kind {
            self.tokens.next();
            return Ok(Accessor::Field(FieldAccessor { id, field, full }));
        }

        Ok(Accessor::Expr(self.parse_expression()?))
    }

    fn parse_accessors(&mut self) -> Result<Vec<Accessor>> {
        let mut accessors = Vec::new();

        while !matches!(self.try_peek()?.kind, TokenKind::ParenClose) {
            accessors.push(self.parse_accessor()?);
        }

        Ok(accessors)
    }

    fn parse_expression(&mut self) -> Result<Expression> {
        use TokenKind::*;

        let id = self.gen_node_id();
        let span: SourceSpan;

        let tok = expect_token!(
            "expression",
            self.tokens.next(),
            IntLiteral(_) | BoolLiteral(_) | Identifier(_) | ParenOpen
        )?;

        let kind = match tok.kind {
            IntLiteral(x) => {
                span = tok.span;
                ExpressionKind::IntLiteral(x)
            }
            BoolLiteral(x) => {
                span = tok.span;
                ExpressionKind::BoolLiteral(x)
            }
            Identifier(sym) => {
                span = tok.span;
                //let Identifier(sym) = expect_token!("", self.tokens.next(), Identifier(_))?.kind else {
                //    unreachable!()
                //};

                ExpressionKind::Identifier(sym)
            }
            ParenOpen => {
                macro_rules! builtin_operators {
                    () => {
                        KeywordOr
                            | KeywordAnd
                            | KeywordXor
                            | KeywordBand
                            | KeywordBor
                            | Star
                            | Div
                            | KeywordRem
                            | KeywordEq
                            | KeywordNe
                            | KeywordLt
                            | KeywordGt
                            | KeywordLe
                            | KeywordGe
                            | KeywordNot
                            | Minus
                            | Plus
                            | Ampersand
                    };
                }

                let start_span = tok.span;

                let tok = expect_token!(
                    "expression call",
                    self.tokens.peek().copied(),
                    builtin_operators!() | KeywordCast | At | Identifier(_)
                )?;

                if matches!(tok.kind, builtin_operators!() | KeywordCast | At) {
                    self.tokens.next();
                }

                let e = match tok.kind {
                    KeywordCast => {
                        let ty = self.parse_type()?;
                        let e = self.parse_expression()?.into();
                        ExpressionKind::Cast { ty, e }
                    }

                    At => {
                        let base = self.parse_expression()?.into();
                        let accessors = self.parse_accessors()?;
                        ExpressionKind::Access { base, accessors }
                    }

                    // Builtin operator call
                    op @ builtin_operators!() => {
                        let op = match op {
                            KeywordEq => BuiltinOpKind::Equals,
                            KeywordNe => BuiltinOpKind::NotEquals,
                            KeywordGt => BuiltinOpKind::GreaterThan,
                            KeywordLt => BuiltinOpKind::LessThan,
                            KeywordGe => BuiltinOpKind::GreaterEquals,
                            KeywordLe => BuiltinOpKind::LessEquals,
                            Star => BuiltinOpKind::Mul,
                            Div => BuiltinOpKind::Div,
                            KeywordBor => BuiltinOpKind::BitwiseOr,
                            KeywordBand => BuiltinOpKind::BitwiseAnd,
                            KeywordAnd => BuiltinOpKind::And,
                            KeywordOr => BuiltinOpKind::Or,
                            KeywordNot => BuiltinOpKind::Not,
                            KeywordXor => BuiltinOpKind::Xor,
                            KeywordRem => BuiltinOpKind::Remainder,
                            Minus => BuiltinOpKind::Sub,
                            Plus => BuiltinOpKind::Add,
                            Ampersand => BuiltinOpKind::AddressOf,
                            _ => panic!(),
                        };

                        ExpressionKind::BuiltinOp {
                            op,
                            args: self.parse_arguments()?,
                        }
                    }

                    // Function call
                    Identifier(sym) => ExpressionKind::Call {
                        name: self.parse_name()?,
                        args: self.parse_arguments()?,
                    },

                    _ => todo!("{:?}", tok.kind),
                };

                let end_token = expect_token!(
                    "closing parenthesis in expression",
                    self.tokens.next(),
                    ParenClose
                )?;

                span = start_span.extend(&end_token.span);

                e
            }

            _ => todo!("Unexpected token: report error"),
        };

        // TODO register the span for this expression
        self.spans.insert(id, span);

        Ok(Expression { id, kind })
    }
}
