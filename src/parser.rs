use crate::ast::*;
use crate::source_location::SourceSpan;
use crate::string_interner::StringInterner;
use crate::tokenizer::{TokenKind, Tokenizer};
use std::collections::HashMap;
use std::iter::Peekable;

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
                Err(ParseError::UnexpectedToken(tok.span, tok.kind.into()))
            } else {
                Ok(tok)
            }
        } else {
            Err(ParseError::UnexpectedEnd)
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
        let Identifier(sym) = name_token.kind else { unreachable!() };

        let id = self.id_generator.gen_id();

        let span = name_token.span;

        self.spans.insert(id, span);

        Ok(Name { id, sym })
    }

    fn parse_item(&mut self) -> Result<Item> {
        use TokenKind::*;

        let fn_keyword = expect_token!(self.tokens.next(), KeywordFn)?;

        let name = self.parse_name()?;

        let params = self.parse_params()?;

        expect_token!(self.tokens.next(), Arrow)?;

        let return_type = self.parse_type()?;

        let body: Box<Block> = self.parse_block()?.into();

        let span = fn_keyword.span.extend(self.spans.get(&body.id).unwrap());

        let id = self.id_generator.gen_id();

        self.spans.insert(id, span);

        let item = Item {
            id,
            kind: ItemKind::Function {
                return_type,
                name,
                params,
                body,
            },
        };

        Ok(item)
    }

    fn parse_type(&mut self) -> Result<Type> {
        use TokenKind::*;

        let ty = expect_token!(
            self.tokens.next(),
            Identifier(_) | KeywordInt | KeywordVoid | KeywordBool
        )?;

        let kind = match ty.kind {
            KeywordInt => TypeKind::Int,
            KeywordVoid => TypeKind::Void,
            KeywordBool => TypeKind::Bool,
            _ => unreachable!(),
        };

        let id = self.gen_node_id();
        let span = ty.span;

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

        let open_brace = expect_token!(self.tokens.next(), BraceOpen)?;

        let mut statements = Vec::new();

        loop {
            let peeked = expect_token!(self.tokens.peek(), _)?;
            if matches!(peeked.kind, BraceClose) {
                break;
            }

            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        let close_brace = expect_token!(self.tokens.next(), BraceClose)?;

        let id = self.gen_node_id();
        let span = open_brace.span.extend(&close_brace.span);

        self.spans.insert(id, span);

        Ok(Block { id, statements })
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        use StatementKind::*;
        use TokenKind::*;

        let id = self.gen_node_id();
        let span: SourceSpan;

        let tok = *expect_token!(self.tokens.peek(), _)?;

        if matches!(
            tok.kind,
            KeywordLet | KeywordLoop | KeywordIf | KeywordBreak | KeywordContinue | KeywordReturn
        ) {
            self.tokens.next();
        }

        let kind = match tok.kind {
            KeywordLet => {
                let name = self.parse_name()?;

                expect_token!(self.tokens.next(), Colon)?;

                let ty = self.parse_type()?;

                expect_token!(self.tokens.next(), Equals)?;

                let init: Box<_> = self.parse_expression()?.into();

                span = tok.span.extend(self.spans.get(&init.id).unwrap());

                Let { name, ty, init }
            }
            KeywordIf => {
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
                span = tok.span;
                Break
            }
            KeywordContinue => {
                span = tok.span;
                Continue
            }
            KeywordReturn => {
                let tok2 = expect_token!(self.tokens.peek(), _)?;
                if matches!(tok2.kind, Semicolon) {
                    span = tok.span;
                    Return(None)
                } else {
                    let return_value = self.parse_expression()?;
                    span = tok.span.extend(self.spans.get(&return_value.id).unwrap());
                    Return(Some(return_value.into()))
                }
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

    fn parse_expression(&mut self) -> Result<Expr> {
        use TokenKind::*;

        let id = self.gen_node_id();
        let span: SourceSpan;

        let tok = expect_token!(self.tokens.next(), _)?;

        let kind = match tok.kind {
            IntLiteral(x) => {
                span = tok.span;
                ExprKind::IntLiteral(x)
            }
            BoolLiteral(x) => {
                span = tok.span;
                ExprKind::BoolLiteral(x)
            }
            Identifier(sym) => todo!(),
            ParenOpen => {
                let start_span = tok.span;

                let tok = expect_token!(self.tokens.next(), _)?;

                let e = match tok.kind {
                    KeywordCast => {
                        let ty = self.parse_type()?;
                        let e = self.parse_expression()?;
                        ExprKind::Cast { ty, e: e.into() }
                    }

                    // Builtin operators that always need two operands
                    op @ (KeywordOr | KeywordAnd | KeywordXor | Star | Div | Equal | NotEqual
                    | Less | Greater | LessEqual | GreaterEqual) => {
                        let x = self.parse_expression()?;
                        let y = self.parse_expression()?;

                        let op = match op {
                            Equal => BuiltinOp::Equals,
                            _ => todo!(),
                        };

                        ExprKind::BuiltinOp {
                            op,
                            args: vec![x, y],
                        }
                    }

                    KeywordNot => {
                        let x = self.parse_expression()?;

                        ExprKind::BuiltinOp {
                            op: BuiltinOp::Not,
                            args: vec![x],
                        }
                    }

                    op @ (Minus | Plus) => {
                        let x = self.parse_expression()?;

                        let op = match op {
                            Minus => BuiltinOp::Sub,
                            Plus => BuiltinOp::Add,
                            _ => unreachable!(),
                        };

                        let peeked = expect_token!(self.tokens.peek(), _)?;
                        if matches!(peeked.kind, ParenClose) {
                            ExprKind::BuiltinOp { op, args: vec![x] }
                        } else {
                            let y = self.parse_expression()?;
                            ExprKind::BuiltinOp {
                                op,
                                args: vec![x, y],
                            }
                        }
                    }

                    // Function call
                    Identifier(sym) => {
                        todo!("Function calls are not implemented yet")
                    }

                    _ => todo!(),
                };

                let end_token = expect_token!(self.tokens.next(), ParenClose)?;

                span = start_span.extend(&end_token.span);

                e
            }
            _ => todo!(),
        };

        // TODO register the span for this expression
        self.spans.insert(id, span);

        Ok(Expr { id, kind })
    }

    /*
    fn parse_expression_element(&mut self) -> Result<Expr> {
        use TokenKind::*;

        let tok = expect_token!(self.tokens.next(), _)?;

        if matches!(tok.kind, ParenOpen) {
            let inner = self.parse_expression()?;
            expect_token!(self.tokens.next(), ParenClose)?;
            return Ok(inner);
        }

        let id = self.gen_node_id();
        let span: SourceSpan;

        let kind = match tok.kind {
            Minus => {
                let e = self.parse_expression()?.into();
                span = tok.span;
                ExprKind::UnaryOp {
                    op: UnaryOpKind::Negate,
                    e,
                }
            }
            IntLiteral(x) => {
                span = tok.span;
                ExprKind::IntLiteral(x)
            }
            BoolLiteral(x) => {
                span = tok.span;
                ExprKind::BoolLiteral(x)
            }
            KeywordCast => {
                expect_token!(self.tokens.next(), ParenOpen)?;

                let ty = self.parse_type()?;

                expect_token!(self.tokens.next(), Comma)?;

                let e = self.parse_expression()?;

                let last_token = expect_token!(self.tokens.next(), ParenClose)?;

                span = tok.span.extend(&last_token.span);

                ExprKind::Cast { ty, e: e.into() }
            }
            Identifier(sym) => {
                if let Some(Token {
                    kind: ParenOpen, ..
                }) = self.tokens.peek()
                {
                    self.tokens.next(); // Eat the `(`

                    let name = {
                        let id = self.id_generator.gen_id();
                        let span = tok.span;

                        self.spans.insert(id, span);

                        Name { id, sym }
                    };

                    let mut args = Vec::new();
                    loop {
                        let peeked = expect_token!(self.tokens.peek(), _)?;
                        if matches!(peeked.kind, ParenClose) {
                            span = tok.span.extend(&peeked.span);
                            self.tokens.next();
                            break;
                        }

                        let arg = self.parse_expression()?;
                        args.push(arg);

                        let peeked = expect_token!(self.tokens.peek(), Comma | ParenClose)?;
                        if matches!(peeked.kind, Comma) {
                            self.tokens.next();
                        }
                    }

                    ExprKind::Call { name, args }
                } else {
                    span = tok.span;
                    ExprKind::Identifier(sym)
                }
            }

            _ => todo!(),
        };

        self.spans.insert(id, span);

        Ok(Expr { id, kind })
    }

    fn parse_binop_expression(&mut self, precedence: u32, mut lhs: Expr) -> Result<Expr> {
        loop {
            let Some(&tok) = self.tokens.peek() else { return Ok(lhs); };

            let Some((op_precedence, op)) = self.binary_ops.get(&tok.kind) else {
                return Ok(lhs);
            };

            let (op_precedence, op) = (*op_precedence, *op);

            // Eat the operator
            self.tokens.next();

            if op_precedence < precedence {
                return Ok(lhs);
            }

            let mut rhs = self.parse_expression_element()?;

            // Check if we have another operator
            let peeked = self.tokens.peek();
            let has_next_operator = match peeked {
                Some(tok) if self.binary_ops.get(&tok.kind).is_some() => true,
                _ => false,
            };

            // There is no next token or the next token is not an operator, so we are done.
            if !has_next_operator {
                let id = self.gen_node_id();
                self.spans.insert(id, tok.span);
                return Ok(Expr {
                    id,
                    kind: ExprKind::BinaryOp {
                        op,
                        lhs: lhs.into(),
                        rhs: rhs.into(),
                    },
                });
            };

            // We previously already found out that there is another token and that the token is
            // a binary operator.
            let Some(peeked) = self.tokens.peek() else { unreachable!() };
            let Some((next_op_precedence, _)) = self.binary_ops.get(&peeked.kind) else { unreachable!() };

            let next_op_precedence = *next_op_precedence;

            if next_op_precedence > op_precedence {
                rhs = self.parse_binop_expression(next_op_precedence, rhs)?;
            }

            let id = self.gen_node_id();
            self.spans.insert(id, tok.span);
            lhs = Expr {
                id,
                kind: ExprKind::BinaryOp {
                    op,
                    lhs: lhs.into(),
                    rhs: rhs.into(),
                },
            };
        }
    }
    */
}

mod tests {}
