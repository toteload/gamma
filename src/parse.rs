use crate::ast::*;
use crate::sexpr::{self, SExpr};

/*
#[derive(Debug)]
enum ParseError<'a> {
    ExpectedToken(TokenKind<'a>),
    ExpectedIdentifier,
    ExpectedLiteralInt,
}

impl<'a> std::fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl<'a> std::error::Error for ParseError<'a> {}

fn expect_token<'a>(
    tokenizer: &mut Tokenizer<'a>,
    tok: TokenKind<'a>,
) -> Result<Token<'a>, ParseError<'a>> {
    let cur = tokenizer.next();
    if let Some(t) = cur {
        if t.kind == tok {
            return Ok(t);
        }
    }

    Err(ParseError::ExpectedToken(tok))
}

fn expect_identifier<'a>(
    tokenizer: &mut Tokenizer<'a>,
) -> Result<Token<'a>, ParseError<'a>> {
    let cur = tokenizer.next();
    if let Some(Token {
        kind: TokenKind::Identifier(_),
        ..
    }) = cur
    {
        Ok(cur.unwrap())
    } else {
        Err(ParseError::ExpectedIdentifier)
    }
}

fn expect_literal_int<'a>(
    tokenizer: &mut Tokenizer<'a>,
) -> Result<Token<'a>, ParseError<'a>> {
    let cur = tokenizer.next();
    if let Some(Token {
        kind: TokenKind::LiteralInt(_),
        ..
    }) = cur
    {
        Ok(cur.unwrap())
    } else {
        Err(ParseError::ExpectedLiteralInt)
    }
}

fn parse(mut tokenizer: Tokenizer) -> Result<AstNode, ParseError> {
    expect_token(&mut tokenizer, TokenKind::KeywordFn)?;

    let name = expect_identifier(&mut tokenizer)?;

    expect_token(&mut tokenizer, TokenKind::ParenOpen)?;
    expect_token(&mut tokenizer, TokenKind::ParenClose)?;
    expect_token(&mut tokenizer, TokenKind::BraceOpen)?;

    let val = expect_literal_int(&mut tokenizer)?
        .kind
        .as_str()
        .parse::<i64>()
        .unwrap();

    expect_token(&mut tokenizer, TokenKind::BraceClose)?;

    let ast = AstNode::Function(
        name.kind.as_str().to_string(),
        Box::new(AstNode::LiteralInt(val)),
    );

    Ok(ast)
}
*/
