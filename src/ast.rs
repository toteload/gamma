use crate::sexpr::{self, SExpr, SourceSpan};

#[derive(Clone, Debug)]
pub enum Op {
    Add,
    GreaterThan,
}

// Items appear at the toplevel
pub enum Item {
    Function {
        name: String,
        params: Vec<String>,
        body: Expr,
    },
}

// For the time being types in the AST can only be a string identifier like "int".
// In the future, when you want to support slices or arrays, this would probably
// have to be changed to something a little more complex, e.g. (slice int)
type Type = String;

pub struct Param {
    name: String,
    ty: Type,
}

pub struct Items {
    items: Vec<Item>,
    locs: Vec<SourceSpan>,
}

#[derive(Clone, Debug)]
pub enum Expr {
    LiteralInt(i64),
    Identifier(String),
    Builtin(Op, Vec<Expr>),
    Call(String, Vec<Expr>),
    If {
        cond: Box<Expr>,
        then: Box<Expr>,
        otherwise: Option<Box<Expr>>,
    },
}

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[derive(Debug)]
pub enum ParseError {
    IllegalItem,
    IllegalExpression,
    ExpectedAtom,
    ExpectedList,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for ParseError {}

pub fn parse(source: &str) -> Result<Vec<Item>> {
    use sexpr::SExprKind::*;

    let tree = sexpr::parse(source)?;

    if let SExpr {
        kind: List(items), ..
    } = tree
    {
        items.iter().map(parse_item).collect()
    } else {
        panic!("Oh oh");
    }
}

fn parse_type(tree: &SExpr) -> Result<Type> {
    todo!()
}

fn parse_param(tree: &SExpr) -> Result<Param> {
    use sexpr::SExprKind::*;

    let List(ref param) = tree.kind else { todo!() };

    todo!()
}

fn parse_params(tree: &SExpr) -> Result<Vec<String>> {
    use sexpr::SExprKind::*;

    let List(ref params) = tree.kind else { return Err(ParseError::ExpectedList.into()); };

    params
        .iter()
        .map(|x| match &x.kind {
            Atom(a) => Ok(a.to_string()),
            List(_) => Err(ParseError::ExpectedAtom.into()),
        })
        .collect()
}

fn parse_item(tree: &SExpr) -> Result<Item> {
    use sexpr::SExprKind::*;

    match &tree.kind {
        List(children) => {
            let kinds = children.iter().map(|x| &x.kind).collect::<Vec<_>>();

            match &kinds[..] {
                [Atom("fn"), Atom(name), List(_), Atom(return_type), body] => {
                    let body = parse_expr(&children[4])?;
                    let params = parse_params(&children[2])?;

                    Ok(Item::Function {
                        name: name.to_string(),
                        params,
                        body,
                    })
                }

                _ => todo!(),
            }
        }
        _ => Err(ParseError::IllegalItem.into()),
    }
}

fn parse_expr(tree: &SExpr) -> Result<Expr> {
    use sexpr::SExprKind::*;

    match &tree.kind {
        Atom(x) => {
            if let Ok(val) = x.parse::<i64>() {
                Ok(Expr::LiteralInt(val))
            } else {
                Ok(Expr::Identifier(x.to_string()))
            }
        }
        List(children) => {
            let kinds = children.iter().map(|x| &x.kind).collect::<Vec<_>>();

            match &kinds[..] {
                [Atom("+"), ..] => Ok(Expr::Builtin(
                    Op::Add,
                    children[1..]
                        .iter()
                        .map(|x| parse_expr(x))
                        .collect::<Result<Vec<_>>>()?,
                )),

                [Atom(name), ..] => {
                    let args = children[1..]
                        .iter()
                        .map(parse_expr)
                        .collect::<Result<Vec<_>>>()?;
                    Ok(Expr::Call(name.to_string(), args))
                }

                _ => todo!(),
            }
        }
    }

    /*
    match e.nodes[0] {
        Atom(x) => {
            if let Ok(val) = x.parse::<i64>() {
                Ok(Expr::LiteralInt(val))
            } else {
                Ok(Expr::Identifier(x.to_string()))
            }
        }
        List { .. } => match e.list_at(0).nodes {
            [Atom("+"), xs @ ..] => Ok(Expr::Builtin(
                Op::Add,
                (1..(1 + xs.len()))
                    .map(|x| parse_expr(&e.advance(x)))
                    .collect::<Result<Vec<_>>>()?,
            )),
            //[Atom(">"), xs @ ..] => Ok(Expr::Builtin(
            //    Op::GreaterThan,
            //    xs.iter().map(parse_expr).collect::<Result<_>>()?,
            //)),
            //[Atom("if"), cond, then, otherwise] => Ok(Expr::If {
            //    cond: parse_expr(cond)?.into(),
            //    then: parse_expr(then)?.into(),
            //    otherwise: Some(parse_expr(otherwise)?.into()),
            //}),
            //[Atom("if"), cond, then] => Ok(Expr::If {
            //    cond: parse_expr(cond)?.into(),
            //    then: parse_expr(then)?.into(),
            //    otherwise: None,
            //}),
            //[Atom(name), xs @ ..] => {
            //    let args = xs.iter().map(parse_expr).collect::<Result<_>>()?;
            //    Ok(Expr::Call(name.to_string(), args))
            //}
            _ => Err(ParseError::IllegalExpression.into()),
        },
    }
    */
}

#[cfg(test)]
mod parse_tests {
    use super::*;

    #[test]
    fn parse_main() {
        let res = parse("(fn main () (+ 12 24))");
        assert!(res.is_ok());
    }

    #[test]
    fn parse_bad_main() {
        assert!(parse("main").is_err());
    }
}
