use crate::ast;
use crate::env::Env;
use crate::types;

use types::Type;

use std::convert::{TryFrom, TryInto};

#[derive(Clone, Debug)]
pub enum AnyValue {
    Unit,
    Bool(bool),
    Int(i64),
    Function(FunctionValue),
}

#[derive(Clone, Debug)]
pub struct Param {
    name: String,
    ty: Type,
}

#[derive(Clone, Debug)]
pub struct FunctionValue {
    params: Vec<Param>,
    return_type: Type,
    body: ast::Expr,
}

impl From<FunctionValue> for AnyValue {
    fn from(f: FunctionValue) -> Self {
        AnyValue::Function(f)
    }
}

#[derive(Debug)]
pub enum ConversionError {
    AnyValueIsNotFunction,
}

impl std::fmt::Display for ConversionError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for ConversionError {}

impl TryFrom<AnyValue> for FunctionValue {
    type Error = ConversionError;

    fn try_from(val: AnyValue) -> std::result::Result<Self, Self::Error> {
        if let AnyValue::Function(v) = val {
            Ok(v)
        } else {
            Err(ConversionError::AnyValueIsNotFunction)
        }
    }
}

#[derive(Debug)]
pub enum EvalError {
    IdentifierNotFound,
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for EvalError {}

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn eval_call(f: &FunctionValue, env: &Env<AnyValue>, args: &[AnyValue]) -> Result<AnyValue> {
    let mut param_env = Env::new(Some(env));

    for (Param { name, .. }, val) in f.params.iter().zip(args.iter().cloned()) {
        param_env.insert(name, val);
    }

    eval_expr(&param_env, &f.body)
}

fn eval_expr(env: &Env<AnyValue>, e: &ast::Expr) -> Result<AnyValue> {
    use ast::Expr::*;

    match e {
        LiteralInt(x) => Ok(AnyValue::Int(*x)),
        Identifier(name) => env
            .get(&name)
            .ok_or(EvalError::IdentifierNotFound.into())
            .cloned(),
        Call(name, args) => {
            let f = env
                .get(name)
                .ok_or(EvalError::IdentifierNotFound)?
                .clone()
                .try_into()?;

            let args = args
                .iter()
                .map(|x| eval_expr(env, x))
                .collect::<Result<Vec<_>>>()?;

            eval_call(&f, env, &args)
        }
        If {
            cond,
            then,
            otherwise,
        } => {
            let cond_val = eval_expr(env, cond);
            todo!()
        }
        Builtin(ast::Op::Add, args) => {
            let mut acc = 0i64;
            for arg in args {
                let val = eval_expr(env, arg)?;
                match val {
                    AnyValue::Int(x) => acc += x,
                    _ => panic!("Should not happen if we verify correctly"),
                }
            }

            Ok(AnyValue::Int(acc))
        }
        _ => todo!(),
    }
}

pub fn eval(items: &[ast::Item]) -> Result<AnyValue> {
    let mut env = Env::<AnyValue>::new(None);

    for item in items {
        match item {
            ast::Item::Function { name, params, body } => {
                env.insert(
                    name,
                    FunctionValue {
                        params: params
                            .iter()
                            .cloned()
                            .map(|name| Param {
                                name,
                                ty: Type::Unit,
                            })
                            .collect(),
                        return_type: Type::Unit,
                        body: body.clone(),
                    }
                    .into(),
                );
            }
        }
    }

    let main_start = ast::Expr::Call("main".to_string(), Vec::new());

    eval_expr(&env, &main_start)
}

/*
use crate::ast;
use crate::ast::*;
use crate::env::Env;

use std::convert::{TryFrom, TryInto};
use std::error::Error;

struct Interpreter {
    env: Env<AnyValue>,
}

#[derive(Clone, Debug)]
pub enum AnyValue {
    Unit,
    Bool(bool),
    Int(i64),
    Function(FunctionValue),
}

#[derive(Clone, Debug)]
pub struct FunctionValue {
    params: Vec<String>,
    body: Block,
}

impl From<FunctionValue> for AnyValue {
    fn from(f: FunctionValue) -> Self {
        AnyValue::Function(f)
    }
}

#[derive(Debug)]
pub enum ConversionError {
    AnyValueIsNotFunction,
}

impl std::fmt::Display for ConversionError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for ConversionError {}

impl TryFrom<&AnyValue> for FunctionValue {
    type Error = ConversionError;

    fn try_from(val: &AnyValue) -> Result<Self, Self::Error> {
        if let AnyValue::Function(v) = val {
            Ok(v.clone())
        } else {
            Err(ConversionError::AnyValueIsNotFunction)
        }
    }
}

/*
type FunctionValueInner<'a> = &'a FunctionValue;

impl<'a> TryFrom<&'a AnyValue> for FunctionValueInner<'a> {
    type Error = ConversionError;

    fn try_from(val: &'a AnyValue) -> Result<Self, Self::Error> {
        if let AnyValue::Function(v) = val {
            Ok(v)
        } else {
            Err(ConversionError::AnyValueIsNotFunction)
        }
    }
}
*/

impl FunctionValue {
    fn call(
        &self,
        args: &[AnyValue],
        env: &mut Env<AnyValue>,
    ) -> Result<AnyValue, Box<dyn Error>> {
        env.push_scope();

        for (name, val) in self.params.iter().zip(args) {
            env.insert(name.to_string(), val.clone());
        }

        let res = interpret_block(&self.body, env);

        env.pop_scope();

        res
    }
}

impl std::error::Error for InterpretError {}

#[derive(Debug)]
enum InterpretError {
    IdentifierNotFound,
}

impl std::fmt::Display for InterpretError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub fn interpret_expr(
    e: &Expr,
    env: &mut Env<AnyValue>,
) -> Result<AnyValue, Box<dyn Error>> {
    let res = match &e.kind {
        ExprKind::Identifier(id) => env
            .get(&id)
            .cloned()
            .ok_or(InterpretError::IdentifierNotFound)?,
        ExprKind::BinOp(op, left, right) => {
            let lhs = interpret_expr(left, env)?;
            let rhs = interpret_expr(right, env)?;
            match op {
                ast::BinOp::Add => match (lhs, rhs) {
                    (AnyValue::Int(a), AnyValue::Int(b)) => {
                        AnyValue::Int(a + b)
                    }
                    _ => todo!(),
                },
                ast::BinOp::Mul => match (lhs, rhs) {
                    (AnyValue::Int(a), AnyValue::Int(b)) => {
                        AnyValue::Int(a * b)
                    }
                    _ => todo!(),
                },
                ast::BinOp::Sub => match (lhs, rhs) {
                    (AnyValue::Int(a), AnyValue::Int(b)) => {
                        AnyValue::Int(a - b)
                    }
                    _ => todo!(),
                },
                ast::BinOp::LE => match (lhs, rhs) {
                    (AnyValue::Int(a), AnyValue::Int(b)) => {
                        AnyValue::Bool(a <= b)
                    }
                    _ => todo!(),
                },
                _ => todo!(),
            }
        }
        ExprKind::Call(name, expr_args) => {
            let f: FunctionValue = env
                .get(name)
                .ok_or(InterpretError::IdentifierNotFound)?
                .try_into()?;
            let mut args = Vec::new();
            for e in expr_args {
                args.push(interpret_expr(e, env)?);
            }

            f.call(&args, env)?
        }
        ExprKind::If {
            cond,
            then,
            otherwise,
        } => {
            let cond_val = interpret_expr(&cond, env)?;
            if let AnyValue::Bool(b) = cond_val {
                if b {
                    interpret_expr(&then, env)?
                } else {
                    interpret_expr(&otherwise, env)?
                }
            } else {
                todo!();
            }
        }
        ExprKind::Loop(block) => {
            todo!()
        }
        ExprKind::Break => todo!(),
        ExprKind::LiteralBool(b) => AnyValue::Bool(*b),
        ExprKind::LiteralInt(i) => AnyValue::Int(*i),
    };

    Ok(res)
}

pub fn interpret_block(
    block: &Block,
    env: &mut Env<AnyValue>,
) -> Result<AnyValue, Box<dyn Error>> {
    // TODO This code will probably crash when there is an empty function body.

    let mut scope_count: usize = 0;

    for stmt in &block.stmts[..block.stmts.len() - 1] {
        match &stmt {
            Stmt::Let(name, _typ, init) => {
                env.push_scope();
                scope_count += 1;
                let init_val = interpret_expr(&init, env)?;
                env.insert(name.clone(), init_val);
            }
            Stmt::Assign(dst, val) => {
                *env.get_mut(&dst)
                    .ok_or(InterpretError::IdentifierNotFound)? =
                    interpret_expr(&val, env)?;
            }
            Stmt::Expr(e) => {
                interpret_expr(&e, env)?;
            }
        };
    }

    let res = match &block.stmts[block.stmts.len() - 1] {
        Stmt::Let(..) => todo!(),
        Stmt::Assign(..) => todo!(),
        Stmt::Expr(e) => interpret_expr(&e, env)?,
    };

    for i in 0..scope_count {
        env.pop_scope();
    }

    Ok(res)
}

pub fn interpret_toplevel(toplevel: &Toplevel) -> Env<AnyValue> {
    let mut env = Env::new();

    for Function {
        name, params, body, ..
    } in &toplevel.functions
    {
        env.insert(
            name.clone(),
            FunctionValue {
                params: params.iter().map(|x| &x.name).cloned().collect(),
                body: body.clone(),
            }
            .into(),
        );
    }

    env
}
*/
