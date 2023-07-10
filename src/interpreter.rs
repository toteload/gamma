use crate::ast;
use crate::string_interner::Symbol;
use crate::env::Env;
use crate::types::Type;

#[derive(Clone, Debug)]
pub enum AnyValue {
    Void,
    Bool(bool),
    Int(i64),
}

#[derive(Clone, Debug)]
pub struct Param {
    name: Symbol,
    ty: Type,
}

#[derive(Clone, Debug)]
pub struct Function {
    params: Vec<Param>,
    return_type: Type,
    body: ast::StatementList,
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

type Result<T> = std::result::Result<T, EvalError>;

fn eval_call(f: &Function, env: &Env<AnyValue>, args: &[AnyValue]) -> Result<AnyValue> {
    let mut param_env = Env::new(Some(env));

    for (Param { name, .. }, val) in f.params.iter().zip(args.iter().cloned()) {
        param_env.insert(name, val);
    }

    eval_expr(&param_env, &f.body)
}

fn eval_expr(env: &Env<AnyValue>, e: &ast::Expr) -> Result<AnyValue> {
    todo!()
}

pub fn eval(items: &[ast::Item]) -> Result<AnyValue> {
    let mut env = Env::<AnyValue>::new(None);

    todo!()
}
