use crate::ast;
use crate::env::Env;
use std::convert::{TryFrom, TryInto};

pub struct CodeGenerator<'a> {
    ctx: &'a jello::Context,
    builder: jello::Builder<'a>,
    module: jello::Module<'a>,

    env: Env<jello::AnyValue>,

    current_function: Option<jello::FunctionValue>,

    i64_t: jello::IntType,
    i1_t: jello::IntType,
}

#[derive(Debug)]
pub enum CodeGenError {
    NameNotFound(String),
}

impl std::fmt::Display for CodeGenError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for CodeGenError {}

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

impl<'a> CodeGenerator<'a> {
    pub fn new(ctx: &'a jello::Context) -> CodeGenerator<'a> {
        CodeGenerator {
            ctx,
            builder: ctx.create_builder(),
            module: ctx.create_module("main"),
            env: Env::new(),
            current_function: None,
            i64_t: ctx.i64_type(),
            i1_t: ctx.i1_type(),
        }
    }

    fn gen_item(&mut self, item: &ast::Item) -> Result<jello::AnyValue> {
        match item {
            ast::Item::Function { name, body, params } => {
                let f: jello::FunctionValue =
                    self.env.find(name).unwrap().try_into()?;

                self.env.push_scope();
                self.current_function = Some(f);

                for (i, param) in params.iter().enumerate() {
                    self.env.insert(param, f.nth_param(i as u32).unwrap());
                }

                let entry = self.ctx.append_basic_block(&f, "entry");
                self.builder.position_at_end(entry);
                let ret = self.gen_expr(body)?.try_into()?;
                self.builder.build_ret(ret);

                self.current_function = None;
                self.env.pop_scope();

                Ok(f.into())
            }
        }
    }

    fn gen_expr(&mut self, e: &ast::Expr) -> Result<jello::AnyValue> {
        use ast::{Expr::*, Op::*};

        match e {
            LiteralInt(x) => Ok(self.i64_t.const_int(*x as u64, false).into()),
            Call(name, args) => {
                let f = self
                    .env
                    .find(name)
                    .ok_or_else(|| CodeGenError::NameNotFound(name.clone()))?
                    .try_into()?;

                let xs = args
                    .iter()
                    .map(|x| self.gen_expr(x))
                    .collect::<Result<Vec<_>>>()?;

                Ok(self.builder.build_call(f, &xs))
            }
            If {
                cond,
                then,
                otherwise: None,
            } => {
                let cond_val = self.gen_expr(cond)?;

                let then_block = self.ctx.append_basic_block(
                    &self.current_function.unwrap(),
                    "then",
                );

                let end_block = self
                    .ctx
                    .append_basic_block(&self.current_function.unwrap(), "end");

                self.builder.build_cond_br(
                    cond_val.try_into()?,
                    then_block,
                    end_block,
                );

                self.builder.position_at_end(then_block);
                self.gen_expr(then)?;
                self.builder.build_br(end_block);

                self.builder.position_at_end(end_block);
                Ok(jello::AnyValue::Void)
            }
            If {
                cond,
                then,
                otherwise: Some(otherwise),
            } => {
                let cond_val = self.gen_expr(cond)?;

                let res = self.builder.build_alloca(self.i64_t.into());

                let then_block = self.ctx.append_basic_block(
                    &self.current_function.unwrap(),
                    "then",
                );

                let otherwise_block = self.ctx.append_basic_block(
                    &self.current_function.unwrap(),
                    "else",
                );

                let end_block = self
                    .ctx
                    .append_basic_block(&self.current_function.unwrap(), "end");

                self.builder.build_cond_br(
                    cond_val.try_into()?,
                    then_block,
                    otherwise_block,
                );

                self.builder.position_at_end(then_block);
                let then_val = self.gen_expr(then)?;
                self.builder.build_store(then_val, res);
                self.builder.build_br(end_block);

                self.builder.position_at_end(otherwise_block);
                let otherwise_val = self.gen_expr(otherwise)?;
                self.builder.build_store(otherwise_val, res);
                self.builder.build_br(end_block);

                self.builder.position_at_end(end_block);
                Ok(self.builder.build_load(res))
            }
            Identifier(name) => {
                self.env.find(name).copied().ok_or_else(|| {
                    CodeGenError::NameNotFound(name.clone()).into()
                })
            }
            Builtin(op, params) => {
                let xs = params
                    .iter()
                    .map(|x| self.gen_expr(x))
                    .collect::<Result<Vec<jello::AnyValue>>>()?;

                match op {
                    Add => {
                        let xs: Vec<_> = xs
                            .iter()
                            .map(jello::IntValue::try_from)
                            .collect::<std::result::Result<_, _>>()?;

                        Ok(xs
                            .into_iter()
                            .reduce(|acc, x| self.builder.build_add(acc, x))
                            .unwrap()
                            .into())
                    }
                    GreaterThan => {
                        let xs: Vec<_> = xs
                            .iter()
                            .map(jello::IntValue::try_from)
                            .collect::<std::result::Result<_, _>>()?;

                        Ok(xs
                            .windows(2)
                            .map(|x| {
                                self.builder.build_cmp(
                                    x[0],
                                    x[1],
                                    jello::IntComparison::SignedGreaterThan,
                                )
                            })
                            .into_iter()
                            .reduce(|acc, x| self.builder.build_and(acc, x))
                            .unwrap()
                            .into())
                    }
                }
            }
        }
    }

    pub fn compile(
        &mut self,
        items: &[ast::Item],
        outfile: &str,
    ) -> Result<()> {
        self.env.push_scope();

        for i in items {
            match i {
                ast::Item::Function { name, params, .. } => {
                    let f = self.module.add_function(
                        name,
                        &self.ctx.function_type(
                            &jello::FunctionTypeDescriptor {
                                ret_type: self.i64_t.into(),
                                params: &params
                                    .iter()
                                    .map(|_| self.i64_t.into())
                                    .collect::<Vec<_>>(),
                            },
                        ),
                    );

                    self.env.insert(name, f.into());
                }
            }
        }

        for i in items {
            self.gen_item(i)?;
        }

        self.env.pop_scope();

        let passes = jello::PassManager::create();
        passes.add_mem_to_reg();
        passes.run(&self.module);

        self.module.print_to_file(outfile);

        Ok(())
    }
}

// Old code generation code.
/*
struct CodeGenerator<'a> {
    ctx: &'a jello::Context,
    builder: jello::Builder<'a>,
    module: jello::Module<'a>,

    current_function: Option<jello::FunctionValue>,

    i64_type: jello::IntType,
    i1_type: jello::IntType,
}

impl<'a> CodeGenerator<'a> {
    fn create(ctx: &'a jello::Context) -> CodeGenerator<'a> {
        CodeGenerator {
            ctx,
            builder: ctx.create_builder(),
            module: ctx.create_module("main"),
            current_function: None,
            i64_type: ctx.i64_type(),
            i1_type: ctx.i1_type(),
        }
    }

    fn gen_function_code(
        &mut self,
        f: jello::FunctionValue,
        def: &ast::Function,
        env: &Env<jello::AnyValue>,
    ) -> Result<jello::FunctionValue, Box<dyn Error>> {
        let ast::Function { params, body, .. } = def;
        let mut param_env = Env::new(Some(env));

        for (i, p) in params.iter().enumerate() {
            param_env.insert(
                p.name.to_string(),
                f.nth_param(i.try_into()?).unwrap(),
            );
        }

        self.current_function = Some(f);

        let entry = self.ctx.append_basic_block(&f, "entry");
        self.builder.position_at_end(entry);
        let ret = self.gen_code_base(&body, &param_env)?.try_into()?;
        self.builder.build_ret(ret);

        self.current_function = None;

        Ok(f)
    }

    fn gen_code_base(
        &mut self,
        expr: &ast::Expr,
        env: &Env<jello::AnyValue>,
    ) -> Result<jello::AnyValue, Box<dyn Error>> {
        use ast::ExprKind;

        let res = match &expr.kind {
            ExprKind::Identifier(id) => *env.get(&id).unwrap(),
            ExprKind::Call(fname, args) => {
                let f = env.get(&fname).unwrap();
                let mut arg_vals = Vec::new();

                for arg in args {
                    arg_vals.push(self.gen_code_base(&arg, env)?);
                }

                self.builder.build_call(f.try_into().unwrap(), &arg_vals)
            }

            ExprKind::BinOp(op, left, right) => {
                let lhs = self.gen_code_base(&left, env)?;
                let rhs = self.gen_code_base(&right, env)?;

                match op {
                    ast::BinOp::Add => self
                        .builder
                        .build_add(lhs.try_into()?, rhs.try_into()?)
                        .into(),
                    ast::BinOp::Sub => self
                        .builder
                        .build_sub(lhs.try_into()?, rhs.try_into()?)
                        .into(),
                    ast::BinOp::Mul => self
                        .builder
                        .build_mul(lhs.try_into()?, rhs.try_into()?)
                        .into(),
                    ast::BinOp::LE => self
                        .builder
                        .build_cmp(
                            lhs.try_into()?,
                            rhs.try_into()?,
                            jello::IntComparison::SLE,
                        )
                        .into(),
                    _ => todo!(),
                }
            }
            ExprKind::LiteralInt(x) => {
                self.i64_type.const_int(*x as u64, false).into()
            }
            ExprKind::LiteralBool(b) => {
                self.i1_type.const_int(if *b { 1 } else { 0 }, false).into()
            }

            ExprKind::If {
                cond,
                then,
                otherwise,
            } => {
                let cond_val = self.gen_code_base(&cond, env)?;

                let res = self.builder.build_alloca(self.i64_type.into());

                let then_block = self.ctx.append_basic_block(
                    &self.current_function.unwrap(),
                    "then",
                );

                let otherwise_block = self.ctx.append_basic_block(
                    &self.current_function.unwrap(),
                    "else",
                );

                let end_block = self
                    .ctx
                    .append_basic_block(&self.current_function.unwrap(), "end");

                self.builder.build_cond_br(
                    cond_val.try_into()?,
                    then_block,
                    otherwise_block,
                );

                self.builder.position_at_end(then_block);
                let then_val = self.gen_code_base(&then, env)?;
                self.builder.build_store(then_val, res);
                self.builder.build_br(end_block);

                self.builder.position_at_end(otherwise_block);
                let otherwise_val = self.gen_code_base(&otherwise, env)?;
                self.builder.build_store(otherwise_val, res);
                self.builder.build_br(end_block);

                self.builder.position_at_end(end_block);
                self.builder.build_load(res)
            }
        };

        Ok(res)
    }

    fn compile(
        &mut self,
        toplevel: &ast::Toplevel,
    ) -> Result<(), Box<dyn Error>> {
        let mut toplevel_functions: Env<jello::AnyValue> = Env::new(None);

        for f in &toplevel.functions {
            let ast::Function { name, params, .. } = f;

            let mut param_types = Vec::<jello::AnyType>::new();

            for _ in 0..params.len() {
                param_types.push(self.i64_type.into());
            }

            toplevel_functions.insert(
                f.name.to_string(),
                self.module
                    .add_function(
                        &name,
                        &self.ctx.function_type(
                            &jello::FunctionTypeDescriptor {
                                ret_type: self.i64_type.into(),
                                params: &param_types,
                            },
                        ),
                    )
                    .into(),
            );
        }

        for f in &toplevel.functions {
            self.gen_function_code(
                toplevel_functions.get(&f.name).unwrap().try_into().unwrap(),
                &f,
                &toplevel_functions,
            )?;
        }

        let passes = jello::PassManager::create();
        passes.add_mem_to_reg();
        passes.run(&self.module);

        self.module.print_to_file("out.ll");

        Ok(())
    }
}
*/
