use crate::ast::*;
use crate::env::Env;
use crate::string_interner::{StringInterner, Symbol};
use std::convert::TryInto;

pub struct CodeGenerator<'a> {
    ctx: &'a jello::Context,
    builder: jello::Builder<'a>,
    module: jello::Module<'a>,

    current_function: Option<jello::FunctionValue>,

    i64_t: jello::IntType,

    symbols: &'a StringInterner,
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
    pub fn new(ctx: &'a jello::Context, symbols: &'a StringInterner) -> CodeGenerator<'a> {
        CodeGenerator {
            ctx,
            builder: ctx.create_builder(),
            module: ctx.create_module("main"),
            current_function: None,
            i64_t: ctx.i64_type(),
            symbols,
        }
    }

    fn add_basic_block(&mut self, name: &str) -> jello::BasicBlock {
        self.ctx
            .append_basic_block(&self.current_function.unwrap(), name)
    }

    fn gen_item(
        &mut self,
        env: &Env<Symbol, jello::AnyValue>,
        item: &Item,
    ) -> Result<jello::AnyValue> {
        match &item.kind {
            ItemKind::Function {
                name, body, params, ..
            } => {
                let f: jello::FunctionValue = env.get(&name.sym).unwrap().try_into()?;

                let mut env = Env::new(Some(env));

                self.current_function = Some(f);

                for (i, param) in params.iter().enumerate() {
                    env.insert(param.name.sym, f.nth_param(i as u32).unwrap());
                }

                let entry = self.ctx.append_basic_block(&f, "entry");
                self.builder.position_at_end(entry);

                self.gen_block(&env, &body)?;

                self.current_function = None;

                Ok(f.into())
            }
        }
    }

    fn gen_block(&mut self, env: &Env<Symbol, jello::AnyValue>, block: &Block) -> Result<()> {
        for statement in &block.statements {
            self.gen_statement(&env, statement)?;
        }

        Ok(())
    }

    fn gen_statement(
        &mut self,
        env: &Env<Symbol, jello::AnyValue>,
        stmt: &Statement,
    ) -> Result<()> {
        match &stmt.kind {
            StatementKind::Return(Some(e)) => {
                let val = self.gen_expr(env, e)?.try_into()?;
                self.builder.build_ret(Some(val));
            }
            StatementKind::If {
                cond,
                then,
                otherwise,
            } => {
                let cond_val = self.gen_expr(env, cond)?;

                let then_block = self.add_basic_block("then");

                let otherwise_block = self.add_basic_block("else");

                let end_block = self.add_basic_block("end");

                self.builder
                    .build_cond_br(cond_val.try_into()?, then_block, otherwise_block);

                self.builder.position_at_end(then_block);
                self.gen_block(env, then)?;
                self.builder.build_br(end_block);

                self.builder.position_at_end(otherwise_block);
                if let Some(otherwise) = otherwise {
                    self.gen_block(env, otherwise)?;
                }
                self.builder.build_br(end_block);

                self.builder.position_at_end(end_block);
            }
            _ => todo!(),
        }

        Ok(())
    }

    fn gen_expr(
        &mut self,
        env: &Env<Symbol, jello::AnyValue>,
        e: &Expr,
    ) -> Result<jello::AnyValue> {
        match &e.kind {
            ExprKind::IntLiteral(x) => Ok(self.i64_t.const_int(*x as u64, false).into()),
            ExprKind::BoolLiteral(x) => Ok(self.i64_t.const_int(if *x { 1 } else { 0 }, false).into()),
            ExprKind::BinaryOp { op, lhs, rhs } => {
                let lhs = self.gen_expr(env, lhs)?;
                let rhs = self.gen_expr(env, rhs)?;

                match op {
                    BinaryOpKind::Add => Ok(self
                        .builder
                        .build_add(lhs.try_into()?, rhs.try_into()?)
                        .into()),
                    BinaryOpKind::Sub => Ok(self
                        .builder
                        .build_sub(lhs.try_into()?, rhs.try_into()?)
                        .into()),
                    BinaryOpKind::Mul => Ok(self
                        .builder
                        .build_mul(lhs.try_into()?, rhs.try_into()?)
                        .into()),

                    BinaryOpKind::Div => todo!(),

                    BinaryOpKind::Equals => Ok(self
                        .builder
                        .build_cmp(
                            lhs.try_into()?,
                            rhs.try_into()?,
                            jello::IntComparison::Equal,
                        )
                        .into()),

                    BinaryOpKind::LogicalAnd | BinaryOpKind::BitwiseAnd => Ok(self
                        .builder
                        .build_and(lhs.try_into()?, rhs.try_into()?)
                        .into()),

                    BinaryOpKind::LogicalOr | BinaryOpKind::BitwiseOr => Ok(self
                        .builder
                        .build_or(lhs.try_into()?, rhs.try_into()?)
                        .into()),

                    _ => todo!(),
                }
            }
            _ => todo!(),
            /*
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
            */
        }
    }

    pub fn compile(&mut self, items: &[Item], outfile: &str) -> Result<()> {
        let mut env = Env::new(None);

        for item in items {
            match &item.kind {
                ItemKind::Function { name, params, .. } => {
                    let f = self.module.add_function(
                        self.symbols.get_str(name.sym),
                        &self.ctx.function_type(&jello::FunctionTypeDescriptor {
                            ret_type: self.i64_t.into(),
                            params: &params.iter().map(|_| self.i64_t.into()).collect::<Vec<_>>(),
                        }),
                    );

                    env.insert(name.sym, f.into());
                }
            }
        }

        for item in items {
            self.gen_item(&env, item)?;
        }

        let passes = jello::PassManager::create();
        passes.add_mem_to_reg();
        passes.run(&self.module);

        self.module.print_to_file(outfile);

        Ok(())
    }

    pub fn compile_to_binary(&mut self, items: &[Item]) -> Result<()> {
        // LLVMTargetEmitToFile
        //
        // or
        //
        // LLVMMemoryBufferRef mem = LLVMWriteBitcodeToMemoryBuffer(module)
        // LLVMBinaryRef binary = LLVMCreateBinary(mem, context, error_message);
        //
        todo!()
    }
}
