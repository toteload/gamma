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
    i1_t: jello::IntType,

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
            i1_t: ctx.i1_type(),
            symbols,
        }
    }

    fn gen_item(
        &mut self,
        item: &Item,
        env: &Env<Symbol, jello::AnyValue>,
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

                for statement in &body.statements {
                    self.gen_statement(statement, &env)?;
                }

                self.current_function = None;

                Ok(f.into())
            }
        }
    }

    fn gen_statement(
        &mut self,
        stmt: &Statement,
        env: &Env<Symbol, jello::AnyValue>,
    ) -> Result<()> {
        match &stmt.kind {
            StatementKind::Return(Some(e)) => {
                let val = self.gen_expr(e, env)?.try_into()?;
                self.builder.build_ret(Some(val));
            }
            _ => todo!(),
        }

        Ok(())
    }

    fn gen_expr(
        &mut self,
        e: &Expr,
        env: &Env<Symbol, jello::AnyValue>,
    ) -> Result<jello::AnyValue> {
        match &e.kind {
            ExprKind::IntLiteral(x) => Ok(self.i64_t.const_int(*x as u64, false).into()),
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
            self.gen_item(item, &env)?;
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
