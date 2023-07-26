use crate::ast::*;
use crate::env::Env as GenericEnv;
use crate::string_interner::{StringInterner, Symbol};
use crate::types::{Type, TypeInterner, TypeToken};
use anyhow::{anyhow, Result};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassManager,
    types::IntType,
    values::{AnyValue, AnyValueEnum, BasicValueEnum, FunctionValue},
    IntPredicate,
};
use std::collections::HashMap;
use std::path::Path;

type Env<'a, 'ctx> = GenericEnv<'a, Symbol, AnyValueEnum<'ctx>>;

pub struct CodeGenerator<'ctx> {
    ctx: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    current_function: Option<FunctionValue<'ctx>>,

    i64_t: IntType<'ctx>,

    symbols: &'ctx StringInterner,
    node_types: &'ctx HashMap<NodeId, TypeToken>,
    type_interner: &'ctx TypeInterner,
}

impl<'ctx> CodeGenerator<'ctx> {
    pub fn new(
        ctx: &'ctx Context,
        symbols: &'ctx StringInterner,
        node_types: &'ctx HashMap<NodeId, TypeToken>,
        type_interner: &'ctx TypeInterner,
    ) -> Self {
        let module = ctx.create_module("main");

        Self {
            ctx,
            builder: ctx.create_builder(),
            module,
            current_function: None,

            i64_t: ctx.i64_type(),

            symbols,
            node_types,
            type_interner,
        }
    }

    fn add_basic_block(&self, name: &str) -> BasicBlock {
        self.ctx
            .append_basic_block(self.current_function.unwrap(), name)
    }

    fn gen_item<'a>(&mut self, env: &Env<'a, 'ctx>, item: &Item) -> Result<()>
    where
        'ctx: 'a,
    {
        match &item.kind {
            ItemKind::Function {
                name, body, params, ..
            } => {
                let f: FunctionValue = env.get(&name.sym).unwrap().into_function_value();

                let mut env = Env::new(Some(env));

                self.current_function = Some(f);

                for (i, param) in params.iter().enumerate() {
                    env.insert(
                        param.name.sym,
                        f.get_nth_param(i as u32).unwrap().as_any_value_enum(),
                    );
                }

                let entry = self.add_basic_block("entry");
                self.builder.position_at_end(entry);

                self.gen_block(&env, &body)?;

                self.current_function = None;

                Ok(())
            }
        }
    }

    fn gen_block(&self, env: &Env, block: &Block) -> Result<bool> {
        for statement in &block.statements {
            let is_return = self.gen_statement(&env, statement)?;
            if is_return {
                return Ok(true);
            }
        }

        Ok(false)
    }

    fn gen_statement(&self, env: &Env, stmt: &Statement) -> Result<bool> {
        match &stmt.kind {
            StatementKind::Return(Some(e)) => {
                let val = self.gen_expr(env, e)?;
                self.builder.build_return(Some(&val));
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

                self.builder.build_conditional_branch(
                    cond_val.into_int_value(),
                    then_block,
                    otherwise_block,
                );

                let mut branch_return_count = 0u32;

                self.builder.position_at_end(then_block);
                let contained_return = self.gen_block(env, then)?;

                if !contained_return {
                    self.builder.build_unconditional_branch(end_block);
                }

                branch_return_count += contained_return as u32;

                self.builder.position_at_end(otherwise_block);
                if let Some(otherwise) = otherwise {
                    let contained_return = self.gen_block(env, otherwise)?;
                    if !contained_return {
                        self.builder.build_unconditional_branch(end_block);
                    }

                    branch_return_count += contained_return as u32;
                } else {
                    self.builder.build_unconditional_branch(end_block);
                }

                if branch_return_count != 2 {
                    self.builder.position_at_end(end_block);
                } else {
                    let res = unsafe { end_block.delete() };
                    if let Err(_) = res {
                        return Err(anyhow!("Error deleting basic block"));
                    }
                }
            }
            StatementKind::Empty => (),
            _ => todo!(),
        }

        Ok(matches!(stmt.kind, StatementKind::Return(_)))
    }

    fn gen_expr(&self, env: &Env, e: &Expr) -> Result<BasicValueEnum> {
        match &e.kind {
            ExprKind::IntLiteral(x) => Ok(self.i64_t.const_int(*x as u64, false).into()),
            ExprKind::BoolLiteral(x) => {
                Ok(self.i64_t.const_int(if *x { 1 } else { 0 }, false).into())
            }
            ExprKind::Cast { e: src, .. } => {
                let src_ty_token = self.node_types.get(&src.id).unwrap();
                let src_ty = self.type_interner.get(&src_ty_token);

                let dst_ty_token = self.node_types.get(&e.id).unwrap();
                let dst_ty = self.type_interner.get(&dst_ty_token);

                let val = self.gen_expr(env, src)?;

                match (src_ty, dst_ty) {
                    (&Type::Int, &Type::Bool) => Ok(self
                        .builder
                        .build_int_compare(
                            IntPredicate::NE,
                            val.into_int_value(),
                            self.i64_t.const_int(0, false),
                            "",
                        )
                        .into()),
                    (&Type::Bool, &Type::Int) => Ok(self
                        .builder
                        .build_int_z_extend(val.into_int_value(), self.i64_t, "")
                        .into()),
                    _ => todo!(),
                }
            }
            ExprKind::BinaryOp { op, lhs, rhs } => {
                let lhs = self.gen_expr(env, lhs)?;
                let rhs = self.gen_expr(env, rhs)?;

                match op {
                    BinaryOpKind::Add => Ok(self
                        .builder
                        .build_int_add(lhs.into_int_value(), rhs.into_int_value(), "")
                        .into()),
                    BinaryOpKind::Sub => Ok(self
                        .builder
                        .build_int_sub(lhs.into_int_value(), rhs.into_int_value(), "")
                        .into()),
                    BinaryOpKind::Mul => Ok(self
                        .builder
                        .build_int_mul(lhs.into_int_value(), rhs.into_int_value(), "")
                        .into()),
                    BinaryOpKind::Div => todo!(),
                    BinaryOpKind::Equals => Ok(self
                        .builder
                        .build_int_compare(
                            IntPredicate::EQ,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "",
                        )
                        .into()),
                    BinaryOpKind::LogicalAnd | BinaryOpKind::BitwiseAnd => Ok(self
                        .builder
                        .build_and(lhs.into_int_value(), rhs.into_int_value(), "")
                        .into()),
                    BinaryOpKind::LogicalOr | BinaryOpKind::BitwiseOr => Ok(self
                        .builder
                        .build_or(lhs.into_int_value(), rhs.into_int_value(), "")
                        .into()),
                    BinaryOpKind::Xor => Ok(self
                        .builder
                        .build_xor(lhs.into_int_value(), rhs.into_int_value(), "")
                        .into()),
                    _ => todo!(),
                }
            }
            _ => todo!(),
        }
    }

    pub fn compile(&mut self, items: &[Item]) -> Result<String> {
        let mut env = Env::new(None);

        for item in items {
            match &item.kind {
                ItemKind::Function { name, params, .. } => {
                    let function_type = self.i64_t.fn_type(
                        &params.iter().map(|_| self.i64_t.into()).collect::<Vec<_>>(),
                        false,
                    );

                    let f = self.module.add_function(
                        self.symbols.get_str(name.sym),
                        function_type,
                        None,
                    );

                    env.insert(name.sym, f.into());
                }
            }
        }

        for item in items {
            self.gen_item(&env, item)?;
        }

        if let Err(s) = self.module.verify() {
            return Err(anyhow!(s.to_string()));
        }

        let pass_manager: PassManager<Module<'_>> = PassManager::create(());
        pass_manager.add_promote_memory_to_register_pass();
        pass_manager.run_on(&self.module);

        if let Err(s) = self.module.print_to_file(Path::new("./out.ll")) {
            return Err(anyhow!(s.to_string()));
        }

        /*
        Target::initialize_x86(&InitializationConfig::default());

        let target = Target::from_name("x86-64").unwrap();
        let target_machine = target.create_target_machine(
            &TargetTriple::create("x86_64-pc-windows-msvc"),
            "x86-64",
            "",
            OptimizationLevel::Default,
            RelocMode::Default,
            CodeModel::Default,
        ).unwrap();

        assert!(target_machine.write_to_file(&self.module, FileType::Object, Path::new("./out")).is_ok());
        */

        Ok(self.module.to_string())
    }
}
