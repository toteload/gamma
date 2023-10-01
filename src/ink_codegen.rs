use crate::ast::*;
use crate::compiler::{Context as CompilerContext, PrintableError};
use crate::scope_stack::ScopeStack;
use crate::string_interner::{StringInterner, Symbol};
use crate::types::{Type, TypeInterner, TypeToken};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassManager,
    types::{BasicTypeEnum, IntType},
    values::{BasicValueEnum, FunctionValue, PointerValue},
    IntPredicate,
};
use std::collections::HashMap;

#[derive(Debug)]
pub enum CodegenError {
    InkwellError,
}

impl PrintableError for CodegenError {
    fn print(&self, context: &CompilerContext) {
        println!("ERROR: Codegen error...");
    }
}

#[derive(Debug, Clone, Copy)]
enum Variable<'ctx> {
    Stack(PointerValue<'ctx>),
    Parameter(BasicValueEnum<'ctx>),
}

pub struct CodeGenerator<'ctx> {
    ctx: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,

    current_function: Option<FunctionValue<'ctx>>,
    current_basic_block: Option<BasicBlock<'ctx>>,
    loop_end_blocks: Vec<BasicBlock<'ctx>>,

    functions: HashMap<Symbol, FunctionValue<'ctx>>,
    scopes: ScopeStack<Symbol, (BasicTypeEnum<'ctx>, Variable<'ctx>)>,

    i64_t: IntType<'ctx>,
    typetoken_to_inktype: HashMap<TypeToken, BasicTypeEnum<'ctx>>,

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
            current_basic_block: None,
            loop_end_blocks: Vec::new(),
            functions: HashMap::new(),
            scopes: ScopeStack::new(),

            i64_t: ctx.i64_type(),

            symbols,
            node_types,
            type_interner,

            typetoken_to_inktype: HashMap::new(),
        }
    }

    fn get_inktype_of_node(&mut self, id: NodeId) -> BasicTypeEnum<'ctx> {
        let Some(tok) = self.node_types.get(&id) else {
            panic!("Could not get type token of node")
        };

        if let Some(t) = self.typetoken_to_inktype.get(tok) {
            return *t;
        }

        let ty = self.type_interner.get(&tok);

        let t = match ty {
            Type::Bool | Type::Int => self.i64_t.into(),
            _ => todo!("Create LLVM type for internal type"),
        };

        self.typetoken_to_inktype.insert(*tok, t);

        t
    }

    fn add_basic_block(&self, name: &str) -> BasicBlock<'ctx> {
        self.ctx
            .insert_basic_block_after(self.current_basic_block.unwrap(), name)
    }

    fn position_builder_at_end_of(&mut self, basic_block: BasicBlock<'ctx>) {
        self.current_basic_block = Some(basic_block);
        self.builder.position_at_end(basic_block);
    }

    fn gen_item(&mut self, item: &Item) -> Result<(), CodegenError> {
        match &item.kind {
            ItemKind::Function {
                name, body, params, ..
            } => {
                let f: FunctionValue = *self
                    .functions
                    .get(&name.sym)
                    .expect("Function should be known");

                self.scopes.push_empty_scope();

                self.current_function = Some(f);

                for (i, param) in params.iter().enumerate() {
                    let val = f.get_nth_param(i as u32).unwrap();
                    self.scopes
                        .insert(param.name.sym, (val.get_type(), Variable::Parameter(val)));
                }

                let entry = self.ctx.append_basic_block(f, "entry");
                self.position_builder_at_end_of(entry);

                self.gen_block(&body)?;

                self.current_function = None;

                self.scopes.pop();

                Ok(())
            }
        }
    }

    fn gen_block(&mut self, block: &Block) -> Result<bool, CodegenError> {
        self.scopes.push_empty_scope();

        for statement in &block.statements {
            let has_terminator = self.gen_statement(statement)?;
            if has_terminator {
                return Ok(true);
            }
        }

        Ok(false)
    }

    fn gen_statement(&mut self, stmt: &Statement) -> Result<bool, CodegenError> {
        match &stmt.kind {
            StatementKind::Return(Some(e)) => {
                let val = self.gen_expr(e)?;
                self.builder.build_return(Some(&val));
            }
            StatementKind::If {
                cond,
                then,
                otherwise,
            } => {
                let cond_val = self.gen_expr(cond)?;

                let end_block = self.add_basic_block("endif");
                let otherwise_block = self.add_basic_block("otherwise");
                let then_block = self.add_basic_block("then");

                self.builder.build_conditional_branch(
                    cond_val.into_int_value(),
                    then_block,
                    otherwise_block,
                );

                let mut branch_terminator_count = 0u32;

                self.position_builder_at_end_of(then_block);
                let then_has_terminator = self.gen_block(then)?;

                if !then_has_terminator {
                    self.builder.build_unconditional_branch(end_block);
                    branch_terminator_count += 1;
                }

                self.position_builder_at_end_of(otherwise_block);
                if let Some(otherwise) = otherwise {
                    let otherwise_has_terminator = self.gen_block(otherwise)?;
                    if !otherwise_has_terminator {
                        self.builder.build_unconditional_branch(end_block);
                        branch_terminator_count += 1;
                    }
                } else {
                    self.builder.build_unconditional_branch(end_block);
                }

                if branch_terminator_count != 2 {
                    self.position_builder_at_end_of(end_block);
                } else {
                    let res = unsafe { end_block.delete() };
                    if let Err(_) = res {
                        todo!()
                    }
                }
            }
            StatementKind::Loop(body) => {
                let end_block = self.add_basic_block("endloop");
                let body_block = self.add_basic_block("loop");

                self.loop_end_blocks.push(end_block);

                self.builder.build_unconditional_branch(body_block);

                self.position_builder_at_end_of(body_block);

                self.gen_block(body)?;

                self.builder.build_unconditional_branch(body_block);

                self.loop_end_blocks.pop();

                self.position_builder_at_end_of(end_block);
            }
            StatementKind::Let { name, ty } => {
                let basic_type = self.get_inktype_of_node(ty.id);
                let ptr = self.builder.build_alloca(basic_type, "");

                self.scopes
                    .insert(name.sym, (basic_type, Variable::Stack(ptr)));
            }
            StatementKind::Set { dst, val } => {
                let p = self.get_dst_ptr(dst)?;
                let x = self.gen_expr(val)?;
                self.builder.build_store(p, x);
            }
            StatementKind::Break => {
                self.builder.build_unconditional_branch(
                    *self
                        .loop_end_blocks
                        .last()
                        .expect("There should be a basic block after a loop"),
                );
            }
            _ => todo!("Statement \"{:?}\"", stmt.kind),
        }

        let is_terminator = matches!(stmt.kind, StatementKind::Return(_) | StatementKind::Break);
        Ok(is_terminator)
    }

    fn get_dst_ptr(&self, e: &Expr) -> Result<PointerValue, CodegenError> {
        match &e.kind {
            ExprKind::Identifier(sym) => {
                let (ty, var) = self.scopes.get(&sym).expect("Identifier should exist");
                let Variable::Stack(ptr) = var else {
                    panic!("Tried to get dst of non-stack variable")
                };
                Ok(*ptr)
            }
            _ => todo!("Get pointer for expression"),
        }
    }

    fn gen_expr(&self, e: &Expr) -> Result<BasicValueEnum, CodegenError> {
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

                let val = self.gen_expr(src)?;

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
            ExprKind::BuiltinOp { op, args } => match op {
                BuiltinOpKind::Not => {
                    let x = self.gen_expr(&args[0])?;
                    Ok(self.builder.build_not(x.into_int_value(), "").into())
                }
                BuiltinOpKind::Equals => {
                    // For now only support two operands at a time
                    assert_eq!(args.len(), 2);

                    let x = self.gen_expr(&args[0])?;
                    let y = self.gen_expr(&args[1])?;
                    Ok(self
                        .builder
                        .build_int_compare(
                            IntPredicate::EQ,
                            x.into_int_value(),
                            y.into_int_value(),
                            "",
                        )
                        .into())
                }
                BuiltinOpKind::Add => {
                    assert!(args.len() >= 1);

                    let vals = args
                        .iter()
                        .map(|x| self.gen_expr(&x))
                        .collect::<Result<Vec<_>, _>>()?;

                    // The empty slice case is not covered by the destructuring. However, it cannot
                    // be empty, because we assert for that.
                    let [first, xs @ ..] = vals.as_slice() else {
                        unreachable!()
                    };

                    let sum_val = xs.iter().fold(first.into_int_value(), |acc, e| {
                        self.builder
                            .build_int_add(acc, e.into_int_value(), "")
                            .into()
                    });

                    Ok(sum_val.into())
                }
                BuiltinOpKind::Sub => {
                    assert!(args.len() >= 2);

                    let vals = args
                        .iter()
                        .map(|x| self.gen_expr(&x))
                        .collect::<Result<Vec<_>, _>>()?;

                    // The empty slice case is not covered by the destructuring. However, it cannot
                    // be empty, because we assert for that.
                    let [first, xs @ ..] = vals.as_slice() else {
                        unreachable!()
                    };

                    let sum_val = xs.iter().fold(first.into_int_value(), |acc, e| {
                        self.builder
                            .build_int_sub(acc, e.into_int_value(), "")
                            .into()
                    });

                    Ok(sum_val.into())
                }
                BuiltinOpKind::Mul => {
                    // For now only support two operands at a time
                    assert_eq!(args.len(), 2);

                    let x = self.gen_expr(&args[0])?;
                    let y = self.gen_expr(&args[1])?;
                    Ok(self
                        .builder
                        .build_int_mul(x.into_int_value(), y.into_int_value(), "")
                        .into())
                }
                BuiltinOpKind::Div => {
                    // For now only support two operands at a time
                    assert_eq!(args.len(), 2);

                    let x = self.gen_expr(&args[0])?;
                    let y = self.gen_expr(&args[1])?;
                    Ok(self
                        .builder
                        .build_int_signed_div(x.into_int_value(), y.into_int_value(), "")
                        .into())
                }
                _ => todo!("BuiltinOpKind \"{:?}\" not yet implemented", op),
            },
            ExprKind::Identifier(sym) => {
                let (ty, var) = *self.scopes.get(&sym).expect("Identifer should exist");
                match var {
                    Variable::Stack(ptr) => Ok(self.builder.build_load(ty, ptr, "")),
                    _ => todo!("Retrieve value from variable"),
                }
            }
            _ => todo!("{:?}", e.kind),
        }
    }

    pub fn compile(&mut self, items: &[Item], optimize: bool) -> Result<String, CodegenError> {
        self.scopes.push_empty_scope();

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

                    self.functions.insert(name.sym, f.into());
                }
            }
        }

        for item in items {
            self.gen_item(item)?;
        }

        self.scopes.pop();

        if let Err(s) = self.module.verify() {
            print!("{}", s.to_str().unwrap());
            print!("{}", self.module.to_string());
            todo!("Handle error properly")
        }

        if optimize {
            let pass_manager: PassManager<Module<'_>> = PassManager::create(());
            pass_manager.add_promote_memory_to_register_pass();
            pass_manager.run_on(&self.module);
        }

        /*
        if let Err(s) = self.module.print_to_file(Path::new("./out.ll")) {
            //return Err(anyhow!(s.to_string()));
            todo!()
        }
        */

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
