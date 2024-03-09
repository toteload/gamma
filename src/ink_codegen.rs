use crate::ast::*;
use crate::error::Error;
use crate::string_interner::{StringInterner, Symbol};
use crate::types::{Signedness, Type, TypeInterner, TypeToken};
use inkwell::targets::TargetMachine;
use inkwell::types::PointerType;
use inkwell::{
    basic_block::BasicBlock,
    builder::{Builder, BuilderError},
    context::Context,
    intrinsics::Intrinsic,
    memory_buffer::MemoryBuffer,
    module::Module,
    passes::PassManager,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple},
    types::{BasicType, BasicTypeEnum, FunctionType, IntType, VoidType},
    values::{BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace, IntPredicate, OptimizationLevel,
};
use std::collections::HashMap;

impl From<BuilderError> for Error {
    fn from(e: BuilderError) -> Self {
        todo!()
    }
}

impl From<&str> for Error {
    fn from(e: &str) -> Self {
        todo!()
    }
}

#[derive(Debug, Clone, Copy)]
enum VariableValue<'ctx> {
    Stack(PointerValue<'ctx>),
    Parameter(BasicValueEnum<'ctx>),
}

#[derive(Clone, Copy)]
struct Variable<'ctx> {
    type_token: TypeToken,
    ty: BasicTypeEnum<'ctx>,
    val: VariableValue<'ctx>,
}

#[derive(Clone, Copy, PartialEq)]
enum ScopeKind {
    Block,
    Loop,
    Parameter,
    Function,
    Global,
}

struct Scope<'ctx> {
    kind: ScopeKind,
    stack_restorepoint: Option<BasicValueEnum<'ctx>>,
    variables: HashMap<Symbol, Variable<'ctx>>,
}

#[derive(Debug, Copy, Clone)]
pub enum MachineTarget {
    Windows,
    Macos,
}

struct AllocaTypeData<'ctx> {
    explicit_align: Option<u32>,
    basic_type: BasicTypeEnum<'ctx>,
    basic_type_with_align_padding: BasicTypeEnum<'ctx>,
}

pub struct CodeGenerator<'ctx> {
    ctx: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    target_machine: Option<TargetMachine>,

    current_function: Option<FunctionValue<'ctx>>,
    current_basic_block: Option<BasicBlock<'ctx>>,
    loop_end_blocks: Vec<BasicBlock<'ctx>>,

    functions: HashMap<Symbol, FunctionValue<'ctx>>,
    scopes: Vec<Scope<'ctx>>,

    llvm_stacksave: FunctionValue<'ctx>,
    llvm_stackrestore: FunctionValue<'ctx>,

    i8_t: IntType<'ctx>,
    i16_t: IntType<'ctx>,
    i32_t: IntType<'ctx>,
    i64_t: IntType<'ctx>,
    void_t: VoidType<'ctx>,
    ptr_t: PointerType<'ctx>,

    void_token: TypeToken,
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

        let llvm_stacksave = {
            let intrinsic =
                Intrinsic::find("llvm.stacksave").expect("llvm.stacksave should be present");
            intrinsic.get_declaration(&module, &[]).unwrap()
        };

        let llvm_stackrestore = {
            let intrinsic =
                Intrinsic::find("llvm.stackrestore").expect("llvm.stackrestore should be present");
            intrinsic.get_declaration(&module, &[]).unwrap()
        };

        let void_token = type_interner.get_for_type(&Type::Void).expect("");

        Self {
            ctx,
            builder: ctx.create_builder(),
            module,
            target_machine: None,

            current_function: None,
            current_basic_block: None,
            loop_end_blocks: Vec::new(),

            functions: HashMap::new(),
            scopes: Vec::new(),

            llvm_stacksave,
            llvm_stackrestore,

            i8_t: ctx.i8_type(),
            i16_t: ctx.i16_type(),
            i32_t: ctx.i32_type(),
            i64_t: ctx.i64_type(),
            void_t: ctx.void_type(),
            ptr_t: ctx.i8_type().ptr_type(AddressSpace::default()),

            void_token,

            symbols,
            node_types,
            type_interner,

            typetoken_to_inktype: HashMap::from([
                (
                    type_interner
                        .get_for_type(&Type::Int {
                            signedness: Signedness::Signed,
                            width: 8,
                        })
                        .unwrap(),
                    ctx.i8_type().into(),
                ),
                (
                    type_interner
                        .get_for_type(&Type::Int {
                            signedness: Signedness::Signed,
                            width: 16,
                        })
                        .unwrap(),
                    ctx.i16_type().into(),
                ),
                (
                    type_interner
                        .get_for_type(&Type::Int {
                            signedness: Signedness::Signed,
                            width: 32,
                        })
                        .unwrap(),
                    ctx.i32_type().into(),
                ),
                (
                    type_interner
                        .get_for_type(&Type::Int {
                            signedness: Signedness::Signed,
                            width: 64,
                        })
                        .unwrap(),
                    ctx.i64_type().into(),
                ),
                (
                    type_interner
                        .get_for_type(&Type::Int {
                            signedness: Signedness::Unsigned,
                            width: 8,
                        })
                        .unwrap(),
                    ctx.i8_type().into(),
                ),
                (
                    type_interner
                        .get_for_type(&Type::Int {
                            signedness: Signedness::Unsigned,
                            width: 16,
                        })
                        .unwrap(),
                    ctx.i16_type().into(),
                ),
                (
                    type_interner
                        .get_for_type(&Type::Int {
                            signedness: Signedness::Unsigned,
                            width: 32,
                        })
                        .unwrap(),
                    ctx.i32_type().into(),
                ),
                (
                    type_interner
                        .get_for_type(&Type::Int {
                            signedness: Signedness::Unsigned,
                            width: 64,
                        })
                        .unwrap(),
                    ctx.i64_type().into(),
                ),
                (
                    type_interner.get_for_type(&Type::Bool).unwrap(),
                    ctx.i8_type().into(),
                ),
            ]),
        }
    }

    fn int_type(&self, width: u32) -> IntType<'ctx> {
        match width {
            8 => self.i8_t,
            16 => self.i16_t,
            32 => self.i32_t,
            64 => self.i64_t,
            _ => panic!(),
        }
    }

    fn is_type_token_void_function(&self, token: TypeToken) -> bool {
        let ty = self.type_interner.get(&token);
        if let Type::Function { return_type, .. } = ty {
            return *return_type == self.void_token;
        }

        false
    }

    fn get_ink_function_type(&mut self, token: TypeToken) -> FunctionType<'ctx> {
        let Type::Function {
            return_type,
            params,
        } = self.type_interner.get(&token)
        else {
            panic!()
        };

        let params = params
            .iter()
            .map(|p| self.get_ink_basic_type(*p).into())
            .collect::<Vec<_>>();

        if *return_type == self.void_token {
            self.void_t.fn_type(params.as_slice(), false)
        } else {
            self.get_ink_basic_type(*return_type)
                .fn_type(params.as_slice(), false)
        }
    }

    fn get_ink_basic_type(&mut self, token: TypeToken) -> BasicTypeEnum<'ctx> {
        if let Some(t) = self.typetoken_to_inktype.get(&token) {
            return *t;
        }

        let ty = self.type_interner.get(&token);

        let t = match ty {
            Type::Pointer(_) => self.ptr_t.into(),
            Type::Array(size, inner) => {
                let t = self.get_ink_basic_type(*inner);
                assert!(*size < u32::MAX as i64);
                t.array_type(*size as u32).into()
            }
            _ => todo!("Create LLVM type for internal type: {:?}", ty),
        };

        self.typetoken_to_inktype.insert(token, t);

        t
    }

    fn get_inktype_of_node(&mut self, id: NodeId) -> BasicTypeEnum<'ctx> {
        let Some(type_token) = self.node_types.get(&id) else {
            panic!("Could not get type token of node")
        };

        self.get_ink_basic_type(*type_token)
    }

    fn get_alloca_type_data(&mut self, type_token: TypeToken) -> AllocaTypeData<'ctx> {
        if let Some(t) = self.typetoken_to_inktype.get(&type_token) {
            return AllocaTypeData {
                explicit_align: None,
                basic_type: *t,
                basic_type_with_align_padding: *t,
            };
        }

        let ty = self.type_interner.get(&type_token);

        match ty {
            Type::Pointer(_) => AllocaTypeData {
                explicit_align: None,
                basic_type: self.ptr_t.into(),
                basic_type_with_align_padding: self.ptr_t.into(),
            },
            Type::Array(size, inner) => {
                let AllocaTypeData {
                    explicit_align,
                    basic_type_with_align_padding: t,
                    ..
                }= self.get_alloca_type_data(*inner);

                assert!(*size < u32::MAX as i64);

                let t = t.array_type(*size as u32).into();

                AllocaTypeData {
                    explicit_align,
                    basic_type: t,
                    basic_type_with_align_padding: t,
                }
            }
            Type::Layout(_) => {
                let align = ty.align(self.type_interner);
                let byte_size = ty.byte_size(self.type_interner);
                let basic_type: BasicTypeEnum<'ctx> = self.i8_t.array_type(byte_size).into();
                let aligned_byte_size = byte_size.next_multiple_of(align);
                let basic_type_with_align_padding = if byte_size == aligned_byte_size {
                    basic_type.into()
                } else {
                    self.i8_t.array_type(aligned_byte_size).into()
                };

                AllocaTypeData {
                    explicit_align: Some(align),
                    basic_type,
                    basic_type_with_align_padding,
                }
            }
            _ => todo!("Create LLVM type for internal type: {:?}", ty),
        }
    }

    fn add_basic_block(&self, name: &str) -> BasicBlock<'ctx> {
        self.ctx
            .insert_basic_block_after(self.current_basic_block.unwrap(), name)
    }

    fn position_builder_at_end_of(&mut self, basic_block: BasicBlock<'ctx>) {
        self.current_basic_block = Some(basic_block);
        self.builder.position_at_end(basic_block);
    }

    fn add_to_current_scope(&mut self, name: Symbol, var: Variable<'ctx>) {
        self.scopes
            .last_mut()
            .expect("There should be a scope present")
            .variables
            .insert(name, var);
    }

    fn current_scope(&self) -> &Scope {
        self.scopes.last().expect("There should always be a scope")
    }

    fn get_variable(&self, name: &Symbol) -> Option<Variable<'ctx>> {
        for scope in self.scopes.iter().rev() {
            if let result @ Some(_) = scope.variables.get(name) {
                return result.copied();
            }
        }

        None
    }

    fn push_scope_with_stack_restore_point(&mut self, kind: ScopeKind) {
        let restore_point = self
            .builder
            .build_call(self.llvm_stacksave, &[], "")
            .unwrap()
            .try_as_basic_value()
            .left()
            .expect("llvm.stacksave should return a basic value");

        self.scopes.push(Scope {
            kind,
            stack_restorepoint: Some(restore_point),
            variables: HashMap::new(),
        });
    }

    fn gen_item(&mut self, item: &Item) -> Result<(), Error> {
        match &item.kind {
            ItemKind::Function {
                name, body, params, ..
            } => {
                let f: FunctionValue = *self
                    .functions
                    .get(&name.sym)
                    .expect("Function should be known");

                self.scopes.push(Scope {
                    kind: ScopeKind::Parameter,
                    stack_restorepoint: None,
                    variables: HashMap::new(),
                });

                self.current_function = Some(f);

                for (i, param) in params.iter().enumerate() {
                    let val = f.get_nth_param(i as u32).unwrap();
                    self.add_to_current_scope(
                        param.name.sym,
                        Variable {
                            type_token: *self.node_types.get(&param.id).unwrap(),
                            ty: val.get_type(),
                            val: VariableValue::Parameter(val),
                        },
                    );
                }

                let entry = self.ctx.append_basic_block(f, "entry");

                self.position_builder_at_end_of(entry);

                self.push_scope_with_stack_restore_point(ScopeKind::Function);
                self.gen_block(body)?;
                self.scopes.pop();

                self.current_function = None;

                self.scopes.pop();

                Ok(())
            }
            ItemKind::ExternalFunction { .. } | ItemKind::Layout { .. } => Ok(()),
        }
    }

    fn gen_block(&mut self, block: &Block) -> Result<bool, Error> {
        for statement in &block.statements {
            let terminated = self.gen_statement(statement)?;
            if terminated {
                return Ok(true);
            }
        }

        Ok(false)
    }

    fn gen_statement(&mut self, stmt: &Statement) -> Result<bool, Error> {
        match &stmt.kind {
            StatementKind::Return(Some(e)) => {
                let function_scope = self
                    .scopes
                    .iter()
                    .rev()
                    .find(|scope| scope.kind == ScopeKind::Function)
                    .expect("There should exist a function scope");
                let restore_point = function_scope
                    .stack_restorepoint
                    .expect("The function should have a stack restore point");
                let val = self.gen_expr(e)?;
                self.builder
                    .build_call(self.llvm_stackrestore, &[restore_point.into()], "")?;
                self.builder.build_return(Some(&val))?;
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
                )?;

                let mut branch_terminator_count = 0u32;

                self.position_builder_at_end_of(then_block);
                self.push_scope_with_stack_restore_point(ScopeKind::Block);
                let has_terminator = self.gen_block(then)?;

                if !has_terminator {
                    let restore_point = self
                        .current_scope()
                        .stack_restorepoint
                        .expect("Blocks should have a stack restore point");
                    self.builder
                        .build_call(self.llvm_stackrestore, &[restore_point.into()], "")?;

                    self.builder.build_unconditional_branch(end_block)?;
                    branch_terminator_count += 1;
                }

                self.scopes.pop();

                self.position_builder_at_end_of(otherwise_block);
                if let Some(otherwise) = otherwise {
                    self.push_scope_with_stack_restore_point(ScopeKind::Block);
                    let has_terminator = self.gen_block(otherwise)?;

                    if !has_terminator {
                        let restore_point = self
                            .current_scope()
                            .stack_restorepoint
                            .expect("Blocks should have a stack restore point");
                        self.builder.build_call(
                            self.llvm_stackrestore,
                            &[restore_point.into()],
                            "",
                        )?;

                        self.builder.build_unconditional_branch(end_block)?;
                        branch_terminator_count += 1;
                    }
                } else {
                    self.builder.build_unconditional_branch(end_block)?;
                }

                if branch_terminator_count != 2 {
                    self.position_builder_at_end_of(end_block);
                } else {
                    let res = unsafe { end_block.delete() };
                    if res.is_err() {
                        todo!()
                    }
                }
            }
            StatementKind::Loop(body) => {
                let end_block = self.add_basic_block("endloop");
                let body_block = self.add_basic_block("loop");

                self.loop_end_blocks.push(end_block);

                self.builder.build_unconditional_branch(body_block)?;

                self.position_builder_at_end_of(body_block);

                self.push_scope_with_stack_restore_point(ScopeKind::Loop);
                self.gen_block(body)?;

                let restore_point = self
                    .current_scope()
                    .stack_restorepoint
                    .expect("Loop scopes should have a stack restore point");
                self.builder
                    .build_call(self.llvm_stackrestore, &[restore_point.into()], "")?;

                self.builder.build_unconditional_branch(body_block)?;
                self.scopes.pop();

                self.loop_end_blocks.pop();

                self.position_builder_at_end_of(end_block);
            }
            StatementKind::Let { name, ty, init } => {
                let type_token = self.node_types.get(&ty.id).unwrap();

                let AllocaTypeData {
                    explicit_align,
                    basic_type,
                    ..
                } = self.get_alloca_type_data(*type_token);

                let ptr = self.builder.build_alloca(basic_type, "").expect("");

                if let Some(align) = explicit_align {
                    let instruction = ptr.as_instruction().unwrap();
                    instruction.set_alignment(align)?;
                }

                if let Some(init) = init {
                    let val = self.gen_expr(init)?;
                    self.builder.build_store(ptr, val)?;
                }

                self.add_to_current_scope(
                    name.sym,
                    Variable {
                        type_token: *self.node_types.get(&ty.id).unwrap(),
                        ty: basic_type,
                        val: VariableValue::Stack(ptr),
                    },
                );
            }
            StatementKind::Set { dst, val } => {
                let p = self.get_dst_ptr(dst)?;
                let x = self.gen_expr(val)?;
                self.builder.build_store(p, x)?;
            }
            StatementKind::Break => {
                let loop_scope = self
                    .scopes
                    .iter()
                    .rev()
                    .find(|scope| scope.kind == ScopeKind::Loop)
                    .expect("There should exist a loop scope");

                let restore_point = loop_scope
                    .stack_restorepoint
                    .expect("The loop should have a stack restore point");

                self.builder
                    .build_call(self.llvm_stackrestore, &[restore_point.into()], "")?;
                self.builder.build_unconditional_branch(
                    *self
                        .loop_end_blocks
                        .last()
                        .expect("There should be a basic block present after a loop"),
                )?;
            }
            StatementKind::Expr(e) => 'expr_block: {
                if let ExprKind::Call { name, args } = &e.kind {
                    let Some(tok) = self.node_types.get(&name.id) else {
                        todo!()
                    };
                    if self.is_type_token_void_function(*tok) {
                        let args = args
                            .iter()
                            .map(|arg| self.gen_expr(arg).map(|x| x.into()))
                            .collect::<Result<Vec<_>, _>>()?;
                        let f = self.functions.get(&name.sym).expect("");
                        self.builder.build_call(*f, &args, "")?;

                        break 'expr_block;
                    }
                }

                self.gen_expr(e)?;
            }
            _ => todo!("Statement \"{:?}\"", stmt.kind),
        }

        let is_terminator = matches!(stmt.kind, StatementKind::Return(_) | StatementKind::Break);
        Ok(is_terminator)
    }

    fn get_dst_ptr(&mut self, e: &Expr) -> Result<PointerValue<'ctx>, Error> {
        match &e.kind {
            ExprKind::Identifier(sym) => {
                let Variable { ty, val, .. } =
                    self.get_variable(sym).expect("Identifier should exist");
                let VariableValue::Stack(ptr) = val else {
                    panic!("Tried to get dst of non-stack variable")
                };
                Ok(ptr)
            }
            ExprKind::BuiltinOp {
                op: BuiltinOpKind::At,
                args,
            } => {
                assert!(args.len() <= 2);

                if args.len() == 1 {
                    let ty = self.get_inktype_of_node(args[0].id);
                    let ptr = self.get_dst_ptr(&args[0])?;
                    let ptr = self.builder.build_load(ty, ptr, "")?.into_pointer_value();
                    Ok(ptr)
                } else {
                    let ty = self.get_inktype_of_node(args[0].id);
                    let ptr = self.get_dst_ptr(&args[0])?;
                    let idx = self.gen_expr(&args[1])?.into_int_value();
                    Ok(unsafe { self.builder.build_gep(ty, ptr, &[idx], "")? })
                }
            }
            _ => todo!("Get pointer for expression: {:?}", e.kind),
        }
    }

    fn gen_expr(&mut self, e: &Expr) -> Result<BasicValueEnum<'ctx>, Error> {
        match &e.kind {
            ExprKind::IntLiteral(x) => Ok(self.i64_t.const_int(*x as u64, false).into()),
            ExprKind::BoolLiteral(x) => {
                Ok(self.i64_t.const_int(if *x { 1 } else { 0 }, false).into())
            }
            ExprKind::Identifier(sym) => {
                let Variable { ty, val, .. } =
                    self.get_variable(sym).expect("Identifer should exist");
                match val {
                    VariableValue::Stack(ptr) => Ok(self.builder.build_load(ty, ptr, "")?),
                    _ => todo!("Retrieve value from variable"),
                }
            }
            ExprKind::Cast { e: src, .. } => {
                let src_ty_token = self.node_types.get(&src.id).unwrap();
                let src_ty = self.type_interner.get(src_ty_token);

                let dst_ty_token = self.node_types.get(&e.id).unwrap();
                let dst_ty = self.type_interner.get(dst_ty_token);

                let val = self.gen_expr(src)?;

                match (src_ty, dst_ty) {
                    (Type::Int { .. }, Type::Bool) => Ok(self
                        .builder
                        .build_int_compare(
                            IntPredicate::NE,
                            val.into_int_value(),
                            self.i64_t.const_int(0, false),
                            "",
                        )?
                        .into()),

                    (Type::Bool, Type::Int { width, .. }) => Ok(self
                        .builder
                        .build_int_z_extend(val.into_int_value(), self.int_type(*width), "")?
                        .into()),

                    (
                        Type::Int {
                            width: src_width, ..
                        },
                        Type::Int {
                            width: dst_width, ..
                        },
                    ) if src_width > dst_width => Ok(self
                        .builder
                        .build_int_truncate(val.into_int_value(), self.int_type(*dst_width), "")?
                        .into()),

                    (Type::Int { .. }, Type::Pointer(_)) => Ok(self
                        .builder
                        .build_int_to_ptr(val.into_int_value(), self.ptr_t, "")?
                        .into()),

                    (Type::Array(_, _), Type::Pointer(_)) => {
                        todo!()
                        //let base_type = val.into_array_value().get_type().get_element_type();
                        //Ok(self.builder.build_bitcast(val, base_type.ptr_type(AddressSpace::default()), "")?.into())
                    }

                    _ => todo!("Cast from {:?} to {:?}", src_ty, dst_ty),
                }
            }
            ExprKind::BuiltinOp { op, args } => match op {
                BuiltinOpKind::Not => {
                    let x = self.gen_expr(&args[0])?;
                    Ok(self.builder.build_not(x.into_int_value(), "")?.into())
                }
                BuiltinOpKind::Equals | BuiltinOpKind::NotEquals => {
                    // For now only support two operands at a time
                    assert_eq!(args.len(), 2);

                    let predicate = match op {
                        BuiltinOpKind::Equals => IntPredicate::EQ,
                        BuiltinOpKind::NotEquals => IntPredicate::NE,
                        _ => todo!(),
                    };

                    let x = self.gen_expr(&args[0])?;
                    let y = self.gen_expr(&args[1])?;
                    Ok(self
                        .builder
                        .build_int_compare(predicate, x.into_int_value(), y.into_int_value(), "")?
                        .into())
                }
                BuiltinOpKind::Add => {
                    assert!(!args.is_empty());

                    let vals = args
                        .iter()
                        .map(|x| self.gen_expr(x))
                        .collect::<Result<Vec<_>, _>>()?;

                    // The empty slice case is not covered by the destructuring. However, it cannot
                    // be empty, because we assert for that.
                    let [first, xs @ ..] = vals.as_slice() else {
                        unreachable!()
                    };

                    let mut sum_val = first.into_int_value();
                    for x in xs.iter() {
                        sum_val = self
                            .builder
                            .build_int_add(sum_val, x.into_int_value(), "")?;
                    }

                    Ok(sum_val.into())
                }
                BuiltinOpKind::Sub => {
                    assert!(args.len() >= 2);

                    let vals = args
                        .iter()
                        .map(|x| self.gen_expr(x))
                        .collect::<Result<Vec<_>, _>>()?;

                    // The empty slice case is not covered by the destructuring. However, it cannot
                    // be empty, because we assert for that.
                    let [first, xs @ ..] = vals.as_slice() else {
                        unreachable!()
                    };

                    let mut sum_val = first.into_int_value();
                    for x in xs.iter() {
                        sum_val = self
                            .builder
                            .build_int_sub(sum_val, x.into_int_value(), "")?;
                    }

                    Ok(sum_val.into())
                }
                BuiltinOpKind::Mul => {
                    // For now only support two operands at a time
                    assert_eq!(args.len(), 2);

                    let x = self.gen_expr(&args[0])?;
                    let y = self.gen_expr(&args[1])?;
                    Ok(self
                        .builder
                        .build_int_mul(x.into_int_value(), y.into_int_value(), "")?
                        .into())
                }
                BuiltinOpKind::Div => {
                    // For now only support two operands at a time
                    assert_eq!(args.len(), 2);

                    let x = self.gen_expr(&args[0])?;
                    let y = self.gen_expr(&args[1])?;
                    Ok(self
                        .builder
                        .build_int_signed_div(x.into_int_value(), y.into_int_value(), "")?
                        .into())
                }
                BuiltinOpKind::AddressOf => {
                    let ExprKind::Identifier(sym) = args[0].kind else {
                        todo!()
                    };
                    let var = self.get_variable(&sym).unwrap();
                    let Variable {
                        val: VariableValue::Stack(ptr),
                        ..
                    } = var
                    else {
                        todo!()
                    };

                    Ok(ptr.into())
                }
                BuiltinOpKind::At => {
                    let ExprKind::Identifier(sym) = args[0].kind else {
                        todo!()
                    };

                    let Variable {
                        type_token,
                        ty,
                        val,
                    } = self.get_variable(&sym).unwrap();

                    debug_assert!(ty.is_pointer_type());

                    let ptr = match val {
                        VariableValue::Stack(p) => p,
                        VariableValue::Parameter(p) => p.into_pointer_value(),
                    };

                    let addr = self.builder.build_load(ty, ptr, "")?.into_pointer_value();

                    let Type::Pointer(pointee_type_token) = self.type_interner.get(&type_token)
                    else {
                        todo!()
                    };
                    let pointee_type = self.get_ink_basic_type(*pointee_type_token);

                    Ok(self.builder.build_load(pointee_type, addr, "")?)
                }
                _ => todo!("BuiltinOpKind \"{:?}\"", op),
            },

            ExprKind::Call { name, args } => {
                let args = args
                    .iter()
                    .map(|arg| self.gen_expr(arg).map(|x| x.into()))
                    .collect::<Result<Vec<_>, _>>()?;
                let f = self.functions.get(&name.sym).expect("");
                let val = self.builder.build_call(*f, &args, "");
                Ok(val?.try_as_basic_value().left().expect(""))
            }

            _ => todo!("{:?}", e.kind),
        }
    }

    pub fn compile(&mut self, items: &[Item], machine: &MachineTarget) -> Result<(), Error> {
        self.scopes.push(Scope {
            kind: ScopeKind::Global,
            stack_restorepoint: None,
            variables: HashMap::new(),
        });

        for item in items {
            match &item.kind {
                ItemKind::Function { name, .. } | ItemKind::ExternalFunction { name, .. } => {
                    let function_type =
                        self.get_ink_function_type(*self.node_types.get(&item.id).expect(""));
                    let f =
                        self.module
                            .add_function(self.symbols.get(&name.sym), function_type, None);
                    self.functions.insert(name.sym, f);
                }
                ItemKind::Layout { .. } => (),
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

        // TODO Maybe this should be in some other place, so it doesn't get called multiple times
        Target::initialize_x86(&InitializationConfig::default());

        let triple = match machine {
            MachineTarget::Windows => TargetTriple::create("x86_64-pc-windows-msvc"),
            MachineTarget::Macos => TargetTriple::create("x86_64-apple-darwin21.6.0"),
        };
        self.module.set_triple(&triple);

        let target = Target::from_triple(&triple)
            .expect("Should be able to get target from initialized triple");
        let features = "";
        let target_machine = target
            .create_target_machine(
                &triple,
                "",
                features,
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .expect("Should be able to create target machine");

        let target_data = target_machine.get_target_data();
        let data_layout = target_data.get_data_layout();

        // TODO(david) I am not sure if this is really necessary?
        // Code generation is all done through the target machine, so I expect it will set the
        // data layout there as well.
        self.module.set_data_layout(&data_layout);
        self.target_machine = Some(target_machine);

        //let result = target_machine.write_to_file(&self.module, FileType::Object, Path::new(options.filename));

        //match options.output {
        //    OutputTarget::Assembly => {
        //        let result =
        //            target_machine.write_to_memory_buffer(&self.module, FileType::Assembly);

        //        match result {
        //            Err(msg) => todo!("Handle error: {}", msg.to_string()),
        //            Ok(buffer) => {
        //
        //            }
        //        }
        //    }
        //}

        Ok(())
    }

    pub fn run_optimization_passes(&mut self) {
        let pass_manager: PassManager<Module<'_>> = PassManager::create(());
        pass_manager.add_promote_memory_to_register_pass();
        pass_manager.add_cfg_simplification_pass();
        pass_manager.add_instruction_combining_pass();
        pass_manager.add_instruction_simplify_pass();
        pass_manager.add_aggressive_dce_pass();
        pass_manager.run_on(&self.module);
    }

    pub fn emit_llvm_ir_output(&self) -> String {
        self.module.to_string()
    }

    pub fn emit_asm_output(&self) -> Result<String, Error> {
        self.target_machine
            .as_ref()
            .unwrap()
            .write_to_memory_buffer(&self.module, FileType::Assembly)
            .map(|buffer| {
                std::str::from_utf8(buffer.as_slice())
                    .expect("Buffer should contain valid UTF-8")
                    .into()
            })
            .map_err(|_| todo!())
    }

    pub fn emit_object_output(&self) -> Result<MemoryBuffer, Error> {
        self.target_machine
            .as_ref()
            .unwrap()
            .write_to_memory_buffer(&self.module, FileType::Object)
            .map_err(|_| todo!())
    }
}
