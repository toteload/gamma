use crate::{
    ast::{AstMap, NodeIdGenerator},
    error::Error,
    ink_codegen::{CodeGenerator, MachineTarget},
    parser::Parser,
    semantics::{validate_semantics, SemanticContext},
    source_location::SourceSpan,
    string_interner::{StringInterner, Symbol},
    type_annotation::type_annotate,
    type_check2::type_check,
    type_coercion::type_coerce,
    type_interner::{TypeInterner, TypeToken},
    type_node_annotation::annotate_type_nodes,
    types::{Signedness, Type},
};
use inkwell::memory_buffer::MemoryBuffer;
use std::collections::HashMap;

pub struct Options {
    pub target: MachineTarget,
    pub enable_optimizations: bool,
    pub emit_llvm_ir: bool,
    pub emit_asm: bool,
    pub emit_object: bool,
}

pub struct Output {
    pub llvm_ir: Option<String>,
    pub asm: Option<String>,
    pub object: Option<MemoryBuffer>,
}

pub struct Context {
    pub id_generator: NodeIdGenerator,
    pub symbols: StringInterner,
    pub type_tokens: TypeInterner,

    pub type_table: HashMap<Symbol, TypeToken>,

    pub spans: AstMap<SourceSpan>,
    pub types: AstMap<TypeToken>,
}

impl From<Error> for Vec<Error> {
    fn from(e: Error) -> Self {
        todo!()
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

impl Context {
    pub fn new() -> Self {
        let mut symbols = StringInterner::new();
        let type_tokens = TypeInterner::new();

        let type_table = HashMap::from([
            (
                symbols.add("i8"),
                type_tokens.add(Type::Int {
                    signedness: Signedness::Signed,
                    width: 8,
                }),
            ),
            (
                symbols.add("i16"),
                type_tokens.add(Type::Int {
                    signedness: Signedness::Signed,
                    width: 16,
                }),
            ),
            (
                symbols.add("i32"),
                type_tokens.add(Type::Int {
                    signedness: Signedness::Signed,
                    width: 32,
                }),
            ),
            (
                symbols.add("i64"),
                type_tokens.add(Type::Int {
                    signedness: Signedness::Signed,
                    width: 64,
                }),
            ),
            (
                symbols.add("u8"),
                type_tokens.add(Type::Int {
                    signedness: Signedness::Unsigned,
                    width: 8,
                }),
            ),
            (
                symbols.add("u16"),
                type_tokens.add(Type::Int {
                    signedness: Signedness::Unsigned,
                    width: 16,
                }),
            ),
            (
                symbols.add("u32"),
                type_tokens.add(Type::Int {
                    signedness: Signedness::Unsigned,
                    width: 32,
                }),
            ),
            (
                symbols.add("u64"),
                type_tokens.add(Type::Int {
                    signedness: Signedness::Unsigned,
                    width: 64,
                }),
            ),
            (symbols.add("void"), type_tokens.add(Type::Void)),
            (symbols.add("bool"), type_tokens.add(Type::Bool)),
        ]);

        Context {
            id_generator: NodeIdGenerator::new(),
            symbols,
            type_tokens,
            type_table,
            spans: HashMap::new(),
            types: HashMap::new(),
        }
    }

    pub fn compile(&mut self, source: &str, options: &Options) -> Result<Output, Vec<Error>> {
        let mut parser = Parser::new(
            source,
            &mut self.symbols,
            &mut self.id_generator,
            &mut self.spans,
        );

        let mut items = parser.parse_items().map_err(|e| vec![e])?;

        println!("{}", self.symbols.to_string());

        let semantic_context = SemanticContext {
            symbols: &self.symbols,
            spans: &self.spans,
        };

        validate_semantics(&semantic_context, &items)?;

        annotate_type_nodes(
            &items,
            &mut self.type_tokens,
            &mut self.types,
            &mut self.type_table,
        )?;

        type_annotate(
            &items,
            &mut self.type_tokens,
            &mut self.types,
            &mut self.type_table,
        )?;

        type_coerce(
            &mut items,
            &mut self.type_tokens,
            &mut self.types,
            &mut self.id_generator,
        )?;

        type_check(
            &items,
            &mut self.type_tokens,
            &mut self.types,
            &mut self.type_table,
        )?;

        let ctx = inkwell::context::Context::create();
        let mut codegen = CodeGenerator::new(&ctx, &self.symbols, &self.types, &self.type_tokens);
        codegen
            .compile(&items, &options.target)
            .map_err(|e| vec![e])?;

        if options.enable_optimizations {
            codegen.run_optimization_passes();
        }

        let llvm_ir = options.emit_llvm_ir.then(|| codegen.emit_llvm_ir_output());

        let asm = if options.emit_asm {
            Some(codegen.emit_asm_output()?)
        } else {
            None
        };

        let object = if options.emit_object {
            Some(codegen.emit_object_output()?)
        } else {
            None
        };

        Ok(Output {
            llvm_ir,
            asm,
            object,
        })
    }
}
