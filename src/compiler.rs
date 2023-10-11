use crate::{
    ast::{NodeId, NodeIdGenerator},
    error::Error,
    ink_codegen::CodeGenerator,
    parser::Parser,
    semantics::{validate_semantics, SemanticContext},
    source_location::SourceSpan,
    string_interner::{StringInterner, Symbol},
    type_check::type_check,
    types::{Type, TypeInterner, TypeToken},
};
use std::collections::HashMap;

pub use crate::ink_codegen::{Options, OutputTarget};

pub struct Context {
    pub id_generator: NodeIdGenerator,

    pub symbols: StringInterner,
    pub type_tokens: TypeInterner,
    pub type_table: HashMap<Symbol, TypeToken>,

    pub spans: HashMap<NodeId, SourceSpan>,
    pub types: HashMap<NodeId, TypeToken>,
}

impl Context {
    pub fn new() -> Self {
        let mut symbols = StringInterner::new();
        let mut type_tokens = TypeInterner::new();
        let type_table = HashMap::from([
            (symbols.add("int"), type_tokens.add(Type::Int)),
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

    pub fn compile(&mut self, source: &str, options: &Options) -> Result<String, Vec<Error>> {
        let mut parser = Parser::new(
            source,
            &mut self.symbols,
            &mut self.id_generator,
            &mut self.spans,
        );

        let items = parser.parse_items().map_err(|e| vec![e])?;

        let semantic_context = SemanticContext {
            symbols: &self.symbols,
            spans: &self.spans,
        };

        validate_semantics(&semantic_context, &items)?;

        type_check(
            &items,
            &mut self.type_tokens,
            &mut self.types,
            &mut self.type_table,
        )?;

        let ctx = inkwell::context::Context::create();
        let mut codegen = CodeGenerator::new(&ctx, &self.symbols, &self.types, &self.type_tokens);
        let output = codegen.compile(&items, options).map_err(|e| vec![e])?;

        Ok(output)
    }
}
