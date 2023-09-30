use crate::{
    ast::{NodeId, NodeIdGenerator},
    ink_codegen::CodeGenerator,
    parser::Parser,
    semantics::validate_semantics,
    source_location::SourceSpan,
    string_interner::StringInterner,
    type_check::type_check,
    types::{TypeInterner, TypeToken},
};
use std::collections::HashMap;

pub trait PrintableError {
    fn print(&self, context: &Context);
}

pub struct Options {
    pub optimize: bool,
}

pub struct Context {
    pub id_generator: NodeIdGenerator,

    pub symbols: StringInterner,
    pub types: TypeInterner,

    pub ast_spans: HashMap<NodeId, SourceSpan>,
    pub ast_types: HashMap<NodeId, TypeToken>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            id_generator: NodeIdGenerator::new(),
            symbols: StringInterner::new(),
            types: TypeInterner::new(),
            ast_spans: HashMap::new(),
            ast_types: HashMap::new(),
        }
    }

    pub fn compile(
        &mut self,
        source: &str,
        options: &Options,
    ) -> Result<String, Vec<Box<dyn PrintableError>>> {
        let mut parser = Parser::new(
            source,
            &mut self.symbols,
            &mut self.id_generator,
            &mut self.ast_spans,
        );

        let items = parser.parse_items().map_err(|e| {
            let err = Box::new(e) as Box<dyn PrintableError>;
            vec![err]
        })?;

        validate_semantics(&self, &items)?;

        type_check(&items, &mut self.types, &mut self.ast_types).map_err(|errs| {
            errs.into_iter()
                .map(|e| Box::new(e) as Box<dyn PrintableError>)
                .collect::<Vec<Box<dyn PrintableError>>>()
        })?;

        let ctx = inkwell::context::Context::create();
        let mut codegen = CodeGenerator::new(&ctx, &self.symbols, &self.ast_types, &self.types);
        let output = codegen.compile(&items, options.optimize).map_err(|e| {
            let err = Box::new(e) as Box<dyn PrintableError>;
            vec![err]
        })?;

        Ok(output)
    }
}
