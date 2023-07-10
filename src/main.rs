#![allow(dead_code, unused_variables)]

mod ast;
mod ast_visitor;
mod codegen;
mod env;
mod parser;
mod semantics;
mod source_location;
mod string_interner;
mod tokenizer;
mod type_check;
mod types;

use ast::NodeIdGenerator;
use codegen::CodeGenerator;
use parser::Parser;
use std::collections::HashMap;
use string_interner::StringInterner;
use type_check::{type_check, TypeError};
use types::TypeInterner;

/*
struct Compiler {
    string_interner: StringInterner,
    id_generator: NodeIdGenerator,
    spans: HashMap<NodeId, SourceSpan>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            string_interner: StringInterner::new(),
            id_generator: NodeIdGenerator::new(),
            spans: HashMap::new(),
        }
    }

    pub fn parse_source(&mut self, source: &src) -> Result<Vec<Item>, ParseError> {
        let mut parser = Parser::new(source, &mut self.string_interner, &mut self.id_generator, &mut self.spans);
    }

    pub fn validate_items(&mut self, items: &[Item]) ->  Option<ValidationError> {
        todo!()
    }

    pub fn generate_code(&mut self, items: &[Item]) -> Code {
        todo!()
    }
}
*/

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Gamma compiler version 0.1\n");

    let source = "fn main() -> int { return 0; }";

    let lines = source.lines().collect::<Vec<_>>();

    let mut string_interner = StringInterner::new();
    let mut id_generator = NodeIdGenerator::new();
    let mut spans = HashMap::new();

    let mut parser = Parser::new(source, &mut string_interner, &mut id_generator, &mut spans);

    let items = parser.parse_items()?;

    let errors = semantics::validate_all(&string_interner, &spans, &items);

    println!("{} errors were found", errors.len());

    for error in errors.iter() {
        error.print(&string_interner);
    }

    if !errors.is_empty() {
        return Ok(());
    }

    let mut type_interner = TypeInterner::new();
    let mut types = HashMap::new();

    let type_errors = type_check(&items, &mut type_interner, &mut types);

    for err in &type_errors {
        match err {
            TypeError::TypeMismatch { expected, received } => {
                println!(
                    "{}",
                    lines[(spans.get(&expected.1).unwrap().start.line - 1) as usize]
                );
                println!(
                    "Expected type {} due to {:?}, got type {} from {:?}",
                    type_interner.get(&expected.0).to_string(&type_interner),
                    spans.get(&expected.1).unwrap(),
                    type_interner.get(&received.0).to_string(&type_interner),
                    spans.get(&received.1).unwrap(),
                );
            }
            _ => todo!(),
        }
    }

    if type_errors.is_empty() {
        println!("The program successfully type checked!");
    } else {
        println!("There were type errors");
        return Ok(());
    }

    let ctx = jello::Context::create();
    let mut code_generator = CodeGenerator::new(&ctx, &string_interner);
    code_generator.compile(&items, "out.ll")?;

    println!("Generated \"out.ll\"");

    Ok(())
}
