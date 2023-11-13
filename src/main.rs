#![allow(dead_code, unused_variables)]

mod ast;
mod ast_visitor;
mod compiler;
mod error;
mod ink_codegen;
mod parser;
mod scope_stack;
mod semantics;
mod source_location;
mod string_interner;
mod tokenizer;
mod type_check;
mod types;

use anyhow::Result;
use ast::NodeIdGenerator;
use ink_codegen::{CodeGenerator, Options, OutputTarget};
use inkwell::targets::{InitializationConfig, Target};
use parser::Parser;
use semantics::validate_semantics;
use semantics::SemanticContext;
use std::collections::HashMap;
use std::fs;
use string_interner::{StringInterner, Symbol};
use type_check::type_check;
use types::{Signedness, Type, TypeInterner, TypeToken};

fn print_targets() {
    Target::initialize_all(&InitializationConfig::default());

    let mut current = Target::get_first();

    while let Some(target) = current {
        let name = target.get_name();
        let description = target.get_description();

        println!("{:?} => {:?}", name, description);

        current = target.get_next();
    }
}

fn main() -> Result<()> {
    let source = fs::read_to_string("external_fn.gamma").unwrap();

    let mut id_generator = NodeIdGenerator::new();
    let mut symbols = StringInterner::new();
    let mut type_tokens = TypeInterner::new();

    let mut spans = HashMap::new();
    let mut types = HashMap::new();

    let mut parser = Parser::new(&source, &mut symbols, &mut id_generator, &mut spans);

    let items_result = parser.parse_items().map_err(|e| vec![e]);

    if let Err(ref errors) = items_result {
        for error in errors.iter() {
            error.print(&source, &symbols, &type_tokens);
        }
    }

    println!("Parsing done...");

    let Ok(items) = items_result else {
        unreachable!()
    };

    let mut type_table = HashMap::<Symbol, TypeToken>::from([
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

    let semantics_result = {
        let context = SemanticContext {
            symbols: &symbols,
            spans: &spans,
        };

        validate_semantics(&context, &items)
    };

    println!("Semantic validation done...");

    assert!(semantics_result.is_ok());

    let type_check_result = type_check(&items, &mut type_tokens, &mut types, &mut type_table);

    println!("Type checking done...");

    if let Err(ref errors) = type_check_result {
        for error in errors.iter() {
            error.print(&source, &symbols, &type_tokens);
        }
    }

    assert!(type_check_result.is_ok());

    let ctx = inkwell::context::Context::create();
    let mut codegen = CodeGenerator::new(&ctx, &symbols, &types, &type_tokens);
    let options = Options {
        optimize: false,
        output: OutputTarget::LlvmIr,
    };

    let output = codegen.compile(&items, &options).map_err(|e| vec![e]);

    assert!(output.is_ok());

    let text = output.unwrap();

    print!("{}", text);

    /*
    println!("{}", ron::ser::to_string(&symbols).expect(""));
    println!("{}", ron::ser::to_string(&ast_spans).expect(""));

    if let Ok(items) = items_result {
        println!(
            "{}",
            ron::ser::to_string(&items).expect("Should be serializable")
        );
    }
    */

    /*
        println!("Gamma compiler version 0.2\n");

        let source = "
    fn main() -> int {
        if (eq (cast int false) 0) {
            return 1
        } else {
            return 0
        }
    }";

        let lines = source.lines().collect::<Vec<_>>();

        let mut string_interner = StringInterner::new();
        let mut id_generator = NodeIdGenerator::new();
        let mut spans = HashMap::new();

        let mut parser = Parser::new(source, &mut string_interner, &mut id_generator, &mut spans);

        let items = parser.parse_items()?;

        let errors = semantics::validate_all(&string_interner, &spans, &items);

        println!("{} errors were found", errors.len());

        let context = crate::semantics::Context {
            symbols: &string_interner,
            spans: &spans,
        };

        for error in errors.iter() {
            error.print(&context);
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
                _ => println!("TODO: {:?}", err),
            }
        }

        if type_errors.is_empty() {
            println!("The program successfully type checked!");
        } else {
            println!("There were type errors");
            println!("{:?}", items);
            return Ok(());
        }

        let ctx = inkwell::context::Context::create();
        let mut codegen = CodeGenerator::new(&ctx, &string_interner, &types, &type_interner);
        let output = codegen.compile(&items, false)?;
        println!("{}", output);

        */
    Ok(())
}
