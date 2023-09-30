#![allow(dead_code, unused_variables)]

mod ast;
mod ast_visitor;
mod compiler;
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
use compiler::{Context, Options};

fn main() -> Result<()> {
    let source = "fn main() -> int {
    let s: int
    set s = 0
    loop {
        if (= s 10) {
            break
        }

        set s = (+ s 1)
    }

    return s
}";

    let mut context = Context::new();
    let result = context.compile(source, &Options { optimize: false });

    let Ok(output) = result else {
        let Err(errors) = result else { unreachable!() };

        for e in &errors {
            e.print(&context);
        }

        panic!("Compilation resulted in {} error(s)", errors.len());
    };

    print!("{}", output);

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
