use gamma::compiler::{Context, Options};
use gamma::ink_codegen::{OutputTarget, MachineTarget};
use insta::assert_snapshot;
use std::fs;

#[test]
fn sum_loop() {
    let contents = fs::read_to_string("tests/valid_samples/sum_loop.gamma").unwrap();

    let mut context = Context::new();
    let result = context.compile(&contents, &Options { optimize: false, output: OutputTarget::LlvmIr, machine: MachineTarget::Windows });

    let Ok(output) = result else {
        let Err(errors) = result else { unreachable!() };

        for error in errors.iter() {
            error.print(&contents, &context.spans, &context.symbols, &context.type_tokens);
        }

        panic!("Compilation of sample \"sum_loop\" resulted in {} error(s)", errors.len()); 
    };

    assert_snapshot!("sum_loop", output);
}

#[test]
fn only_main() {
    let contents = fs::read_to_string("tests/valid_samples/only_main.gamma").unwrap();

    let mut context = Context::new();
    let result = context.compile(&contents, &Options { optimize: false, output: OutputTarget::LlvmIr, machine: MachineTarget::Windows });

    let Ok(output) = result else {
        let Err(errors) = result else { unreachable!() };

        for error in errors.iter() {
            error.print(&contents, &context.spans, &context.symbols, &context.type_tokens);
        }

        panic!("Compilation of sample \"only_main\" resulted in {} error(s)", errors.len()); 
    };

    assert_snapshot!("only_main", output);
}

#[test]
fn array() {
    let contents = fs::read_to_string("tests/valid_samples/array.gamma").unwrap();

    let mut context = Context::new();
    let result = context.compile(&contents, &Options { optimize: false, output: OutputTarget::LlvmIr, machine: MachineTarget::Windows });

    let Ok(output) = result else {
        let Err(errors) = result else { unreachable!() };

        for error in errors.iter() {
            error.print(&contents, &context.spans, &context.symbols, &context.type_tokens);
        }

        panic!("Compilation of sample \"array\" resulted in {} error(s)", errors.len()); 
    };

    assert_snapshot!("array", output);
}

#[test]
fn pointer() {
    let contents = fs::read_to_string("tests/valid_samples/pointer.gamma").unwrap();

    let mut context = Context::new();
    let result = context.compile(&contents, &Options { optimize: false, output: OutputTarget::LlvmIr, machine: MachineTarget::Windows });

    let Ok(output) = result else {
        let Err(errors) = result else { unreachable!() };

        for error in errors.iter() {
            error.print(&contents, &context.spans, &context.symbols, &context.type_tokens);
        }

        panic!("Compilation of sample \"pointer\" resulted in {} error(s)", errors.len()); 
    };

    assert_snapshot!("pointer", output);
}

#[test]
fn nested_loops() {
    let contents = fs::read_to_string("tests/valid_samples/nested_loops.gamma").unwrap();

    let mut context = Context::new();
    let result = context.compile(&contents, &Options { optimize: false, output: OutputTarget::LlvmIr, machine: MachineTarget::Windows });

    let Ok(output) = result else {
        let Err(errors) = result else { unreachable!() };

        for error in errors.iter() {
            error.print(&contents, &context.spans, &context.symbols, &context.type_tokens);
        }

        panic!("Compilation of sample \"nested_loops\" resulted in {} error(s)", errors.len()); 
    };

    assert_snapshot!("nested_loops", output);
}

#[test]
fn arithmetic() {
    let contents = fs::read_to_string("tests/valid_samples/arithmetic.gamma").unwrap();

    let mut context = Context::new();
    let result = context.compile(&contents, &Options { optimize: false, output: OutputTarget::LlvmIr, machine: MachineTarget::Windows });

    let Ok(output) = result else {
        let Err(errors) = result else { unreachable!() };

        for error in errors.iter() {
            error.print(&contents, &context.spans, &context.symbols, &context.type_tokens);
        }

        panic!("Compilation of sample \"arithmetic\" resulted in {} error(s)", errors.len()); 
    };

    assert_snapshot!("arithmetic", output);
}

