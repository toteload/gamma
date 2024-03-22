use gamma::compiler::{Context, Options, Output};
use gamma::ink_codegen::MachineTarget;
use insta::assert_snapshot;
use std::fs;

fn compile(source: &str) -> String {
    let mut context = Context::new();
    let result = context.compile(
        &source,
        &Options {
            target: MachineTarget::Windows,
            enable_optimizations: false,
            emit_llvm_ir: true,
            emit_asm: false,
            emit_object: false,
        },
    );

    let Ok(Output {
        llvm_ir: Some(output),
        ..
    }) = result
    else {
        let Err(errors) = result else { unreachable!() };

        for error in errors.iter() {
            error.print(
                &source,
                &context.spans,
                &context.symbols,
                &context.type_tokens,
            );
        }

        panic!(
            "Compilation of sample resulted in {} error(s)",
            errors.len(),
        );
    };

    output
}

#[test]
fn sum_loop() {
    let source = fs::read_to_string("tests/valid_samples/sum_loop.gamma").unwrap();
    let output = compile(&source);
    assert_snapshot!("sum_loop", output);
}

#[test]
fn nested_layout() {
    let source = fs::read_to_string("tests/valid_samples/nested_layout.gamma").unwrap();
    let output = compile(&source);
    assert_snapshot!("nested_layout", output);
}

#[test]
fn comments() {
    let source = fs::read_to_string("tests/valid_samples/comments.gamma").unwrap();
    let output = compile(&source);
    assert_snapshot!("comments", output);
}

#[test]
fn only_main() {
    let source = fs::read_to_string("tests/valid_samples/only_main.gamma").unwrap();
    let output = compile(&source);
    assert_snapshot!("only_main", output);
}

#[test]
fn array() {
    let source = fs::read_to_string("tests/valid_samples/array.gamma").unwrap();
    let output = compile(&source);
    assert_snapshot!("array", output);
}

#[test]
fn pointer() {
    let source = fs::read_to_string("tests/valid_samples/pointer.gamma").unwrap();
    let output = compile(&source);
    assert_snapshot!("pointer", output);
}

#[test]
fn bitwise_operators() {
    let source = fs::read_to_string("tests/valid_samples/bitwise_operators.gamma").unwrap();
    let output = compile(&source);
    assert_snapshot!("bitwise_operators", output);
}

#[test]
fn nested_loops() {
    let source = fs::read_to_string("tests/valid_samples/nested_loops.gamma").unwrap();
    let output = compile(&source);
    assert_snapshot!("nested_loops", output);
}

#[test]
fn arithmetic() {
    let source = fs::read_to_string("tests/valid_samples/arithmetic.gamma").unwrap();
    let output = compile(&source);
    assert_snapshot!("arithmetic", output);
}

#[test]
fn break_labeled_loop() {
    let source = fs::read_to_string("tests/valid_samples/break_labeled_loop.gamma").unwrap();
    let output = compile(&source);
    assert_snapshot!("break_labeled_loop", output);
}
