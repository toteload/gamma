use gamma::compiler::{Context, Options};
use insta::assert_snapshot;
use std::fs;

#[test]
fn basic_arithmetic() {
    let contents = fs::read_to_string("tests/valid_samples/basic_arithmetic.gamma").unwrap();

    let mut context = Context::new();
    let result = context.compile(&contents, &Options { optimize: false });

    let Ok(output) = result else {
        let Err(errors) = result else { unreachable!() };

        for e in &errors {
            e.print(&context);
        }

        panic!(
            "Compilation of sample \"basic_arithmetic\" resulted in {} error(s)",
            errors.len()
        );
    };

    assert_snapshot!("basic_arithmetic", output);
}

#[test]
fn basic_main() {
    let contents = fs::read_to_string("tests/valid_samples/basic_main.gamma").unwrap();

    let mut context = Context::new();
    let result = context.compile(&contents, &Options { optimize: false });

    let Ok(output) = result else {
        let Err(errors) = result else { unreachable!() };

        for e in &errors {
            e.print(&context);
        }

        panic!(
            "Compilation of sample \"basic_main\" resulted in {} error(s)",
            errors.len()
        );
    };

    assert_snapshot!("basic_main", output);
}

#[test]
fn nested_loop() {
    let contents = fs::read_to_string("tests/valid_samples/nested_loop.gamma").unwrap();

    let mut context = Context::new();
    let result = context.compile(&contents, &Options { optimize: false });

    let Ok(output) = result else {
        let Err(errors) = result else { unreachable!() };

        for e in &errors {
            e.print(&context);
        }

        panic!(
            "Compilation of sample \"nested_loop\" resulted in {} error(s)",
            errors.len()
        );
    };

    assert_snapshot!("nested_loop", output);
}

#[test]
fn sum_loop() {
    let contents = fs::read_to_string("tests/valid_samples/sum_loop.gamma").unwrap();

    let mut context = Context::new();
    let result = context.compile(&contents, &Options { optimize: false });

    let Ok(output) = result else {
        let Err(errors) = result else { unreachable!() };

        for e in &errors {
            e.print(&context);
        }

        panic!(
            "Compilation of sample \"sum_loop\" resulted in {} error(s)",
            errors.len()
        );
    };

    assert_snapshot!("sum_loop", output);
}
#[test]
fn basic_arithmetic_optimized() {
    let contents = fs::read_to_string("tests/valid_samples/basic_arithmetic.gamma").unwrap();

    let mut context = Context::new();
    let result = context.compile(&contents, &Options { optimize: true });

    let Ok(output) = result else {
        let Err(errors) = result else { unreachable!() };

        for e in &errors {
            e.print(&context);
        }

        panic!(
            "Compilation of sample \"basic_arithmetic_optimized\" resulted in {} error(s)",
            errors.len()
        );
    };

    assert_snapshot!("basic_arithmetic_optimized", output);
}

#[test]
fn basic_main_optimized() {
    let contents = fs::read_to_string("tests/valid_samples/basic_main.gamma").unwrap();

    let mut context = Context::new();
    let result = context.compile(&contents, &Options { optimize: true });

    let Ok(output) = result else {
        let Err(errors) = result else { unreachable!() };

        for e in &errors {
            e.print(&context);
        }

        panic!(
            "Compilation of sample \"basic_main_optimized\" resulted in {} error(s)",
            errors.len()
        );
    };

    assert_snapshot!("basic_main_optimized", output);
}

#[test]
fn nested_loop_optimized() {
    let contents = fs::read_to_string("tests/valid_samples/nested_loop.gamma").unwrap();

    let mut context = Context::new();
    let result = context.compile(&contents, &Options { optimize: true });

    let Ok(output) = result else {
        let Err(errors) = result else { unreachable!() };

        for e in &errors {
            e.print(&context);
        }

        panic!(
            "Compilation of sample \"nested_loop_optimized\" resulted in {} error(s)",
            errors.len()
        );
    };

    assert_snapshot!("nested_loop_optimized", output);
}

#[test]
fn sum_loop_optimized() {
    let contents = fs::read_to_string("tests/valid_samples/sum_loop.gamma").unwrap();

    let mut context = Context::new();
    let result = context.compile(&contents, &Options { optimize: true });

    let Ok(output) = result else {
        let Err(errors) = result else { unreachable!() };

        for e in &errors {
            e.print(&context);
        }

        panic!(
            "Compilation of sample \"sum_loop_optimized\" resulted in {} error(s)",
            errors.len()
        );
    };

    assert_snapshot!("sum_loop_optimized", output);
}
