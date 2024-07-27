#![allow(dead_code, unused_variables)]

mod ast;
mod ast_helpers;
mod ast_transform;
mod compiler;
mod error;
mod ink_codegen;
mod parser;
mod scope_stack;
mod semantics;
mod source_location;
mod string_interner;
mod tokenizer;
mod type_annotation;
mod type_check2;
mod type_coercion;
mod type_interner;
mod type_node_annotation;
mod types;
mod utils;
mod visitor;
mod visitor_mut;
mod visitor_mut_with_context;
mod visitor_with_context;

use anyhow::Result;
use clap::Parser as ClapParser;
use compiler::{Context, Options};
use ink_codegen::MachineTarget;
use inkwell::targets::{InitializationConfig, Target};
use std::fs;
use std::path::Path;

fn parse_machine_target(s: &str) -> Result<MachineTarget, &'static str> {
    match s {
        "windows" => Ok(MachineTarget::Windows),
        "macos" => Ok(MachineTarget::Macos),
        _ => Err("Invalid machine target"),
    }
}

#[derive(ClapParser, Debug)]
struct Args {
    input_file: String,

    #[arg(short, long, default_value_t = false)]
    enable_optimizations: bool,

    #[arg(long, default_value_t = true)]
    emit_asm: bool,

    #[arg(long, default_value_t = true)]
    emit_llvm_ir: bool,

    #[arg(short, long, value_parser = parse_machine_target)]
    target: Option<MachineTarget>,
}

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
    let Args {
        input_file,
        enable_optimizations,
        emit_asm,
        emit_llvm_ir,
        target,
    } = Args::try_parse()?;

    let target = if let Some(target) = target {
        target
    } else {
        parse_machine_target(std::env::consts::OS).expect("")
    };

    let mut compiler_context = Context::new();

    let source = fs::read_to_string(&input_file).unwrap();

    let result = compiler_context.compile(
        &source,
        &Options {
            enable_optimizations,
            emit_asm,
            emit_llvm_ir,
            emit_object: true,
            target,
        },
    );

    let output = match result {
        Ok(x) => x,
        Err(errors) => {
            for e in errors {
                e.print(
                    &source,
                    &compiler_context.spans,
                    &compiler_context.symbols,
                    &compiler_context.type_tokens,
                );
            }
            //println!("{}", compiler_context.symbols.to_string());
            return Ok(());
        }
    };

    // Instructions for building an executable on Windows:
    // - Make sure to run `vcvarsall.bat x64` before doing anything.
    // - Make sure that start_windows.o exists. If it doesn't, compile it with `clang -c
    // start_windows.c -o start_windows.o`.
    // - Output the Gamma program as assembly into a file like program.asm
    // - Compile the assembly with `clang -c program.asm -o program.o`
    // - Link the two files into an executable with `link start_windows.o program.o -entry:start
    // /NODEFAULTLIB kernel32.lib`.

    let input_path = Path::new(&input_file);
    let stem = input_path.file_stem();

    if let Some(llvm_ir) = output.llvm_ir {
        let output_path = input_path.with_extension("ll");
        fs::write(output_path, llvm_ir)?;
    }

    if let Some(asm) = output.asm {
        let output_path = input_path.with_extension("s");
        fs::write(output_path, asm)?;
    }

    if let Some(object) = output.object {
        let output_path = input_path.with_extension("o");
        fs::write(output_path, object.as_slice())?;
    }

    println!("Done!");

    Ok(())
}
