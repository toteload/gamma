#![allow(dead_code, unused_variables)]

mod ast;
mod ast_visitor;
mod compiler;
mod error;
mod ink_codegen;
mod layout;
mod parser;
mod scope_stack;
mod semantics;
mod source_location;
mod string_interner;
mod tokenizer;
mod type_check;
mod types;
mod utils;

use anyhow::Result;
use clap::Parser as ClapParser;
use compiler::Context;
use ink_codegen::{Options, OutputTarget};
use inkwell::targets::{InitializationConfig, Target};
use std::fs;

fn parse_output_format(s: &str) -> Result<OutputTarget, &'static str> {
    match s {
        "llvmir" => Ok(OutputTarget::LlvmIr),
        "asm" => Ok(OutputTarget::Assembly),
        _ => Err("Invalid output format"),
    }
}

impl std::fmt::Display for OutputTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LlvmIr => write!(f, "llvmir"),
            Self::Assembly => write!(f, "asm"),
        }
    }
}

#[derive(ClapParser, Debug)]
struct Args {
    input_file: String,

    #[arg(short, long, default_value_t = false)]
    enable_optimizations: bool,

    #[arg(short, long, default_value_t = OutputTarget::LlvmIr, value_parser = parse_output_format)]
    output_format: OutputTarget,
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
    //for arg in std::env::args_os() {
    //    println!("{arg:?}");
    //}

    let args = Args::try_parse()?;

    let mut compiler_context = Context::new();

    let source = fs::read_to_string(args.input_file).unwrap();

    let result = compiler_context.compile(
        &source,
        &Options {
            optimize: args.enable_optimizations,
            output: args.output_format,
        },
    );

    let Ok(output) = result else {
        let Err(errors) = result else { unreachable!() };
        for e in errors {
            e.print(
                &source,
                &compiler_context.spans,
                &compiler_context.symbols,
                &compiler_context.type_tokens,
            );
        }
        return Ok(());
    };

    // Instructions for building an executable on Windows:
    // - Make sure to run `vcvarsall.bat x64` before doing anything.
    // - Make sure that start_windows.o exists. If it doesn't, compile it with `clang -c
    // start_windows.c -o start_windows.o`.
    // - Output the Gamma program as assembly into a file like program.asm
    // - Compile the assembly with `clang -c program.asm -o program.o`
    // - Link the two files into an executable with `link start_windows.o program.o -entry:start
    // /NODEFAULTLIB kernel32.lib`.

    println!("{}", output);

    Ok(())
}
