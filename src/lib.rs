#![allow(dead_code, unused_variables)]

pub mod ast;
pub mod ast_visitor;
pub mod compiler;
pub mod error;
pub mod ink_codegen;
pub mod parser;
pub mod scope_stack;
pub mod semantics;
pub mod source_location;
pub mod string_interner;
pub mod tokenizer;
pub mod type_check;
pub mod type_interner;
pub mod types;
