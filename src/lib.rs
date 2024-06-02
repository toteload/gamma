#![allow(dead_code, unused_variables)]

pub mod ast;
pub mod compiler;
pub mod error;
pub mod ink_codegen;
pub mod parser;
pub mod scope_stack;
pub mod semantics;
pub mod source_location;
pub mod string_interner;
pub mod tokenizer;
pub mod type_interner;
pub mod type_annotation;
pub mod type_check2;
pub mod type_coercion;
pub mod type_node_annotation;
pub mod types;
pub mod visitor;
pub mod visitor_mut;
