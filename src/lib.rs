#![allow(dead_code, unused_variables)]

pub mod ast;
pub mod ast_helpers;

pub mod parser;
pub mod tokenizer;

pub mod semantics;

pub mod type_annotation;
pub mod type_check2;
pub mod type_coercion;
pub mod type_node_annotation;

pub mod ink_codegen;

pub mod compiler;

pub mod error;

pub mod scope_stack;
pub mod source_location;
pub mod string_interner;

pub mod type_interner;
pub mod types;

pub mod visitor;
pub mod visitor_mut;
pub mod visitor_mut_with_context;
pub mod visitor_with_context;
