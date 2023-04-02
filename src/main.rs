#![allow(dead_code, unused_variables)]

mod ast;
mod env;
mod parse;
mod semantics;
mod string_interner;
mod types;

use string_interner::StringInterner;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = include_str!("../test001.gamma");

    let mut str_interner = StringInterner::new();
    //let mut tokenizer = parse::Tokenizer::new(source, &mut str_interner).peekable();
    //let tokens = tokenizer.collect::<Vec<_>>();
    //for tok in tokens {
    //    println!("{:?}", tok.kind);
    //}

    let items = parse::parse(source, &mut str_interner)?;

    semantics::do_name_validation(&items);

    //println!("{:?}", items);

    Ok(())
}
