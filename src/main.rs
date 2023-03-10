mod ast;
mod ast_interpreter;
mod env;
mod semantics;
mod sexpr;
mod types;

use crate::semantics::is_semantically_correct;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = "
(fn main () int (add 155 2))
(fn add ((a int) (b int)) int (+ a b))";

    let ast = ast::parse(source)?;

    //if !is_semantically_correct(&ast) {
    //    return Err("Program is not semantically correct".into());
    //}

    let exit_code = ast_interpreter::eval(&ast)?;

    println!("{:?}", exit_code);

    Ok(())
}
