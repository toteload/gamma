mod has_main;
mod validate_symbol_use;

use crate::ast::*;
use crate::error::Error;
use crate::source_location::SourceSpan;
use crate::string_interner::StringInterner;
use std::collections::HashMap;

pub struct SemanticContext<'a> {
    pub symbols: &'a StringInterner,
    pub spans: &'a HashMap<NodeId, SourceSpan>,
}

pub trait SemanticProver {
    fn verify(&mut self, items: &[Item]) -> Result<(), Vec<Error>>;
}

pub fn validate_semantics(context: &SemanticContext, items: &[Item]) -> Result<(), Vec<Error>> {
    let x: Box<dyn SemanticProver> = Box::new(has_main::Prover::new(&context));
    let y: Box<dyn SemanticProver> = Box::new(validate_symbol_use::Prover::new(&context));

    let mut provers = vec![x, y];

    let errors: Vec<_> = provers
        .iter_mut()
        .filter_map(|prover| prover.verify(items).err())
        .flatten()
        .collect();

    if !errors.is_empty() {
        return Err(errors);
    }

    Ok(())
}

/*
#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::Parser;

    #[test]
    fn name_validation_test() {
        let source = "
        fn main() -> void {
            let a: int = x;
            let b: int = a;
            let c: int = c;
        }";

        let mut string_interner = StringInterner::new();
        let mut id_generator = NodeIdGenerator::new();
        let mut spans = HashMap::new();

        let mut parser = Parser::new(source, &mut string_interner, &mut id_generator, &mut spans);

        let items = parser.parse_items().unwrap();

        let res = do_name_validation(&spans, &items);

        assert!(res.is_err());

        let violations = res.unwrap_err();

        let x_sym = string_interner.find_symbol("x").unwrap();
        let c_sym = string_interner.find_symbol("c").unwrap();

        assert_eq!(violations[0].sym, x_sym);
        assert_eq!(violations[1].sym, c_sym);
    }
}
*/
