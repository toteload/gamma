mod has_main;
mod validate_symbol_use;

use crate::ast::*;
use crate::source_location::SourceSpan;
use crate::string_interner::StringInterner;
use std::collections::HashMap;

pub trait SemanticError {
    fn print(&self, symbols: &StringInterner);
}

pub trait SemanticAnalyser {
    fn check_for_error(&mut self, items: &[Item]) -> Option<Box<dyn SemanticError>>;
}

pub fn validate_all(
    symbols: &StringInterner,
    spans: &HashMap<NodeId, SourceSpan>,
    items: &[Item],
) -> Vec<Box<dyn SemanticError>> {
    let mut errors = Vec::new();

    if let Some(error) = has_main::Analyser::new(symbols).check_for_error(items) {
        errors.push(error);
    }

    if let Some(error) = validate_symbol_use::Analyser::new(spans).check_for_error(items) {
        errors.push(error);
    }

    errors
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
