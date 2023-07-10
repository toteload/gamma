use crate::ast::*;
use crate::ast_visitor::Visitor;
use crate::semantics::{SemanticAnalyser, SemanticError};
use crate::string_interner::{StringInterner, Symbol};

pub struct Analyser {
    main_symbol: Option<Symbol>,
    found_main_function: bool,
}

impl Visitor for Analyser {
    fn visit_function(
        &mut self,
        name: &Name,
        _params: &[Param],
        _return_type: &Type,
        _body: &Block,
    ) {
        if name.sym == self.main_symbol.unwrap() {
            self.found_main_function = true;
        }
    }
}

impl Analyser {
    pub fn new(symbols: &StringInterner) -> Self {
        let main_symbol = symbols.find_symbol("main");

        Self {
            main_symbol,
            found_main_function: false,
        }
    }
}

struct NoMainFunctionError {}

impl SemanticError for NoMainFunctionError {
    fn print(&self, symbols: &StringInterner) {
        println!("ERROR: Program has no main function.");
    }
}

impl SemanticAnalyser for Analyser {
    fn check_for_error(&mut self, items: &[Item]) -> Option<Box<dyn SemanticError>> {
        let Some(main_symbol) = self.main_symbol else { return Some(Box::new(NoMainFunctionError{})); };

        self.visit_items(items);

        if !self.found_main_function {
            return Some(Box::new(NoMainFunctionError {}));
        }

        None
    }
}
