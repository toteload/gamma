use crate::ast::*;
use crate::ast_visitor::Visitor;
use crate::compiler::{Context, PrintableError};
use crate::semantics::SemanticProver;
use crate::string_interner::Symbol;

struct NoMainFunctionError {}

impl PrintableError for NoMainFunctionError {
    fn print(&self, _: &Context) {
        println!("Error: Program has no main function.");
    }
}

pub struct Prover {
    main_symbol: Option<Symbol>,
    found_main_function: bool,
}

impl Prover {
    pub fn new(_context: &Context) -> Self {
        let main_symbol = _context.symbols.find_symbol("main");

        Self {
            main_symbol,
            found_main_function: false,
        }
    }
}

impl Visitor for Prover {
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

impl SemanticProver for Prover {
    fn verify(&mut self, items: &[Item]) -> Result<(), Vec<Box<dyn PrintableError>>> {
        let Some(main_symbol) = self.main_symbol else {
            return Err(vec![Box::new(NoMainFunctionError {})]);
        };

        self.visit_items(items);

        if !self.found_main_function {
            return Err(vec![Box::new(NoMainFunctionError {})]);
        }

        Ok(())
    }
}
