use crate::ast::*;
use crate::ast_visitor::Visitor;
use crate::compiler::Context;
use crate::error::*;
use crate::semantics::{SemanticContext, SemanticProver};
use crate::string_interner::Symbol;

pub struct Prover {
    main_symbol: Option<Symbol>,
    found_main_function: bool,
}

impl Prover {
    pub fn new(context: &SemanticContext) -> Self {
        let main_symbol = context.symbols.find_symbol("main");

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
    fn verify(&mut self, items: &[Item]) -> Result<(), Vec<Error>> {
        let no_main_defined_error: Error = Error {
            span: None,
            info: vec![ErrorInfo::Text("No main function defined")],
        };

        let Some(main_symbol) = self.main_symbol else {
            return Err(vec![no_main_defined_error]);
        };

        self.visit_items(items);

        if !self.found_main_function {
            return Err(vec![no_main_defined_error]);
        }

        Ok(())
    }
}
