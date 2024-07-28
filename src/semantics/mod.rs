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
    let prove_has_main: Box<dyn SemanticProver> = Box::new(has_main::Prover::new(context));
    let prove_valid_symbol_use: Box<dyn SemanticProver> = Box::new(validate_symbol_use::Prover::new(context));

    let mut provers = [
        prove_has_main, 
        prove_valid_symbol_use,
    ];

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

