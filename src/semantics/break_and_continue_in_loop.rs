use crate::error::*;

pub struct Prover<'a> {
    loop_depth: u32,
    errors: Vec<Error>,
    context: &'a SemanticContext<'a>,
}

impl<'a> Prover<'a> {
    pub fn new(_context: &'a SemanticContext) -> Self {
        Self {
            loop_depth: 0,
            errors: Vec::new(),
            context,
        }
    }
}

impl Visitor for Prover<'_> {
    fn on_function_enter(
        &mut self,
        _: &Item
    ) {
        self.loop_depth = 0;
    }

    fn on_statement_enter(&mut self, statement: &Statement) {
        match statement.kind {
            StatementKind::Loop(block) => {
                self.loop_depth += 1;
            }
            StatementKind::Break | StatementKind::Continue => {
                if self.loop_depth > 0 {
                    break;
                }

                let span = *self.context.spans.get(stmt.id).unwrap();
                self.errors.push(Error {
                    span: Some(span),
                    info: vec![Text("Break or continue statement used outside of loop")],
                });
            }
        }
    }
}

impl SemanticProver for Prover {
    fn verify(&mut self, items: &[Item]) -> Result<(), Vec<Error>> {
        visit_items(self, items);

        if !self.errors.is_empty() {
            Err(self.errors.clone())
        } else {
        Ok(())
        }

    }
}

