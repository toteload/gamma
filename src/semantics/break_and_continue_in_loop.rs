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
    fn visit_function(
        &mut self,
        _name: &Name,
        _params: &[Param],
        _return_type: &Type,
        body: &Block,
    ) {
        self.loop_depth = 0;
        self.visit_block(body);
    }

    fn visit_statement(&mut self, stmt: &Statement) {
        match stmt.kind {
            StatementKind::Loop(block) => {
                self.loop_depth += 1;
                self.visit_block(block);
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
        self.visit_items();

        if !self.errors.is_empty() {
            return Err(self.errors.clone());
        }

        Ok(())
    }
}

