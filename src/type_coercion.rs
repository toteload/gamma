use crate::{
    ast::{self, *},
    ast_visitor::{visit, VisitorMut},
    error::Error,
    scope_stack::ScopeStack,
    string_interner::Symbol,
    types::{Type, TypeInterner, TypeToken},
};
use std::collections::HashMap;

struct TypeCoercer<'a> {
    typetokens: &'a mut TypeInterner,
    ast_types: &'a mut AstMap<TypeToken>,
    typetable: &'a mut HashMap<Symbol, TypeToken>,
    id_generator: &'a mut NodeIdGenerator,
    scopes: ScopeStack<Symbol, TypeToken>,
    errors: Vec<Error>,
}

impl TypeCoercer<'_> {
    fn new<'a>(
        typetokens: &'a mut TypeInterner,
        ast_types: &'a mut AstMap<TypeToken>,
        typetable: &'a mut HashMap<Symbol, TypeToken>,
        id_generator: &'a mut NodeIdGenerator,
    ) -> TypeCoercer<'a> {
        TypeCoercer {
            typetokens,
            ast_types,
            typetable,
            id_generator,
            scopes: ScopeStack::new(),
            errors: Vec::new(),
        }
    }
}

impl VisitorMut for TypeCoercer<'_> {
    fn on_statement_enter(&mut self, statement: &mut Statement) {
        use StatementKind::*;

        match &mut statement.kind {
            Let { ty, init, .. } => {
                if init.is_none() {
                    return;
                }

                let init_is_int_const = matches!(
                    self.typetokens
                        .get(self.ast_types.get(&init.as_ref().unwrap().id).expect("")),
                    Type::IntConstant
                );

                if init_is_int_const {
                    let id = self.id_generator.gen_id();

                    self.ast_types
                        .insert(id, *self.ast_types.get(&ty.id).expect(""));

                    let cast = Box::new(ast::Expression {
                        id: self.id_generator.gen_id(),
                        kind: ExpressionKind::Cast {
                            ty: ast::Type {
                                id,
                                kind: TypeKind::Internal,
                            },
                            e: init.take().unwrap(),
                        },
                    });

                    *init = Some(cast);
                }

                todo!()
            }
            _ => (),
        }
    }

    fn on_expression_enter(&mut self, expression: &mut Expression) {}
}

fn type_coerce() -> Result<(), Vec<Error>> {
    todo!()
}
