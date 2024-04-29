use crate::{
    ast::*,
    ast_visitor::{visit, Visitor},
    error::{Error, ErrorSource},
    scope_stack::ScopeStack,
    string_interner::Symbol,
    types::{is_valid_type_cast, Type, TypeInterner, TypeToken},
};
use std::collections::HashMap;

struct TypeChecker<'a> {
    typetokens: &'a mut TypeInterner,
    ast_types: &'a mut AstMap<TypeToken>,
    type_table: &'a mut HashMap<Symbol, TypeToken>,

    scopes: ScopeStack<Symbol, TypeToken>,
    errors: Vec<Error>,

    // This gets set to the declared return type of a function.
    // When we encounter a return statement in the function, the type of the returned value is
    // checked against this.
    declared_return_type: Option<TypeToken>,
}

impl TypeChecker<'_> {
    fn new<'a>(
        typetokens: &'a mut TypeInterner,
        ast_types: &'a mut HashMap<NodeId, TypeToken>,
        type_table: &'a mut HashMap<Symbol, TypeToken>,
    ) -> TypeChecker<'a> {
        TypeChecker {
            typetokens,
            ast_types,
            type_table,
            scopes: ScopeStack::new(),
            errors: Vec::new(),
            declared_return_type: None,
        }
    }

    fn check_equal_types(&mut self, left: &NodeId, right: &NodeId) {}

    fn check_equal_types_at(
        at: ErrorSource,
        expected: &TypeToken,
        actual: &TypeToken,
    ) -> Option<Error> {
        todo!()
    }

    fn check_set_destination_type(&mut self, dst: &Expression) {
        if matches!(
            dst.kind,
            ExpressionKind::Identifier(_)
                | ExpressionKind::CompoundIdentifier(_)
                | ExpressionKind::BuiltinOp {
                    op: BuiltinOpKind::At,
                    ..
                }
        ) {
            return;
        }

        todo!()
    }

    fn type_of_node(&self, id: &NodeId) -> &Type {
        self.typetokens.get(self.ast_types.get(id).expect(""))
    }

    fn check_cast(&mut self, from: &NodeId, to: &NodeId) {
        let is_valid = is_valid_type_cast(self.type_of_node(from), self.type_of_node(to));
        if is_valid {
            return;
        }

        todo!()
    }
}

impl Visitor for TypeChecker<'_> {
    fn on_block_enter(&mut self, _: &Block) {
        self.scopes.push_empty_scope();
    }

    fn on_block_leave(&mut self, _: &Block) {
        self.scopes.pop();
    }

    fn on_statement_enter(&mut self, statement: &Statement) {
        use StatementKind::*;

        match &statement.kind {
            Let { name, ty, init } => {
                self.scopes
                    .insert(name.sym, *self.ast_types.get(&ty.id).expect(""));

                if let Some(init) = init {
                    self.check_equal_types(&ty.id, &init.id);
                }
            }
            Set { dst, val } => {
                self.check_set_destination_type(dst);
                self.check_equal_types(&dst.id, &val.id);
            }
            If { cond, .. } => {
                todo!("Make sure that cond is Bool");
            }
            Return(e) => todo!(),
            _ => (),
        }
    }

    fn on_expression_enter(&mut self, expression: &Expression) {
        use BuiltinOpKind::*;
        use ExpressionKind::*;

        match &expression.kind {
            Cast { ty, e } => self.check_cast(&e.id, &ty.id),
            Call { name, args } => {
                let Type::Function { params, .. } =
                    self.typetokens.get(self.scopes.get(&name.sym).expect(""))
                else {
                    todo!("")
                };

                let arg_typetokens = args
                    .iter()
                    .map(|arg| self.ast_types.get(&arg.id).expect(""));

                for (expected, actual) in params.iter().zip(arg_typetokens) {
                    TypeChecker::check_equal_types_at(ErrorSource::Unspecified, expected, actual);
                }
            }
            BuiltinOp { op, args } => match op {
                Not | Or | And | Equals | NotEquals | LessThan | GreaterThan | LessEquals
                | GreaterEquals => {}
                AddressOf => {
                    assert!(args.len() == 1);

                    todo!()
                }
                At => {
                    let base = &args[0];
                    // base must be a pointer or an array
                    todo!();
                }
                _ => todo!(),
            },
            _ => (),
        }
    }
}

pub fn type_check(
    items: &[Item],
    typetokens: &mut TypeInterner,
    ast_types: &mut HashMap<NodeId, TypeToken>,
    typetable: &mut HashMap<Symbol, TypeToken>,
) -> Result<(), Vec<Error>> {
    let mut type_checker = TypeChecker::new(typetokens, ast_types, typetable);

    visit(&mut type_checker, items);

    if !type_checker.errors.is_empty() {
        Err(type_checker.errors)
    } else {
        Ok(())
    }
}
