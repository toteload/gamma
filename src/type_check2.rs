use crate::{
    ast::*,
    error::{Error, ErrorSource},
    scope_stack::ScopeStack,
    string_interner::Symbol,
    type_interner::{TypeInterner, TypeToken},
    types::{is_valid_type_cast, is_addressable, Type},
    visitor::Visitor,
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

    fn typetoken_of_node(&self, node: &NodeId) -> TypeToken {
        *self.ast_types.get(node).expect("")
    }

    fn check_equal_types(&mut self, left: &NodeId, right: &NodeId) {
        let a = self.typetoken_of_node(left);
        let b = self.typetoken_of_node(right);

        if a == b {
            return;
        }

        todo!()
    }

    fn check_equal_types_at(
        at: ErrorSource,
        expected: &TypeToken,
        actual: &TypeToken,
    ) -> Option<Error> {
        if actual == expected {
            return None;
        }

        Some(Error {
            source: at,
            info: vec![],
        })
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

    fn check_all_args_are_same_type(&self, args: &[Expression]) {
        let arg_types = args
            .iter()
            .map(|arg| *self.ast_types.get(&arg.id).unwrap())
            .collect::<Vec<_>>();
        let all_are_same_type = arg_types.windows(2).all(|w| w[0] == w[1]);
        assert!(all_are_same_type, "{:?}", args);
    }
}

impl Visitor for TypeChecker<'_> {
    fn on_items_enter(&mut self, items: &[Item]) {
        let mut global_scope = HashMap::new();

        for item in items {
            match &item.kind {
                ItemKind::Function { name, .. } | ItemKind::ExternalFunction { name, .. } => {
                    global_scope.insert(name.sym, *self.ast_types.get(&item.id).unwrap());
                }
                _ => {}
            }
        }

        self.scopes.push_scope(global_scope);
    }

    fn on_block_enter(&mut self, _: &Block) {
        self.scopes.push_empty_scope();
    }

    fn on_block_leave(&mut self, _: &Block) {
        self.scopes.pop();
    }

    fn on_function_enter(&mut self, function: &Item) {
        let Type::Function { return_type, .. } = self.type_of_node(&function.id) else {
            panic!()
        };
        self.declared_return_type = Some(*return_type);
    }

    fn on_function_leave(&mut self, _: &Item) {
        self.declared_return_type = None;
    }

    fn on_statement_leave(&mut self, statement: &Statement) {
        use StatementKind::*;

        match &statement.kind {
            Let { name, ty, init } => {
                self.scopes.insert(
                    name.sym,
                    *self
                        .ast_types
                        .get(&ty.id)
                        .expect("Type node should have a registered type"),
                );

                if let Some(init) = init {
                    self.check_equal_types(&ty.id, &init.id);
                }
            }
            Set { dst, val } => {
                self.check_set_destination_type(dst);
                self.check_equal_types(&dst.id, &val.id);
            }
            If { cond, .. } => {
                assert!(matches!(
                    self.typetokens.get(self.ast_types.get(&cond.id).unwrap()),
                    Type::Bool
                ))
            }
            Return(e) => match (e, self.declared_return_type) {
                (Some(e), Some(expected)) => {
                    assert!(*self.ast_types.get(&e.id).unwrap() == expected);
                }
                (None, None) => {}
                _ => todo!(),
            },
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
                Not => todo!(),
                Or | And => todo!(),
                Equals | NotEquals | LessThan | GreaterThan | LessEquals | GreaterEquals => {
                    self.check_all_args_are_same_type(args);
                }
                BitwiseAnd | BitwiseOr | Xor => {
                    self.check_all_args_are_same_type(args);
                }
                Add | Sub | Mul | Div | Remainder => {
                    self.check_all_args_are_same_type(args);
                }
                AddressOf => {
                    assert!(args.len() == 1);
                    assert!(is_addressable(&args[0]));
                }
                At => {
                    let base = &args[0];
                    assert!(matches!(
                        self.typetokens.get(self.ast_types.get(&base.id).unwrap()),
                        Type::Pointer(_) | Type::Array(_, _)
                    ));
                }
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

    type_checker.visit_items(items);

    if !type_checker.errors.is_empty() {
        Err(type_checker.errors)
    } else {
        Ok(())
    }
}
