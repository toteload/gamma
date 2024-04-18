
struct TypeChecker<'a> {
    typetokens: &'a mut TypeInterner,
    ast_types: &'a mut AstMap<TypeToken>,
    type_table: &'a mut HashMap<Symbol, TypeToken>,

    scopes: ScopeStack<Symbol, TypeToken>,
    errors: Vec<Error>,

    // This gets set to the declared return type of a function.
    // When we encounter a return statement in the function, the type of the returned value is
    // checked against this.
    declared_return_type: TypeToken,
}

impl TypeChecker<'_> {
    fn new<'a>(
        type_tokens: &'a mut TypeInterner,
        types: &'a mut HashMap<NodeId, TypeToken>,
        type_table: &'a mut HashMap<Symbol, TypeToken>,
    ) -> TypeChecker<'a> {
        TypeChecker {
            typetokens,
            types,
            type_table,

            scopes: ScopeStack::new(),
            errors: Vec::new(),

            declared_return_type: void_token,
        }
    }

    fn check_types(&mut self, left: NodeId, right: NodeId) {
    }
}

impl Visitor for TypeChecker {
    fn on_block_enter(&mut self, _: &Block) {
        self.scopes.push_empty_scope();
    }

    fn on_block_leave(&mut self, _: &Block) {
        self.scopes.pop();
    }

    fn on_statement_enter(&mut self, statement: &Statement) {
        use StatementKind::*;

        match statement.kind {
            Let { name, ty, init } => {
                self.scopes.insert(name.sym, self.ast_types.get(ty.id).expect());

                if let Some(init) = init {
                    self.check_types(ty.id, init.id);
                }
            }
            Set { dst, val } => {
                self.check_set_destination_type(dst.id);
                self.check_types(dst.id, val.id);
            }
            If { cond, .. } => {
                todo!("Make sure that cond is Bool");
            }
            Return(e) => todo!(),
            _ => (),
        }
    }

    fn on_expression_enter(&mut self, expression: &Expression) {
        use ExpressionKind::*;

        match expression.kind {
            Cast { ty, e } => self.check_cast(ty.id, e.id),
            Call { name, args } => {
                let Type::Function { params, .. } = self.typetokens.get(self.scopes.get(name.sym).expect("")) else { todo!("") };
                for (expected, actual) in params.zip(&args.iter()) {
                    todo!()
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
                    let base = args[0];
                    // base must be a pointer or an array
                    todo!();
                }
            }
            _ => (),
        }
    }
}
