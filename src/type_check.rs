use crate::ast::BuiltinOpKind;
use crate::ast::*;
use crate::error::*;
use crate::scope_stack::ScopeStack;
use crate::string_interner::Symbol;
use crate::types::{Layout, LayoutField, Signedness, Type, TypeInterner, TypeToken};
use std::collections::HashMap;

struct TypeChecker<'a> {
    type_tokens: &'a mut TypeInterner,
    types: &'a mut HashMap<NodeId, TypeToken>,
    type_table: &'a mut HashMap<Symbol, TypeToken>,

    scopes: ScopeStack<Symbol, TypeToken>,
    errors: Vec<Error>,

    // This gets set to the declared return type of a function.
    // When we encounter a return statement in the function, the type of the returned value is
    // checked against this.
    declared_return_type: TypeToken,
    void_token: TypeToken,
}

impl TypeChecker<'_> {
    fn new<'a>(
        type_tokens: &'a mut TypeInterner,
        types: &'a mut HashMap<NodeId, TypeToken>,
        type_table: &'a mut HashMap<Symbol, TypeToken>,
    ) -> TypeChecker<'a> {
        let void_token = type_tokens.add(Type::Void);
        TypeChecker {
            type_tokens,
            types,
            type_table,
            scopes: ScopeStack::new(),
            errors: Vec::new(),
            declared_return_type: void_token,
            void_token,
        }
    }

    fn is_valid_set_destination(&self, e: &Expr) -> bool {
        matches!(
            e.kind,
            ExprKind::Identifier(_)
                | ExprKind::CompoundIdentifier(_)
                | ExprKind::BuiltinOp {
                    op: BuiltinOpKind::At,
                    ..
                }
        )
    }

    fn get_type_token_of_type_kind(&mut self, kind: &TypeKind) -> Result<TypeToken, Symbol> {
        match kind {
            TypeKind::Identifier(sym) => self.type_table.get(sym).copied().ok_or(*sym),
            TypeKind::Pointer(inner) => {
                let inner = self.get_type_token_of_type_kind(inner)?;
                Ok(self.type_tokens.add(Type::Pointer(inner)))
            }
            TypeKind::Array(size, inner) => {
                let inner = self.get_type_token_of_type_kind(inner)?;
                Ok(self.type_tokens.add(Type::Array(*size, inner)))
            }
        }
    }

    fn get_type_token_of_type_node(&mut self, ty: &crate::ast::Type) -> Result<TypeToken, Error> {
        self.get_type_token_of_type_kind(&ty.kind)
            .map_err(|sym| Error {
                source: ErrorSource::AstNode(ty.id),
                info: vec![
                    ErrorInfo::Text("Identifier is not a valid type: "),
                    ErrorInfo::Identifier(sym),
                ],
            })
    }

    fn get_type_token_of_sym(&self, sym: &Symbol) -> TypeToken {
        *self.scopes.get(sym).unwrap()
    }

    fn get_type_of_sym(&self, sym: &Symbol) -> &Type {
        self.type_tokens.get(&self.get_type_token_of_sym(sym))
    }

    fn is_valid_type_cast(&self, expr: TypeToken, cast_type: TypeToken) -> bool {
        use Type::*;
        let dst_ty = self.type_tokens.get(&cast_type);
        let expr_ty = self.type_tokens.get(&expr);
        match (dst_ty, expr_ty) {
            (Int { .. }, Int { .. }) => true,
            (Int { .. }, Bool) => true,
            (Bool, Int { .. }) => true,
            (Pointer(_), Int { .. }) => true,
            (Pointer(ta), Array(_, tb)) if ta == tb => true,
            _ => false,
        }
    }

    fn visit_items(&mut self, items: &[Item]) {
        let mut top_scope = HashMap::new();

        // 1. Register all the layouts as these may be referenced in `fn` and `external_fn`.
        // 2. Register all the functions and external_fn
        // 3. Type check all the `fn` bodies.

        for item in items {
            match &item.kind {
                ItemKind::Layout {
                    name,
                    fields: ast_fields,
                } => {
                    let fields = ast_fields
                        .iter()
                        .map(|Field { name, offset, ty }| {
                            self.get_type_token_of_type_node(ty).map(|ty| LayoutField {
                                name: name.sym,
                                offset: *offset,
                                ty,
                            })
                        })
                        .collect::<Result<Vec<_>, _>>();

                    let Ok(fields) = fields else { todo!() };

                    let layout_type = Type::Layout(Layout { fields });

                    let layout_type_token = self.type_tokens.add(layout_type);

                    self.type_table.insert(name.sym, layout_type_token);
                }
                _ => (),
            }
        }

        for item in items {
            match &item.kind {
                ItemKind::Function {
                    name,
                    params: param_nodes,
                    return_type: return_type_node,
                    ..
                }
                | ItemKind::ExternalFunction {
                    name,
                    params: param_nodes,
                    return_type: return_type_node,
                } => {
                    let param_type_tokens = param_nodes
                        .iter()
                        .map(|p| self.get_type_token_of_type_node(&p.ty))
                        .collect::<Result<Vec<_>, _>>();

                    let Ok(param_type_tokens) = param_type_tokens else {
                        todo!()
                    };

                    let return_type_token = self.get_type_token_of_type_node(return_type_node);

                    let Ok(return_type_token) = return_type_token else {
                        todo!()
                    };

                    for (param_node, type_token) in param_nodes.iter().zip(&param_type_tokens) {
                        // Type annotate the parameter nodes and their type nodes
                        self.types.insert(param_node.id, *type_token);
                        self.types.insert(param_node.name.id, *type_token);
                        self.types.insert(param_node.ty.id, *type_token);
                    }

                    // Type annotate node for the function's return type
                    self.types.insert(return_type_node.id, return_type_token);

                    let function_type = Type::Function {
                        params: param_type_tokens,
                        return_type: return_type_token,
                    };

                    let function_type_token = self.type_tokens.add(function_type);

                    // Type annotate the function
                    self.types.insert(item.id, function_type_token);

                    top_scope.insert(name.sym, function_type_token);
                }
                _ => (),
            }
        }

        self.scopes.push_scope(top_scope);

        for item in items {
            match &item.kind {
                ItemKind::Function {
                    name,
                    params,
                    return_type,
                    body,
                } => {
                    let return_type_token = *self.types.get(&return_type.id).unwrap();

                    self.declared_return_type = return_type_token;
                    //Some(return_type_token)
                    //.filter(|tt| *tt != self.type_tokens.add(Type::Void));

                    self.visit_function(name, params, return_type, body);
                }
                ItemKind::ExternalFunction { .. } | ItemKind::Layout { .. } => (),
            }
        }
    }

    fn visit_function(
        &mut self,
        name: &Name,
        params: &[Param],
        _return_type: &crate::ast::Type,
        body: &Block,
    ) {
        let function_type_token = self.get_type_token_of_sym(&name.sym);
        let Type::Function {
            params: param_type_tokens,
            return_type,
        } = self.type_tokens.get(&function_type_token)
        else {
            panic!("Type should be a function")
        };

        let param_symbols = params.iter().map(|p| p.name.sym);
        let param_scope_map = param_symbols
            .zip(param_type_tokens.iter().copied())
            .collect::<HashMap<_, _>>();

        self.scopes.push_scope(param_scope_map);

        self.visit_block(body);

        self.scopes.pop();
    }

    fn visit_block(&mut self, block: &Block) {
        self.scopes.push_empty_scope();

        for statement in &block.statements {
            self.visit_statement(statement);
        }
    }

    fn visit_statement(&mut self, statement: &Statement) {
        use StatementKind::*;

        match &statement.kind {
            Let { name, ty, init } => {
                let type_token = self.get_type_token_of_type_node(ty);

                let type_token = match type_token {
                    Ok(x) => x,
                    Err(e) => {
                        self.errors.push(e);
                        return;
                    }
                };

                self.types.insert(ty.id, type_token);
                self.scopes.insert(name.sym, type_token);

                if let Some(init) = init {
                    let init_type_token = match self.visit_expr(init) {
                        Ok(x) => x,
                        Err(e) => {
                            self.errors.push(e);
                            return;
                        }
                    };

                    if type_token != init_type_token {
                        self.errors.push(Error {
                            source: ErrorSource::AstNode(statement.id),
                            info: vec![
                                ErrorInfo::Text("Type mismatch between declared type and type of initializer in set statement"),
                            ],
                        });
                    }
                }
            }
            Set { dst, val } => {
                let dst_result = self.visit_expr(dst);
                let val_result = self.visit_expr(val);

                // The types of `val` and `dst` should match. However, `dst` should also be an
                // "L-value". Something like `set 0 = 1` would type check (in the sense that the
                // left and the right are both integers), but should not be valid.
                if !self.is_valid_set_destination(dst) {
                    self.errors.push(Error {
                        source: ErrorSource::AstNode(statement.id),
                        info: vec![ErrorInfo::Text(
                            "Invalid destination in set statement. It should be an identifier or an @-expression.",
                        )],
                    });
                }

                if let Err(e) = &dst_result {
                    self.errors.push(e.clone());
                }

                if let Err(e) = &val_result {
                    self.errors.push(e.clone());
                }

                match (&dst_result, &val_result) {
                    (Ok(dst_type), Ok(val_type)) if dst_type != val_type => {
                        self.errors.push(Error {
                            source: ErrorSource::AstNode(statement.id),
                            info: vec![
                                ErrorInfo::Text("Type mismatch in set statement. "),
                                ErrorInfo::Text("Destination type is "),
                                ErrorInfo::Type(*dst_type),
                                ErrorInfo::Text(", but value type is "),
                                ErrorInfo::Type(*val_type),
                            ],
                        });
                    }
                    _ => {}
                }
            }
            Expr(e) => {
                if let Err(err) = self.visit_expr(e) {
                    self.errors.push(err);
                }
            }
            If {
                cond,
                then,
                otherwise,
            } => {
                match self.visit_expr(cond) {
                    Ok(ty) if ty != self.type_tokens.add(Type::Bool) => {
                        self.errors.push(Error {
                            source: ErrorSource::AstNode(cond.id),
                            info: vec![ErrorInfo::Text(
                                "Type of condition in if statement must be bool.",
                            )],
                        });
                    }
                    Err(err) => self.errors.push(err),
                    Ok(_) => (),
                }

                self.visit_block(then);
                if let Some(otherwise) = otherwise {
                    self.visit_block(otherwise);
                }
            }
            Return(e) => {
                let found_return_type = match e {
                    Some(e) => match self.visit_expr(e) {
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                        Ok(tt) => tt,
                    },
                    None => self.void_token,
                };

                if found_return_type != self.declared_return_type {
                    self.errors.push(Error {
                        source: ErrorSource::AstNode(statement.id),
                        info: vec![ErrorInfo::Text("Return types do not match")],
                    });
                }
            }
            Loop(body, label) => self.visit_block(body),
            Break(_) | Continue(_) => (),
        }
    }

    fn visit_expr(&mut self, expression: &Expr) -> Result<TypeToken, Error> {
        use ExprKind::*;

        let ty = match &expression.kind {
            IntLiteral(_) => self.type_tokens.add(Type::Int {
                signedness: Signedness::Signed,
                width: 64,
            }),
            BoolLiteral(_) => self.type_tokens.add(Type::Bool),
            Identifier(sym) => {
                if let Some(t) = self.scopes.get(sym) {
                    *t
                } else {
                    panic!()
                    //return Err(Error {
                    //    source: ErrorSource::AstNode(expression.id),
                    //    info: vec![
                    //        ErrorInfo::Text("Undefined symbol used: "),
                    //        ErrorInfo::Identifier(*sym),
                    //    ],
                    //});
                }
            }
            CompoundIdentifier(idents) => {
                let Some(t) = self.scopes.get(&idents[0]) else {
                    panic!()
                };

                let mut ttok = *t;
                for sym in idents[1..].iter() {
                    let layout = self.type_tokens.get(&ttok);
                    let Type::Layout(layout) = layout else {
                        todo!()
                    };
                    let Some(field) = layout.fields.iter().find(|field| field.name == *sym) else {
                        return Err(Error {
                            source: ErrorSource::Unspecified,
                            info: vec![
                                ErrorInfo::Text("Could not find field in layout: "),
                                ErrorInfo::Identifier(*sym),
                                ErrorInfo::Type(ttok),
                                ErrorInfo::Identifier(idents[0]),
                                ErrorInfo::Type(*self.scopes.get(&idents[0]).unwrap()),
                            ],
                        });
                    };
                    ttok = field.ty;
                }
                ttok
            }
            Cast { ty, e } => {
                let expr_ty_token = self.visit_expr(e)?;
                let cast_ty_token = self.get_type_token_of_type_node(ty)?;

                if !self.is_valid_type_cast(expr_ty_token, cast_ty_token) {
                    return Err(Error {
                        source: ErrorSource::AstNode(expression.id),
                        info: vec![
                            ErrorInfo::Text("Invalid cast. From "),
                            ErrorInfo::Type(expr_ty_token),
                            ErrorInfo::Text(" to "),
                            ErrorInfo::Type(cast_ty_token),
                        ],
                    });
                }

                cast_ty_token
            }
            BuiltinOp { op, args } => match op {
                BuiltinOpKind::Equals | BuiltinOpKind::NotEquals => {
                    let arg_types = args
                        .iter()
                        .map(|arg| self.visit_expr(arg))
                        .collect::<Result<Vec<_>, _>>()?;

                    let args_are_of_same_type = arg_types.windows(2).all(|w| w[0] == w[1]);

                    if !args_are_of_same_type {
                        let mut info =
                            vec![ErrorInfo::Text("Arguments must all have the same type.\n")];
                        for (arg, arg_type) in args.iter().zip(arg_types) {
                            info.push(ErrorInfo::AstNode(arg.id));
                            info.push(ErrorInfo::Text(" has type "));
                            info.push(ErrorInfo::Type(arg_type));
                            info.push(ErrorInfo::Text("\n"));
                        }
                        return Err(Error {
                            source: ErrorSource::AstNode(expression.id),
                            info,
                        });
                    }

                    self.type_tokens.add(Type::Bool)
                }
                BuiltinOpKind::Mul
                | BuiltinOpKind::Add
                | BuiltinOpKind::Div
                | BuiltinOpKind::Sub => {
                    assert!(!args.is_empty());

                    let arg_types = args
                        .iter()
                        .map(|arg| self.visit_expr(arg))
                        .collect::<Result<Vec<_>, _>>()?;

                    let args_are_of_same_type = arg_types.windows(2).all(|w| w[0] == w[1]);

                    if !args_are_of_same_type {
                        return Err(Error {
                            source: ErrorSource::AstNode(expression.id),
                            info: vec![ErrorInfo::Text("Arguments must have the same type.")],
                        });
                    }

                    arg_types[0]
                }
                BuiltinOpKind::AddressOf => {
                    let arg_type_token = self.visit_expr(&args[0])?;
                    self.type_tokens.add(Type::Pointer(arg_type_token))
                }
                BuiltinOpKind::At => {
                    let arg_type_token = self.visit_expr(&args[0])?;
                    let ty = self.type_tokens.get(&arg_type_token);

                    match *ty {
                        Type::Pointer(t) => t,
                        Type::Array(_, t) => {
                            let idx_type_token = self.visit_expr(&args[1])?;
                            let ty = self.type_tokens.get(&idx_type_token);

                            if !matches!(ty, Type::Int { .. }) {
                                return Err(Error {
                                    source: ErrorSource::AstNode(expression.id),
                                    info: vec![
                                        ErrorInfo::Text("Array index must have int type, but was "),
                                        ErrorInfo::Type(idx_type_token),
                                    ],
                                });
                            }

                            t
                        }
                        _ => todo!(),
                    }
                }
                BuiltinOpKind::Not => {
                    let [arg] = args.as_slice() else { todo!() };
                    let arg_type_token = self.visit_expr(&arg)?;
                    let ty = self.type_tokens.get(&arg_type_token);

                    if !matches!(ty, Type::Bool) {
                        todo!()
                    }

                    arg_type_token
                }
                BuiltinOpKind::Remainder => {
                    if args.len() != 2 {
                        todo!("remainder must have 2 arguments")
                    }

                    let arg_types = args
                        .iter()
                        .map(|arg| self.visit_expr(arg))
                        .collect::<Result<Vec<_>, _>>()?;
                    let args_are_of_same_type = arg_types.windows(2).all(|w| w[0] == w[1]);
                    let t = self.type_tokens.get(&arg_types[0]);

                    if !args_are_of_same_type || !matches!(t, Type::Int { .. }) {
                        todo!("remainder can only be used with same integer types");
                    }

                    arg_types[0]
                }
                BuiltinOpKind::And => {
                    todo!();
                    self.type_tokens.add(Type::Bool)
                }
                BuiltinOpKind::BitwiseAnd | BuiltinOpKind::BitwiseOr | BuiltinOpKind::Xor => {
                    let arg_types = args
                        .iter()
                        .map(|arg| self.visit_expr(arg))
                        .collect::<Result<Vec<_>, _>>()?;
                    let args_are_of_same_type = arg_types.windows(2).all(|w| w[0] == w[1]);
                    let t = self.type_tokens.get(&arg_types[0]);

                    if !args_are_of_same_type || !matches!(t, Type::Int { .. }) {
                        todo!("operator can only be used with same integer types");
                    }

                    arg_types[0]
                }
                _ => todo!("Operator \"{:?}\"", op),
            },
            Call { name, args } => {
                let function_type = self.get_type_of_sym(&name.sym);
                let Type::Function {
                    return_type,
                    params,
                } = function_type
                else {
                    todo!()
                };

                // TODO Maybe this can be improved?
                // I was force to copy/clone things here, because otherwise the compiler complained
                // about lifetimes. I don't really understand why :(
                let return_type = *return_type;
                let params = params.clone();

                self.types
                    .insert(name.id, self.get_type_token_of_sym(&name.sym));

                let arg_types = args
                    .iter()
                    .map(|arg| self.visit_expr(arg))
                    .collect::<Result<Vec<TypeToken>, _>>()?;

                if params.len() != arg_types.len() {
                    todo!()
                }

                for (expected, actual) in params.iter().zip(&arg_types) {
                    if expected != actual {
                        return Err(Error {
                            source: ErrorSource::Unspecified, // TODO(david) supply the actual location
                            info: vec![
                                ErrorInfo::Text("Expected type "),
                                ErrorInfo::Type(*expected),
                                ErrorInfo::Text(", but got "),
                                ErrorInfo::Type(*actual),
                            ],
                        });
                    }
                }

                return_type
            }
        };

        // Save the type for this AST node.
        self.types.insert(expression.id, ty);

        Ok(ty)
    }
}

pub fn type_check(
    items: &[Item],
    type_tokens: &mut TypeInterner,
    types: &mut HashMap<NodeId, TypeToken>,
    type_table: &mut HashMap<Symbol, TypeToken>,
) -> Result<(), Vec<Error>> {
    let mut type_checker = TypeChecker::new(type_tokens, types, type_table);

    type_checker.visit_items(items);

    if !type_checker.errors.is_empty() {
        Err(type_checker.errors)
    } else {
        Ok(())
    }
}
