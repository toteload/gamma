use crate::ast::BuiltinOpKind;
use crate::ast::*;
use crate::compiler::Context;
use crate::error::*;
use crate::scope_stack::ScopeStack;
use crate::string_interner::Symbol;
use crate::types::{Type, TypeInterner, TypeToken};
use std::collections::HashMap;

struct TypeChecker<'a> {
    type_tokens: &'a mut TypeInterner,
    types: &'a mut HashMap<NodeId, TypeToken>,
    type_table: &'a HashMap<Symbol, TypeToken>,

    scopes: ScopeStack<Symbol, TypeToken>,
    errors: Vec<Error>,

    // This gets set to the declared return type of a function.
    // When we encounter a return statement in the function, the type of the returned value is
    // checked against this.
    declared_return_type: Option<TypeToken>,
}

impl TypeChecker<'_> {
    fn new<'a>(
        type_tokens: &'a mut TypeInterner,
        types: &'a mut HashMap<NodeId, TypeToken>,
        type_table: &'a mut HashMap<Symbol, TypeToken>,
    ) -> TypeChecker<'a> {
        TypeChecker {
            type_tokens,
            types,
            type_table,
            scopes: ScopeStack::new(),
            errors: Vec::new(),
            declared_return_type: None,
        }
    }

    fn is_valid_set_destination(&self, e: &Expr) -> bool {
        matches!(
            e.kind,
            ExprKind::Identifier(_)
                | ExprKind::BuiltinOp {
                    op: BuiltinOpKind::At,
                    ..
                }
        )
    }

    // Check that the found return type matches the declared return type of the function.
    fn check_return_type(&mut self, found_return_type: Option<TypeToken>) {
        if found_return_type != self.declared_return_type {
            self.errors.push(Error {
                kind: ErrorKind::Type,
                span: None,
                info: vec![ErrorInfo::Text("Return types do not match")],
            });
        }
    }

    fn get_type_token_of_type_node_kind(&mut self, kind: &TypeKind) -> Result<TypeToken, Error> {
        match kind {
            TypeKind::Identifier(sym) => self.type_table.get(sym).copied().ok_or(Error {
                kind: ErrorKind::Type,
                span: None,
                info: vec![
                    ErrorInfo::Text("Undefined type identifier encountered: "),
                    ErrorInfo::Identifier(*sym),
                ],
            }),
            TypeKind::Pointer(inner) => {
                let inner = self.get_type_token_of_type_node_kind(inner)?;
                Ok(self.type_tokens.add(Type::Pointer(inner)))
            }
            TypeKind::Array(size, inner) => {
                let inner = self.get_type_token_of_type_node_kind(inner)?;
                Ok(self.type_tokens.add(Type::Array(*size, inner)))
            }
        }
    }

    fn get_type_token_of_sym(&self, sym: &Symbol) -> TypeToken {
        *self.scopes.get(sym).expect("")
    }

    fn is_valid_type_cast(&self, expr: TypeToken, cast_type: TypeToken) -> bool {
        todo!()
    }

    fn visit_items(&mut self, items: &[Item]) {
        let mut top_scope = HashMap::new();

        for item in items {
            match &item.kind {
                ItemKind::Function {
                    name,
                    params: param_nodes,
                    return_type: return_type_node,
                    ..
                } => {
                    let param_type_tokens = param_nodes
                        .iter()
                        .map(|p| self.get_type_token_of_type_node_kind(&p.ty.kind))
                        .collect::<Result<Vec<_>, _>>();

                    let Ok(param_type_tokens) = param_type_tokens else {
                        todo!()
                    };

                    let return_type_token =
                        self.get_type_token_of_type_node_kind(&return_type_node.kind);

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

                    self.declared_return_type = Some(return_type_token)
                        .filter(|tt| *tt != self.type_tokens.add(Type::Void));

                    self.visit_function(name, params, return_type, body);
                }
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
            todo!("Type should be a function")
        };

        let param_symbols = params.iter().map(|p| p.name.sym);
        let param_scope_map = param_symbols
            .zip(param_type_tokens.iter().copied())
            .collect::<HashMap<_, _>>();

        self.scopes.push_scope(param_scope_map);

        self.visit_block(&body);

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
                let type_token = self.get_type_token_of_type_node_kind(&ty.kind);

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
                            kind: ErrorKind::Type,
                            span: None,
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
                // "L-value". Something like `set 0 = 1` would type check, but should not be valid.
                if !self.is_valid_set_destination(dst) {
                    self.errors.push(Error {
                        kind: ErrorKind::Type,
                        span: None,
                        info: vec![ErrorInfo::Text(
                            "Invalid expression for destination in set statement",
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
                            kind: ErrorKind::Type,
                            span: None,
                            info: vec![ErrorInfo::Text("Type mismatch in set statement.")],
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
                            kind: ErrorKind::Type,
                            span: None,
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
            Return(Some(e)) => match self.visit_expr(e) {
                Err(err) => self.errors.push(err),
                Ok(tt) => self.check_return_type(Some(tt)),
            },
            Return(None) => self.check_return_type(None),
            Loop(body) => self.visit_block(body),
            Break | Continue => (),
        }
    }

    fn visit_expr(&mut self, expression: &Expr) -> Result<TypeToken, Error> {
        use ExprKind::*;

        let ty = match &expression.kind {
            IntLiteral(_) => self.type_tokens.add(Type::Int),
            BoolLiteral(_) => self.type_tokens.add(Type::Bool),
            Identifier(sym) => self.get_type_token_of_sym(sym),
            Cast { ty, e } => {
                let expr_ty_token = self.visit_expr(e)?;
                let cast_ty_token = self.get_type_token_of_type_node_kind(&ty.kind)?;

                if !self.is_valid_type_cast(expr_ty_token, cast_ty_token) {
                    return Err(Error {
                        kind: ErrorKind::Type,
                        span: None,
                        info: vec![ErrorInfo::Text("Invalid cast.")],
                    });
                }

                cast_ty_token
            }
            BuiltinOp { op, args } => match op {
                BuiltinOpKind::Equals => {
                    let arg_types = args
                        .iter()
                        .map(|arg| self.visit_expr(arg))
                        .collect::<Result<Vec<_>, _>>()?;
                    let args_are_of_same_type = arg_types.windows(2).all(|w| w[0] == w[1]);

                    if !args_are_of_same_type {
                        return Err(Error {
                            kind: ErrorKind::Type,
                            span: None,
                            info: vec![ErrorInfo::Text("Arguments must have the same type.")],
                        });
                    }

                    self.type_tokens.add(Type::Bool)
                }
                BuiltinOpKind::Mul
                | BuiltinOpKind::Add
                | BuiltinOpKind::Div
                | BuiltinOpKind::Sub => {
                    // TODO: Also check that they are of type int. Right now it is only checked if
                    // they are all the same, but they could all be bools.
                    let arg_types = args
                        .iter()
                        .map(|arg| self.visit_expr(arg))
                        .collect::<Result<Vec<_>, _>>()?;
                    let args_are_of_same_type = arg_types.windows(2).all(|w| w[0] == w[1]);

                    if !args_are_of_same_type {
                        return Err(Error {
                            kind: ErrorKind::Type,
                            span: None,
                            info: vec![ErrorInfo::Text("Arguments must have the same type.")],
                        });
                    }

                    self.type_tokens.add(Type::Int)
                }
                BuiltinOpKind::AddressOf => {
                    let arg_type_token = self.visit_expr(&args[0])?;
                    self.type_tokens.add(Type::Pointer(arg_type_token))
                }
                BuiltinOpKind::At => {
                    let arg_type_token = self.visit_expr(&args[0])?;
                    let ty = self.type_tokens.get(&arg_type_token);

                    match ty {
                        &Type::Pointer(t) => t,
                        &Type::Array(_, t) => {
                            let idx_type_token = self.visit_expr(&args[1])?;
                            let ty = self.type_tokens.get(&idx_type_token);

                            if !matches!(ty, Type::Int) {
                                return Err(Error {
                                    kind: ErrorKind::Type,
                                    span: None,
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
                _ => todo!("Operator \"{:?}\"", op),
            },
            _ => todo!(),
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
