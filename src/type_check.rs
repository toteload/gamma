use crate::ast::BuiltinOpKind;
use crate::ast::*;
use crate::compiler::{Context, PrintableError};
use crate::scope_stack::ScopeStack;
use crate::string_interner::Symbol;
use crate::types::{Type, TypeInterner, TypeToken};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
pub enum TypeError {
    TypeMismatch {
        expected: (TypeToken, NodeId),
        received: (TypeToken, NodeId),
    },
    IfConditionMustBeBool(NodeId),
    ArgumentsMustHaveIdenticalType(NodeId),
    InvalidCast,
    IncorrectReturnType,
    InvalidSetDestination(NodeId),
}

impl PrintableError for TypeError {
    fn print(&self, context: &Context) {
        match self {
            _ => println!("Type checking error: {:?}", self),
        }
    }
}

struct TypeChecker<'a> {
    type_interner: &'a mut TypeInterner,
    types: &'a mut HashMap<NodeId, TypeToken>,

    scopes: ScopeStack<Symbol, TypeToken>,
    type_errors: Vec<TypeError>,

    // This gets set to the declared return type of a function.
    // When we encounter a return statement in the function, the type of the returned value is
    // checked against this.
    declared_return_type: Option<TypeToken>,
}

impl TypeChecker<'_> {
    fn new<'a>(
        type_interner: &'a mut TypeInterner,
        types: &'a mut HashMap<NodeId, TypeToken>,
    ) -> TypeChecker<'a> {
        TypeChecker {
            type_interner,
            types,
            scopes: ScopeStack::new(),
            type_errors: Vec::new(),
            declared_return_type: None,
        }
    }

    fn is_valid_set_destination(&self, e: &Expr) -> bool {
        matches!(e.kind, ExprKind::Identifier(_))
    }

    // Check that the found return type matches the declared return type of the function.
    fn check_return_type(&mut self, found_return_type: Option<TypeToken>) {
        if found_return_type != self.declared_return_type {
            self.type_errors.push(TypeError::IncorrectReturnType);
        }
    }

    fn get_type_token_of_type_node_kind(&mut self, kind: &TypeKind) -> TypeToken {
        match kind {
            TypeKind::Int => self.type_interner.add(Type::Int),
            TypeKind::Void => self.type_interner.add(Type::Void),
            TypeKind::Bool => self.type_interner.add(Type::Bool),
        }
    }

    fn get_type_token_of_sym(&self, sym: &Symbol) -> TypeToken {
        *self.scopes.get(sym).expect("")
    }

    fn is_valid_type_cast(&self, expr: TypeToken, castType: TypeToken) -> bool {
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
                        .collect::<Vec<_>>();

                    let return_type_token =
                        self.get_type_token_of_type_node_kind(&return_type_node.kind);

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

                    let function_type_token = self.type_interner.add(function_type);

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
                        .filter(|tt| *tt != self.type_interner.add(Type::Void));

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
        } = self.type_interner.get(&function_type_token)
        else {
            todo!()
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
            Let { name, ty } => {
                let type_token = self.get_type_token_of_type_node_kind(&ty.kind);
                self.types.insert(ty.id, type_token);
                self.scopes.insert(name.sym, type_token);
            }
            Set { dst, val } => {
                // The types of `val` and `dst` should match. However, `dst` should also be an
                // "L-value". Something like `set 0 = 1` would type check, but should not be valid.
                if !self.is_valid_set_destination(dst) {
                    self.type_errors
                        .push(TypeError::InvalidSetDestination(statement.id));
                }

                let dst_result = self.visit_expr(dst);
                let val_result = self.visit_expr(val);

                if let Err(e) = dst_result {
                    self.type_errors.push(e);
                }

                if let Err(e) = val_result {
                    self.type_errors.push(e);
                }

                match (dst_result, val_result) {
                    (Ok(dst_type), Ok(val_type)) if dst_type != val_type => {
                        self.type_errors.push(TypeError::TypeMismatch {
                            expected: (dst_type, dst.id),
                            received: (val_type, val.id),
                        });
                    }
                    _ => {}
                }
            }
            Expr(e) => {
                if let Err(err) = self.visit_expr(e) {
                    self.type_errors.push(err);
                }
            }
            If {
                cond,
                then,
                otherwise,
            } => {
                match self.visit_expr(cond) {
                    Ok(ty) if ty != self.type_interner.add(Type::Bool) => {
                        self.type_errors
                            .push(TypeError::IfConditionMustBeBool(cond.id));
                    }
                    Err(err) => self.type_errors.push(err),
                    Ok(_) => (),
                }

                self.visit_block(then);
                if let Some(otherwise) = otherwise {
                    self.visit_block(otherwise);
                }
            }
            Return(Some(e)) => match self.visit_expr(e) {
                Err(err) => self.type_errors.push(err),
                Ok(tt) => self.check_return_type(Some(tt)),
            },
            Return(None) => self.check_return_type(None),
            Loop(body) => self.visit_block(body),
            Break | Continue => (),
        }
    }

    fn visit_expr(&mut self, expression: &Expr) -> Result<TypeToken, TypeError> {
        use ExprKind::*;

        let ty = match &expression.kind {
            IntLiteral(_) => self.type_interner.add(Type::Int),
            BoolLiteral(_) => self.type_interner.add(Type::Bool),
            Identifier(sym) => self.get_type_token_of_sym(sym),
            Cast { ty, e } => {
                let expr_ty_token = self.visit_expr(e)?;
                let cast_ty_token = self.get_type_token_of_type_node_kind(&ty.kind);

                if !self.is_valid_type_cast(expr_ty_token, cast_ty_token) {
                    return Err(TypeError::InvalidCast);
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
                        return Err(TypeError::ArgumentsMustHaveIdenticalType(expression.id));
                    }

                    self.type_interner.add(Type::Bool)
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
                        return Err(TypeError::ArgumentsMustHaveIdenticalType(expression.id));
                    }

                    self.type_interner.add(Type::Int)
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
    type_interner: &mut TypeInterner,
    types: &mut HashMap<NodeId, TypeToken>,
) -> Result<(), Vec<TypeError>> {
    let mut type_checker = TypeChecker::new(type_interner, types);

    type_checker.visit_items(items);

    if !type_checker.type_errors.is_empty() {
        Err(type_checker.type_errors)
    } else {
        Ok(())
    }
}
