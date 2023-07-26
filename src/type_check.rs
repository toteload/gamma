use crate::ast::*;
use crate::string_interner::Symbol;
use crate::types::{Type, TypeInterner, TypeToken};
use std::collections::{HashMap, HashSet};

enum TypedScope {
    Parameters(HashMap<Symbol, TypeToken>),
    LetBinding(Symbol, TypeToken),
    Block,
    Global(HashMap<Symbol, TypeToken>),
}

impl TypedScope {
    fn get(&self, sym: &Symbol) -> Option<&TypeToken> {
        use TypedScope::*;

        match self {
            Parameters(map) | Global(map) => map.get(sym),
            LetBinding(name, ty) => {
                if name == sym {
                    Some(ty)
                } else {
                    None
                }
            }
            Block => None,
        }
    }
}

#[derive(Debug)]
pub enum TypeError {
    InvalidUnaryExpr(NodeId),
    TypeMismatch {
        expected: (TypeToken, NodeId),
        received: (TypeToken, NodeId),
    },
    IfConditionMustBeBool(NodeId),
    IllegalUnaryExpression(NodeId),
    IllegalBinaryExpression(NodeId),
    InvalidCast,
}

struct TypeChecker<'a> {
    type_interner: &'a mut TypeInterner,
    types: &'a mut HashMap<NodeId, TypeToken>,

    unary_ops: HashMap<(UnaryOpKind, TypeToken), TypeToken>,
    binary_ops: HashMap<(BinaryOpKind, TypeToken, TypeToken), TypeToken>,

    valid_casts: HashSet<(TypeToken, TypeToken)>,

    scopes: Vec<TypedScope>,
    type_errors: Vec<TypeError>,

    int_type: TypeToken,
    bool_type: TypeToken,
}

impl TypeChecker<'_> {
    fn new<'a>(
        type_interner: &'a mut TypeInterner,
        types: &'a mut HashMap<NodeId, TypeToken>,
    ) -> TypeChecker<'a> {
        let int_type = type_interner.add(&Type::Int);
        let bool_type = type_interner.add(&Type::Bool);

        use BinaryOpKind::*;
        use UnaryOpKind::*;

        let unary_ops = HashMap::from([
            ((Negate, int_type), int_type),
            ((Not, bool_type), bool_type),
            ((Not, int_type), int_type),
        ]);

        let binary_ops = HashMap::from([
            ((Add, int_type, int_type), int_type),
            ((Sub, int_type, int_type), int_type),
            ((Mul, int_type, int_type), int_type),
            ((Div, int_type, int_type), int_type),
            ((LessThan, int_type, int_type), bool_type),
            ((LessEquals, int_type, int_type), bool_type),
            ((GreaterThan, int_type, int_type), bool_type),
            ((GreaterEquals, int_type, int_type), bool_type),
            ((Equals, int_type, int_type), bool_type),
            ((Equals, bool_type, bool_type), bool_type),
            ((NotEquals, int_type, int_type), bool_type),
            ((NotEquals, bool_type, bool_type), bool_type),
            ((LogicalAnd, bool_type, bool_type), bool_type),
            ((LogicalOr, bool_type, bool_type), bool_type),
            ((BitwiseAnd, int_type, int_type), int_type),
            ((BitwiseOr, int_type, int_type), int_type),
            ((Xor, int_type, int_type), int_type),
        ]);

        let valid_casts = HashSet::from([(int_type, bool_type), (bool_type, int_type)]);

        TypeChecker {
            type_interner,
            types,
            unary_ops,
            binary_ops,
            valid_casts,
            scopes: Vec::new(),
            type_errors: Vec::new(),
            int_type,
            bool_type,
        }
    }

    fn is_valid_type_cast(&self, from: TypeToken, to: TypeToken) -> bool {
        self.valid_casts.contains(&(from, to))
    }

    fn get_type_token_of(&self, sym: &Symbol) -> &TypeToken {
        // This should never fail, because the name validation pass should have caught all cases
        // where an undefined symbol is used.

        for scope in self.scopes.iter().rev() {
            if let Some(token) = scope.get(sym) {
                return token;
            }
        }

        panic!("Tried to find the type of an undefined symbol. This should never happen.");
    }

    fn get_type_of(&self, sym: &Symbol) -> &Type {
        return self.type_interner.get(self.get_type_token_of(sym));
    }

    fn get_type_token_for_type_node(&mut self, type_kind: &TypeKind) -> TypeToken {
        match type_kind {
            TypeKind::Int => self.type_interner.add(&Type::Int),
            TypeKind::Void => self.type_interner.add(&Type::Void),
            TypeKind::Bool => self.type_interner.add(&Type::Bool),
        }
    }

    fn visit_items(&mut self, items: &[Item]) {
        let mut global_scope_map = HashMap::new();

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
                        .map(|p| self.get_type_token_for_type_node(&p.ty.kind))
                        .collect::<Vec<_>>();

                    let return_type_token =
                        self.get_type_token_for_type_node(&return_type_node.kind);

                    for (param_node, type_token) in param_nodes.iter().zip(&param_type_tokens) {
                        self.types.insert(param_node.id, *type_token);
                    }

                    self.types.insert(return_type_node.id, return_type_token);

                    let function_type = Type::Function {
                        params: param_type_tokens,
                        return_type: return_type_token,
                    };

                    let function_type_token = self.type_interner.add(&function_type);

                    self.types.insert(item.id, function_type_token);

                    global_scope_map.insert(name.sym, function_type_token);
                }
            }
        }

        self.scopes.push(TypedScope::Global(global_scope_map));

        for item in items {
            match &item.kind {
                ItemKind::Function {
                    name,
                    params,
                    return_type,
                    body,
                } => self.visit_function(name, params, return_type, body),
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
        let Type::Function{ params: param_type_tokens, return_type } = self.get_type_of(&name.sym) else { todo!();};

        let param_symbols = params.iter().map(|p| p.name.sym);
        let param_scope_map = param_symbols
            .zip(param_type_tokens.iter().copied())
            .collect::<HashMap<_, _>>();

        self.scopes.push(TypedScope::Parameters(param_scope_map));

        self.visit_block(&body);

        self.scopes.pop();
    }

    fn visit_block(&mut self, block: &Block) {
        self.scopes.push(TypedScope::Block);

        for statement in &block.statements {
            self.visit_statement(statement);
        }

        // This pops of all the let bindings that were introduced in this block.
        loop {
            if let Some(TypedScope::Block) = self.scopes.pop() {
                break;
            }
        }
    }

    fn visit_statement(&mut self, statement: &Statement) {
        use StatementKind::*;

        match &statement.kind {
            Let { name, ty, init } => {
                let type_of_ty = self.get_type_token_for_type_node(&ty.kind);
                self.types.insert(ty.id, type_of_ty);

                // If this expression does not type check, then how do we continue?
                // A decent solution could be to report the error and assume that the variable
                // should have the declared type. This way we can continue type checking pretending
                // as if everything went right.
                match self.visit_expr(init) {
                    Ok(type_of_init) if type_of_ty != type_of_init => {
                        self.type_errors.push(TypeError::TypeMismatch {
                            expected: (type_of_ty, ty.id),
                            received: (type_of_init, init.id),
                        });
                    }
                    Err(err) => self.type_errors.push(err),
                    Ok(_) => (),
                }

                self.scopes
                    .push(TypedScope::LetBinding(name.sym, type_of_ty));
            }
            Assign { name, val } => {
                let res = self.visit_expr(val);

                assert!(res.is_ok());

                let sym_type_tok = *self.get_type_token_of(&name.sym);
                let val_type_tok = *self.types.get(&val.id).unwrap();

                if sym_type_tok != val_type_tok {
                    self.type_errors.push(TypeError::TypeMismatch {
                        expected: (sym_type_tok, name.id),
                        received: (val_type_tok, val.id),
                    });
                }
            }
            Expr(e) => {
                if let Err(err) = self.visit_expr(e) {
                    self.type_errors.push(err);
                }
            }
            Block(b) | Loop(b) => self.visit_block(b),
            If {
                cond,
                then,
                otherwise,
            } => {
                match self.visit_expr(cond) {
                    Ok(ty) if ty != self.bool_type => {
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
            Return(Some(e)) => {
                if let Err(err) = self.visit_expr(e) {
                    self.type_errors.push(err);
                }
            }
            Empty | Break | Continue | Return(None) => (),
        }
    }

    fn is_unary_op_legal(&self, op: &UnaryOpKind, ty: &TypeToken) -> bool {
        self.unary_ops.get(&(*op, *ty)).is_some()
    }

    fn is_binary_op_legal(&self, op: &BinaryOpKind, lhs: &TypeToken, rhs: &TypeToken) -> bool {
        self.binary_ops.get(&(*op, *lhs, *rhs)).is_some()
    }

    fn visit_expr(&mut self, expression: &Expr) -> Result<TypeToken, TypeError> {
        use ExprKind::*;

        let ty = match &expression.kind {
            IntLiteral(_) => self.int_type,
            BoolLiteral(_) => self.bool_type,
            Identifier(sym) => *self.get_type_token_of(sym),
            Cast { ty, e } => {
                let expr_ty_token = self.visit_expr(e)?;
                let cast_ty_token = self.get_type_token_for_type_node(&ty.kind);

                if !self.is_valid_type_cast(expr_ty_token, cast_ty_token) {
                    return Err(TypeError::InvalidCast);
                }

                cast_ty_token
            }
            UnaryOp { op, e } => {
                let ty = self.visit_expr(e)?;

                let Some(result_type) = self.unary_ops.get(&(*op, ty)) else {
                    return Err(TypeError::IllegalUnaryExpression(expression.id));
                };

                *result_type
            }
            BinaryOp { op, lhs, rhs } => {
                let lty = self.visit_expr(lhs)?;
                let rty = self.visit_expr(rhs)?;

                let Some(result_type) = self.binary_ops.get(&(*op, lty, rty)) else {
                    return Err(TypeError::IllegalBinaryExpression(expression.id));
                };

                *result_type
            }
            Call { name, args } => {
                // First we annotate all the argument nodes with their types.
                for arg in args {
                    self.visit_expr(arg)?;
                }

                let Type::Function{ params, return_type } = self.get_type_of(&name.sym) else { panic!("This should really be a function type."); };

                let arg_types = args.iter().map(|e| self.types.get(&e.id).unwrap());
                let param_types = params.iter();

                let do_arg_and_param_types_match = arg_types.eq(param_types);

                if !do_arg_and_param_types_match {
                    todo!();
                }

                *return_type
            }
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
) -> Vec<TypeError> {
    let mut type_checker = TypeChecker::new(type_interner, types);
    type_checker.visit_items(items);
    type_checker.type_errors
}
