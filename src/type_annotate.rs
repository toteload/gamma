use crate::{
    ast::{self, *},
    ast_visitor::{visit, Visitor},
    error::*,
    scope_stack::ScopeStack,
    string_interner::Symbol,
    types::{Type, TypeInterner, TypeToken},
};
use std::collections::HashMap;

struct TypeAnnotater<'a> {
    typetokens: &'a mut TypeInterner,
    ast_types: &'a mut AstMap<TypeToken>,
    typetable: &'a mut HashMap<Symbol, TypeToken>,
    scopes: ScopeStack<Symbol, TypeToken>,
    errors: Vec<Error>,
}

impl TypeAnnotater<'_> {
    fn new<'a>(
        typetokens: &'a mut TypeInterner,
        ast_types: &'a mut AstMap<TypeToken>,
        typetable: &'a mut HashMap<Symbol, TypeToken>,
    ) -> TypeAnnotater<'a> {
        TypeAnnotater {
            typetokens,
            ast_types,
            typetable,
            scopes: ScopeStack::new(),
            errors: Vec::new(),
        }
    }

    fn get_typetoken_of_typekind(&mut self, kind: &TypeKind) -> Result<TypeToken, Symbol> {
        match kind {
            TypeKind::Identifier(sym) => self.typetable.get(sym).copied().ok_or(*sym),
            TypeKind::Pointer(inner) => {
                let inner = self.get_typetoken_of_typekind(inner)?;
                Ok(self.typetokens.add(Type::Pointer(inner)))
            }
            TypeKind::Array(size, inner) => {
                let inner = self.get_typetoken_of_typekind(inner)?;
                Ok(self.typetokens.add(Type::Array(*size, inner)))
            }
            _ => unreachable!(),
        }
    }

    fn get_typetoken_of_typenode(&mut self, ty: &ast::Type) -> Result<TypeToken, Error> {
        self.get_typetoken_of_typekind(&ty.kind)
            .map_err(|sym| Error {
                source: ErrorSource::AstNode(ty.id),
                info: vec![
                    ErrorInfo::Text("Identifier is not a valid type: "),
                    ErrorInfo::Identifier(sym),
                ],
            })
    }

    fn get_typetoken_of_identifier(&self, identifier: &Symbol) -> TypeToken {
        self.scopes
            .get(identifier)
            .copied()
            .expect(format!("Symbol {identifier:?} should be found in scope").as_str())
    }

    fn get_type_of_identifier(&self, identifier: &Symbol) -> &Type {
        self.typetokens
            .get(&self.get_typetoken_of_identifier(identifier))
    }
}

impl Visitor for TypeAnnotater<'_> {
    fn on_items_enter(&mut self, items: &[Item]) {
        self.scopes.push_scope(todo!());
    }

    fn on_function_enter(&mut self, function: &Item) {
        todo!("Add parameters to a scope");
    }

    fn on_function_leave(&mut self, _: &Item) {
        self.scopes.pop();
    }

    fn on_block_enter(&mut self, _: &Block) {
        self.scopes.push_empty_scope();
    }

    fn on_block_leave(&mut self, _: &Block) {
        self.scopes.pop();
    }

    fn on_type_enter(&mut self, ty: &ast::Type) {
        match self.get_typetoken_of_typenode(ty) {
            Ok(typetoken) => _ = self.ast_types.insert(ty.id, typetoken),
            Err(e) => {
                self.errors.push(e);
                return;
            }
        }
    }

    fn on_statement_enter(&mut self, statement: &Statement) {
        use StatementKind::*;

        if let Let { name, ty, init } = &statement.kind {
            match self.get_typetoken_of_typenode(ty) {
                Ok(typetoken) => {
                    self.scopes.insert(name.sym, typetoken);
                    self.ast_types.insert(name.id, typetoken);
                }
                Err(e) => {
                    self.errors.push(e);
                    return;
                }
            }
        }
    }

    fn on_expression_leave(&mut self, expression: &Expression) {
        use BuiltinOpKind::*;
        use ExpressionKind::*;

        match &expression.kind {
            IntLiteral(_) => _ = self.typetokens.add(Type::IntConstant),
            BoolLiteral(_) => _ = self.typetokens.add(Type::Bool),
            Identifier(sym) => _ = self.scopes.get(sym).copied().expect(""),
            CompoundIdentifier(idents) => todo!(),
            Cast { ty, e } => {
                self.ast_types
                    .insert(expression.id, *self.ast_types.get(&ty.id).expect(""));
            }
            BuiltinOp { op, args } => match op {
                Not | Or | And | Equals | NotEquals | LessThan | GreaterThan | LessEquals
                | GreaterEquals => _ = self.typetokens.add(Type::Bool),
                AddressOf => {
                    let arg = &args[0];
                    let ty = self.ast_types.get(&arg.id).expect("");
                    self.ast_types
                        .insert(expression.id, self.typetokens.add(Type::Pointer(*ty)));
                }
                At => {
                    let pointee = &args[0];

                    let ty = self
                        .typetokens
                        .get(self.ast_types.get(&pointee.id).expect(""));

                    let t = match ty {
                        Type::Pointer(t) | Type::Array(_, t) => *t,
                        _ => self.typetokens.add(Type::Invalid),
                    };

                    self.ast_types.insert(expression.id, t);
                }
                Xor | BitwiseAnd | BitwiseOr | Add | Sub | Mul | Div | Remainder => {
                    let ty = args
                        .iter()
                        .map(|arg| self.ast_types.get(&arg.id).expect(""))
                        .copied()
                        .find(|arg_type| {
                            matches!(self.typetokens.get(arg_type), Type::Int { .. })
                        })
                        .unwrap_or(self.typetokens.add(Type::IntConstant));
                    self.ast_types.insert(expression.id, ty);
                }
            },
            Call { name, args } => {
                let function_type = self.get_type_of_identifier(&name.sym);
                let Type::Function { return_type, .. } = function_type else {
                    panic!()
                };

                self.ast_types.insert(expression.id, *return_type);
            }
        }
    }
}

pub fn type_annotate(
    items: &[Item],
    typetokens: &mut TypeInterner,
    ast_types: &mut AstMap<TypeToken>,
    typetable: &mut HashMap<Symbol, TypeToken>,
) -> Result<(), Vec<Error>> {
    let mut annotater = TypeAnnotater::new(typetokens, ast_types, typetable);

    visit(&mut annotater, items);

    if !annotater.errors.is_empty() {
        Err(annotater.errors)
    } else {
        Ok(())
    }
}
