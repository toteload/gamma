use serde::Serialize;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

#[derive(Clone, Debug, Eq, Serialize)]
pub enum Type {
    Void,
    Bool,
    Int,
    Function {
        params: Vec<TypeToken>,
        return_type: TypeToken,
    },
}

impl Type {
    pub fn kind_u8(&self) -> u8 {
        use Type::*;

        match self {
            Void => 0,
            Bool => 1,
            Int => 2,
            Function { .. } => 3,
        }
    }

    pub fn to_string(&self, type_interner: &TypeInterner) -> String {
        use Type::*;

        match &self {
            Void => "void".to_string(),
            Bool => "bool".to_string(),
            Int => "int".to_string(),
            Function {
                params,
                return_type,
            } => {
                let mut s = "fn(".to_string();

                for param in params {
                    s += &type_interner.get(param).to_string(type_interner);
                    s.push_str(", ");
                }

                s.push_str("): ");
                s += &type_interner.get(return_type).to_string(type_interner);
                s
            }
        }
    }
}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let x = self.kind_u8();

        state.write_u8(x);

        if let Type::Function {
            params,
            return_type,
        } = self
        {
            for param in params {
                param.hash(state);
            }

            return_type.hash(state);
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        use Type::*;

        let self_kind = self.kind_u8();
        let other_kind = other.kind_u8();

        if self_kind != other_kind {
            return false;
        }

        if let (
            Function {
                params: xparams,
                return_type: xreturn_type,
            },
            Function {
                params: yparams,
                return_type: yreturn_type,
            },
        ) = (self, other)
        {
            let same_return_type = xreturn_type == yreturn_type;

            let same_params = xparams
                .iter()
                .zip(yparams)
                .fold(true, |acc, (x, y)| acc && x == y);

            return same_return_type && same_params && xparams.len() == yparams.len();
        }

        true
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct TypeToken(u32);

#[derive(Serialize)]
pub struct TypeInterner {
    tokens: HashMap<Type, TypeToken>,
    types: Vec<Type>,
}

impl TypeInterner {
    pub fn new() -> TypeInterner {
        TypeInterner {
            tokens: HashMap::new(),
            types: Vec::new(),
        }
    }

    pub fn add(&mut self, ty: Type) -> TypeToken {
        if let Some(tok) = self.tokens.get(&ty) {
            return *tok;
        }

        let tok = TypeToken(self.types.len() as u32);

        self.types.push(ty.clone());

        self.tokens.insert(ty, tok);

        tok
    }

    pub fn get_for_type(&self, ty: &Type) -> Option<TypeToken> {
        self.tokens.get(&ty).copied()
    }

    pub fn get(&self, tok: &TypeToken) -> &Type {
        &self.types[tok.0 as usize]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_type_interner_usage() {
        use Type::*;

        let mut interner = TypeInterner::new();

        let a = interner.add(Void);
        let b = interner.add(Bool);
        let c = interner.add(Int);

        assert_eq!(a.0, 0);
        assert_eq!(b.0, 1);
        assert_eq!(c.0, 2);

        assert_ne!(a, b);
        assert_ne!(a, c);
        assert_ne!(b, c);

        assert_eq!(interner.get(&a), &Void);
        assert_eq!(interner.get(&b), &Bool);
        assert_eq!(interner.get(&c), &Int);

        let d = interner.add(Function {
            params: vec![c, c, c],
            return_type: b,
        });

        assert_ne!(a, d);
        assert_ne!(b, d);
        assert_ne!(c, d);

        assert_eq!(d.0, 3);

        let e = interner.add(Bool);

        assert_eq!(b, e);

        let f = interner.add(Function {
            params: vec![c, c, c, c],
            return_type: b,
        });

        assert_ne!(d, f);

        assert_eq!(f.0, 4);
    }
}
