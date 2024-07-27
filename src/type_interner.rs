use crate::string_interner::StringInterner;
use crate::types::Type;
use serde::Serialize;
use std::cell::Cell;
use std::collections::HashMap;
use std::ops::Deref;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct TypeToken(u32);

pub struct TypeInterner {
    tokens: Cell<HashMap<&'static Type, TypeToken>>,

    #[allow(clippy::vec_box)]
    types: Cell<Vec<Box<Type>>>,
}

impl Default for TypeInterner {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeInterner {
    pub fn new() -> TypeInterner {
        TypeInterner {
            tokens: Cell::new(HashMap::new()),
            types: Cell::new(Vec::new()),
        }
    }

    pub fn add(&self, ty: Type) -> TypeToken {
        let tokens = unsafe { &mut (*self.tokens.as_ptr()) };
        let types = unsafe { &mut (*self.types.as_ptr()) };

        if let Some(tok) = tokens.get(&ty) {
            return *tok;
        }

        let tok = TypeToken(types.len() as u32);

        let ty = Box::new(ty);

        let ty_ref: &Type = unsafe { &*(ty.deref() as *const Type) };

        types.push(ty);

        tokens.insert(ty_ref, tok);

        tok
    }

    pub fn find_token(&self, ty: &Type) -> Option<TypeToken> {
        let tokens = unsafe { &(*self.tokens.as_ptr()) };
        tokens.get(ty).copied()
    }

    pub fn find_token_unchecked(&self, ty: &Type) -> TypeToken {
        let tokens = unsafe { &(*self.tokens.as_ptr()) };
        *tokens.get(ty).expect("Type should be interned")
    }

    pub fn get(&self, tok: &TypeToken) -> &Type {
        let types = unsafe { &(*self.types.as_ptr()) };
        &types[tok.0 as usize]
    }

    pub fn to_string(&self, symbols: &StringInterner) -> String {
        let mut s = String::new();
        let types = unsafe { &(*self.types.as_ptr()) };
        for (i, t) in types.iter().enumerate() {
            s += format!("{} = {}\n", i, t.to_string(self, symbols)).as_str();
        }
        s
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::*;

    #[test]
    fn basic_type_interner_usage() {
        use Type::*;

        let interner = TypeInterner::new();

        let a = interner.add(Void);
        let b = interner.add(Bool);
        let c = interner.add(Int {
            signedness: Signedness::Unsigned,
            width: 32,
        });

        assert_eq!(a.0, 0);
        assert_eq!(b.0, 1);
        assert_eq!(c.0, 2);

        assert_ne!(a, b);
        assert_ne!(a, c);
        assert_ne!(b, c);

        assert_eq!(interner.get(&a), &Void);
        assert_eq!(interner.get(&b), &Bool);
        assert_eq!(
            interner.get(&c),
            &Int {
                signedness: Signedness::Unsigned,
                width: 32
            }
        );

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
