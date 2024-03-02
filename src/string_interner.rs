use serde::Serialize;
use std::collections::HashMap;

#[derive(PartialEq, Debug, Clone, Copy, Hash, Eq, PartialOrd, Ord, Serialize)]
pub struct Symbol(u32);

#[derive(Serialize)]
pub struct StringInterner {
    symbols: HashMap<Box<str>, Symbol>,
    strings: Vec<Box<str>>,
}

// This implementation also looks good and simple.
// https://github.com/rust-lang/rust/blob/89b9f7b284aacc5f8613438b80e4dd7bdd10549e/compiler/rustc_span/src/symbol.rs#L1741

impl StringInterner {
    pub fn new() -> StringInterner {
        StringInterner {
            symbols: HashMap::new(),
            strings: Vec::new(),
        }
    }

    pub fn add(&mut self, name: &str) -> Symbol {
        if let Some(sym) = self.symbols.get(name) {
            return *sym;
        }

        let sym = Symbol(self.strings.len() as u32);

        let s = name.to_string().into_boxed_str();

        self.strings.push(s.clone());

        self.symbols.insert(s, sym);

        sym
    }

    pub fn find_symbol(&self, name: &str) -> Option<Symbol> {
        self.symbols.get(name).copied()
    }

    pub fn get(&self, symbol: &Symbol) -> &str {
        &self.strings[symbol.0 as usize]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_string_interner_usage() {
        let mut interner = StringInterner::new();

        let a = interner.add("banana bread");
        let b = interner.add("chocolate");
        let c = interner.add("tuna");
        let d = interner.add("tuna");

        assert_eq!(c, d);

        assert_eq!(interner.get(&a), "banana bread");
        assert_eq!(interner.get(&b), "chocolate");
        assert_eq!(interner.get(&c), "tuna");

        let d = interner.add("chocolate");

        assert_eq!(b, d);

        assert_eq!(interner.find_symbol("tuna").unwrap(), interner.add("tuna"));
    }
}
