use serde::Serialize;
use std::collections::HashMap;
use std::ops::Deref;

#[derive(PartialEq, Debug, Clone, Copy, Hash, Eq, Serialize)]
pub struct Symbol(u32);

pub struct StringInterner {
    symbols: HashMap<&'static str, Symbol>,
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

        let storage = name.to_string().into_boxed_str();

        // Giving the string a 'static lifetime is safe, because the strings
        // can only be accessed while the StringInterner is alive.
        let string: &'static str = unsafe { &*(storage.deref() as *const str) };

        self.strings.push(storage);

        self.symbols.insert(string, sym);

        sym
    }

    pub fn find_symbol(&self, name: &str) -> Option<Symbol> {
        self.symbols.get(name).copied()
    }

    pub fn get_str(&self, symbol: Symbol) -> &str {
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

        assert_eq!(interner.get_str(a), "banana bread");
        assert_eq!(interner.get_str(b), "chocolate");
        assert_eq!(interner.get_str(c), "tuna");

        let d = interner.add("chocolate");

        assert_eq!(b, d);

        assert_eq!(interner.find_symbol("tuna").unwrap(), interner.add("tuna"));
    }
}
