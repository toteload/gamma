use std::collections::HashMap;
use std::hash::Hash;

pub struct ScopeStack<K, V>
where
    K: Hash + Eq,
{
    stack: Vec<HashMap<K, V>>,
}

impl<K, V> Default for ScopeStack<K, V>
where
    K: Hash + Eq,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> ScopeStack<K, V>
where
    K: Hash + Eq,
{
    pub fn new() -> Self {
        ScopeStack { stack: Vec::new() }
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        for scope in self.stack.iter().rev() {
            if let result @ Some(_) = scope.get(key) {
                return result;
            }
        }

        None
    }

    pub fn push_empty_scope(&mut self) {
        self.stack.push(HashMap::new());
    }

    pub fn push_scope(&mut self, scope: HashMap<K, V>) {
        self.stack.push(scope);
    }

    pub fn pop(&mut self) {
        self.stack.pop();
    }

    pub fn insert(&mut self, key: K, item: V) {
        let top = self.stack.last_mut().expect("Stack should not be empty");
        top.insert(key, item);
    }
}
