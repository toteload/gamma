use std::cmp::Eq;
use std::collections::HashMap;
use std::hash::Hash;

pub struct Env<'a, K, V>
where
    K: Hash + Eq,
{
    parent: Option<&'a Env<'a, K, V>>,
    vars: HashMap<K, V>,
}

impl<'a, K, V> Env<'a, K, V>
where
    K: Hash + Eq,
{
    pub fn new(parent: Option<&'a Env<K, V>>) -> Self {
        Env {
            parent,
            vars: HashMap::new(),
        }
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        let maybe_found = self.vars.get(key);

        if maybe_found.is_some() {
            maybe_found
        } else if let Some(parent) = self.parent {
            parent.get(key)
        } else {
            None
        }
    }

    pub fn insert(&mut self, key: K, val: V) {
        self.vars.insert(key, val);
    }
}
