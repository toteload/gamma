use std::collections::HashMap;

struct Env<'a, T> {
    parent: Option<&'a Env<'a, T>>,
    vars: HashMap<String, T>,
}

impl<'a, T> Env<'a, T> {
    fn new(parent: Option<&'a Env<T>>) -> Env<'a, T> {
        Env {
            parent,
            vars: HashMap::new(),
        }
    }

    fn insert(&mut self, key: String, var: T) {
        self.vars.insert(key, var);
    }

    fn get(&self, key: &String) -> Option<&T> {
        let res = self.vars.get(key);
        if res.is_none() {
            if let Some(parent) = self.parent {
                parent.get(key)
            } else {
                None
            }
        } else {
            res
        }
    }
}
