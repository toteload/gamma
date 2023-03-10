use std::collections::HashMap;

type Scope<T> = HashMap<String, T>;

pub struct ScopedEnv<T> {
    scopes: Vec<Scope<T>>,
}

impl<T> ScopedEnv<T> {
    pub fn new() -> ScopedEnv<T> {
        ScopedEnv { scopes: Vec::new() }
    }

    pub fn insert(&mut self, name: &str, val: T) {
        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.to_string(), val);
    }

    pub fn find(&self, name: &str) -> Option<&T> {
        for scope in self.scopes.iter().rev() {
            if let Some(x) = scope.get(name) {
                return Some(x);
            }
        }

        None
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    pub fn pop_scope(&mut self) -> Option<Scope<T>> {
        self.scopes.pop()
    }
}

pub struct Env<'a, T> {
    parent: Option<&'a Env<'a, T>>,
    vars: HashMap<String, T>,
}

impl<'a, T> Env<'a, T> {
    pub fn new(parent: Option<&'a Env<T>>) -> Self {
        Env {
            parent,
            vars: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<&T> {
        let maybe_found = self.vars.get(name);

        if maybe_found.is_some() {
            maybe_found
        } else {
            if let Some(parent) = self.parent {
                parent.get(name)
            } else {
                None
            }
        }
    }

    pub fn insert(&mut self, name: &str, val: T) {
        self.vars.insert(name.to_string(), val);
    }
}
