use gesture::try_destructure_multiple;
use method::Specificity;

use crate::util::{fresh_id, Id};

use self::{
    gesture::{Binding, EvalError},
    method::{Method, MethodTable},
    value::Value,
};
use std::collections::HashMap;

pub mod gesture;
pub mod method;
pub mod value;

pub struct Runtime {
    pub heap: Heap,
}

impl Runtime {
    pub fn new() -> Runtime {
        Runtime { heap: Heap::new() }
    }

    pub fn lookup_method<'a>(
        &'a self,
        env_id: Id<Env>,
        args: &[Value],
    ) -> Result<(&'a Method, Vec<Binding>), Vec<&'a Method>> {
        let methods = self.heap.collect_methods(env_id);

        let matches: Vec<_> = methods
            .iter()
            .filter_map(|&m| match try_destructure_multiple(self, &m.pats, args) {
                Ok(bindings) => Some((m, bindings)),
                Err(_) => None,
            })
            .collect();

        match &matches[..] {
            [] => Err(vec![]),
            [_] => Ok(matches.into_iter().nth(0).unwrap()),
            [(ma, _), (mb, _), ..] => match ma.cmp_specificity(mb) {
                Some(_) => Ok(matches.into_iter().nth(0).unwrap()),
                None => Err(matches.into_iter().map(|(m, _)| m).collect()),
            },
        }
    }
}

pub struct Heap {
    envs: HashMap<Id<Env>, Env>,
}

impl Heap {
    pub fn new() -> Heap {
        Heap { envs: hashmap! {} }
    }

    pub fn store_env(&mut self, env: Env) -> Id<Env> {
        let id = fresh_id();
        self.envs.insert(id, env);
        id
    }

    pub fn alloc_env(&mut self) -> Id<Env> {
        self.store_env(Env::new())
    }

    pub fn get_env(&self, env_id: Id<Env>) -> &Env {
        self.envs.get(&env_id).unwrap()
    }

    pub fn alloc_child_env(&mut self, parent_id: Id<Env>) -> Id<Env> {
        self.store_env(Env::new_under_parent(parent_id))
    }

    pub fn define_var(&mut self, env_id: Id<Env>, name: String, value: Value) {
        let env = self.envs.get_mut(&env_id).unwrap();
        env.items.insert(name, value);
    }

    pub fn set_var(
        &mut self,
        env_id: Id<Env>,
        name: String,
        value: Value,
    ) -> Result<(), EvalError> {
        let env = self.envs.get_mut(&env_id).unwrap();
        if env.items.contains_key(&name) {
            env.items.insert(name, value);
            Ok(())
        } else {
            match env.parent {
                Some(parent) => self.set_var(parent, name, value),
                None => Err(EvalError::Msg(format!(
                    "variable '{}' is not defined",
                    name
                ))),
            }
        }
    }

    pub fn get_var(&self, env_id: Id<Env>, name: &str) -> Result<Value, EvalError> {
        let env = self.envs.get(&env_id).unwrap();
        match (env.items.get(name), env.parent) {
            (Some(v), _) => Ok(v.clone()),
            (None, Some(parent)) => self.get_var(parent, name),
            (None, None) => Err(EvalError::Msg(format!(
                "variable '{}' is not defined",
                name
            ))),
        }
    }

    pub fn collect_methods(&self, env_id: Id<Env>) -> Vec<&Method> {
        let mut env = self.envs.get(&env_id).unwrap();
        let mut methods = Vec::new();
        loop {
            methods.extend(env.method_table.methods());

            env = match env.parent {
                Some(e) => self.envs.get(&e).unwrap(),
                None => break,
            }
        }
        methods
    }
}

pub struct Env {
    parent: Option<Id<Env>>,
    items: HashMap<String, Value>,
    pub method_table: MethodTable,
}

impl Env {
    pub fn new() -> Env {
        Env {
            parent: None,
            items: hashmap! {},
            method_table: MethodTable::new(),
        }
    }

    pub fn new_under_parent(id: Id<Env>) -> Env {
        Env {
            parent: Some(id),
            items: hashmap! {},
            method_table: MethodTable::new(),
        }
    }
}
