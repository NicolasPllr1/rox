use std::collections::HashMap;

use crate::ast::LoxValue;
use crate::token::Token;

#[derive(Debug, Clone)]
pub struct Env {
    enclosing: Option<Box<Env>>, // parent environement. To implement scope.
    values: HashMap<String, LoxValue>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            enclosing: None,
            values: HashMap::new(),
        }
    }

    pub fn new_from(enclosing: &Env) -> Env {
        Env {
            enclosing: Some(Box::new(enclosing.clone())),
            values: HashMap::new(),
        }
    }

    pub fn set_enclosing_env(&mut self, enclosing: Env) {
        self.enclosing = Some(Box::new(enclosing));
    }
    pub fn define(&mut self, name: &str, value: LoxValue) {
        self.values.insert(name.to_owned(), value);
    }

    pub fn assign(&mut self, name: &Token, value: LoxValue) {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme.to_owned(), value);
        } else if let Some(parent_env) = &mut self.enclosing {
            parent_env.assign(name, value);
        } else {
            let var_name = &name.lexeme;
            panic!("Undefined variable {var_name}")
        }
    }

    pub fn get(&self, name: &str) -> &LoxValue {
        match self.values.get(name) {
            Some(val) => val,
            None => {
                // NOTE: the importance of as_ref():
                // self.enclosing.as_ref().map(|env| env.get(name)).expect(
                //     "Varibale with name {name} not found in environement nor enclosing envs",
                // )

                // NOTE: the importance of borrowing:
                if let Some(parent_env) = &self.enclosing {
                    println!("parent env: {parent_env:?}");
                    parent_env.get(name)
                } else {
                    panic!("Undefined variable {name}")
                }
            }
        }
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]

mod tests {

    use super::*;

    #[test]
    fn test_nested_env() {
        let mut env1 = Env::new();

        let var1 = LoxValue::Number(1.0);
        let var2 = LoxValue::Number(2.0);
        let var3 = LoxValue::Number(3.0);

        env1.define("var1", var1);

        let mut env2 = Env::new_from(&env1);
        env2.define("var2", var2);
        env2.define("var3", var3);

        dbg!(&env1);
        dbg!(&env2);
        assert!(*env1.get("var3") == LoxValue::Number(3.0))
    }
}
