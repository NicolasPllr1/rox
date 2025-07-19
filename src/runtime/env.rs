use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::lexing::token::Token;
use crate::parsing::ast::expression::LoxValue;

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    enclosing: Option<Rc<RefCell<Env>>>, // parent environement. To implement scope.
    values: HashMap<String, LoxValue>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            enclosing: None,
            values: HashMap::new(),
        }
    }

    pub fn new_from(enclosing: &Rc<RefCell<Env>>) -> Env {
        let mut env = Env::default();
        env.set_enclosing_env(enclosing);
        env
    }

    pub fn set_enclosing_env(&mut self, enclosing: &Rc<RefCell<Env>>) {
        self.enclosing = Some(Rc::clone(&enclosing));
    }
    pub fn define(&mut self, name: &str, value: LoxValue) {
        self.values.insert(name.to_owned(), value);
    }

    pub fn assign(&mut self, name: &Token, value: LoxValue) {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme.to_owned(), value);
        } else if let Some(parent_env_cell) = &self.enclosing {
            let mut parent_env = parent_env_cell.borrow_mut();
            parent_env.assign(name, value);
        } else {
            let var_name = &name.lexeme;
            panic!("Undefined variable {var_name}")
        }
    }

    pub fn get(&self, name: &str) -> LoxValue {
        // TODO: try to avoid cloning, introduced along with &parent_env_rc.borrow()

        match self.values.get(name) {
            Some(val) => val.clone(),
            None => {
                // NOTE: the importance of borrowing:
                if let Some(parent_env_rc) = &self.enclosing {
                    let parent_env: &Env = &parent_env_rc.borrow(); // NOTE: no need to borrow ?
                    parent_env.get(name).clone()
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
        let env1 = Rc::new(RefCell::new(Env::default()));

        let var1 = LoxValue::Number(1.0);
        let var2 = LoxValue::Number(2.0);

        env1.borrow_mut().define("var1", var1.clone());

        let mut env2 = Env::new_from(&Rc::clone(&env1));
        env2.define("var2", var2.clone());

        dbg!(&env1);
        dbg!(&env2);
        assert!(env1.borrow().get("var1") == var1.clone());

        assert!(env2.get("var1") == var1.clone()); // has to lool up enclosing (parent) env
        assert!(env2.get("var2") == var2.clone());
    }
}
