use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::lexing::token::Token;
use crate::parsing::ast::expression::LoxValue;

#[derive(Debug, Clone, PartialEq)]
pub struct Env<'de> {
    enclosing: Option<Rc<RefCell<Env<'de>>>>, // parent environement. To implement scope.
    values: HashMap<&'de str, LoxValue<'de>>,
}

impl<'de> Env<'de> {
    pub fn new() -> Env<'de> {
        Env {
            enclosing: None,
            values: HashMap::new(),
        }
    }

    pub fn new_from(enclosing: &Rc<RefCell<Env<'de>>>) -> Env<'de> {
        let mut env = Env::default();
        env.set_enclosing_env(enclosing);
        env
    }

    pub fn set_enclosing_env(&mut self, enclosing: &Rc<RefCell<Env<'de>>>) {
        self.enclosing = Some(Rc::clone(&enclosing));
    }
    pub fn define(&mut self, name: &'de str, value: LoxValue<'de>) {
        self.values.insert(name, value);
    }

    pub fn assign(&mut self, name: &Token<'de>, value: &LoxValue<'de>) {
        if self.values.contains_key(name.lexeme) {
            self.values.insert(name.lexeme, value.clone());
        } else if let Some(parent_env_cell) = &self.enclosing {
            let mut parent_env = parent_env_cell.borrow_mut();
            parent_env.assign(name, value);
        } else {
            let var_name = &name.lexeme;
            panic!("Undefined variable {var_name}")
        }
    }

    pub fn get(&self, name: &str) -> LoxValue<'de> {
        match self.values.get(name) {
            Some(val) => val.clone(),
            None => {
                // NOTE: the importance of borrowing:
                if let Some(parent_env_rc) = &self.enclosing {
                    // NOTE: study this situation, 'a temporary with access to the borrow'
                    // let owned_parent_env = Rc::clone(parent_env_rc).borrow();
                    // owned_parent_env.get(name)
                    // let parent_env_rc = Rc::clone(parent_env_rc);
                    let val = parent_env_rc.borrow().get(name);
                    val
                } else {
                    panic!("Undefined variable {name}")
                }
            }
        }
    }
}

impl Default for Env<'_> {
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
