use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::lexing::token::Token;
use crate::parsing::ast::expression::LoxValue;

#[derive(Debug, Clone, PartialEq)]
pub struct Env<'de> {
    pub enclosing: Option<Rc<RefCell<Env<'de>>>>, // parent environement. To implement scope.
    pub values: HashMap<&'de str, LoxValue<'de>>,
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
        self.enclosing = Some(Rc::clone(enclosing));
    }
    pub fn define(&mut self, name: &'de str, value: LoxValue<'de>) {
        self.values.insert(name, value);
    }

    // TODO: delete plain `assign`, use instead `assign_at`
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

    pub fn get_at(&self, distance: &usize, name: &str) -> LoxValue<'de> {
        if *distance == 0 {
            return self
                .values
                .get(name)
                .unwrap_or_else(|| panic!("could not find variable '{name}' at distance 0"))
                .clone();
        }
        // walk up the chain of enclosing environments
        let mut env_rc = Rc::clone(
            self.enclosing
                .as_ref()
                .unwrap_or_else(|| panic!("no enclosing environment at distance 1 for {name}")),
        );
        let mut level = 1;
        while level < *distance {
            // take borrow to access enclosing
            let enclosing_ref = {
                let borrowed = env_rc.borrow();
                borrowed
                    .enclosing
                    .as_ref()
                    .unwrap_or_else(|| {
                        panic!(
                            "no enclosing environment at distance {} for {name}",
                            level + 1
                        )
                    })
                    .clone()
            };
            env_rc = enclosing_ref;
            level += 1;
        }
        // finally, get the name in the found environment
        let env = env_rc.borrow();
        env.values
            .get(name)
            .unwrap_or_else(|| panic!("could not find variable '{name}' at distance {distance}"))
            .clone()
    }

    pub fn assign_at(&mut self, distance: &usize, name: &'de str, value: &LoxValue<'de>) {
        if *distance == 0 {
            self.values.insert(name, value.clone());
            return;
        }

        // walk up the chain of enclosing environments
        let mut env_rc = Rc::clone(
            self.enclosing
                .as_ref()
                .unwrap_or_else(|| panic!("no enclosing environment at distance 1 for {name}")),
        );
        let mut level = 1;
        while level < *distance {
            // take borrow to access enclosing
            let enclosing_ref = {
                let borrowed = env_rc.borrow();
                borrowed
                    .enclosing
                    .as_ref()
                    .unwrap_or_else(|| {
                        panic!(
                            "no enclosing environment at distance {} for {}",
                            level + 1,
                            name
                        )
                    })
                    .clone()
            };
            env_rc = enclosing_ref;
            level += 1;
        }
        // finally, get the name in the found environment
        let mut env = env_rc.borrow_mut();
        env.values.insert(name, value.clone());
    }
}

impl Default for Env<'_> {
    fn default() -> Self {
        Self::new()
    }
}
