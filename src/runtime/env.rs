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

    pub fn get_at(&self, distance: &usize, name: &str) -> LoxValue<'de> {
        // get environement at 'distance' d where 1 distance unit ~ 1 scope
        // NOTE: could not refactor this out. I had problems with lifetimes,
        // trying the signature: pub fn get_ancestor(&'de self, distance: &usize) -> Option<&'de Env<'de>>

        // if *distance == 0 {
        //     return self
        //         .values
        //         .get(name)
        //         .expect("could not find vairable {name} at distance 0")
        //         .clone();
        // }
        //
        // must go up the chain of enclosing envs

        // let mut d = 0;
        // let mut ancestor = Rc::clone(self.enclosing.as_ref().expect(&format!(
        //     "no enclosing environement at distance 1 for {name} "
        // )));
        // d = 1;
        //
        // while d < *distance {
        //     // ancestor = match ancestor.borrow().enclosing {
        //     //     None => panic!(),
        //     //     Some(env) => env.as_ref().borrow(),
        //     // };
        //     //
        //     d += 1;
        //     let next = ancestor.borrow();
        //     // .enclosing
        //     // .as_ref()
        //     // .expect("no enclosing environment at distance {d} for {}");
        //     //
        //     ancestor = Rc::clone(
        //         next.enclosing
        //             .as_ref()
        //             .expect("no enclosing environment at distance {d} for {}"),
        //     );
        // }
        //
        // ancestor
        //     .borrow()
        //     .values
        //     .get(name)
        //     .expect("could not find vairable {name} at distancec {distance}")
        //     .clone()
        // direct within current scope

        // I’ve re-written get_at so that:
        //
        //     * We always keep an `Rc<RefCell<Env<'de>>>` for the “ancestor” scope, cloning the `Rc` as we walk up.
        //     * Each borrow of the inner `Env` is confined to its own block (so the temporary `Ref` is dropped before we clone the next `Rc`).
        //     * We preserve your original signature and error messages (now properly formatted via `format!`).
        if *distance == 0 {
            return self
                .values
                .get(name)
                .expect(&format!("could not find variable '{}' at distance 0", name))
                .clone();
        }
        // walk up the chain of enclosing environments
        let mut env_rc = Rc::clone(self.enclosing.as_ref().expect(&format!(
            "no enclosing environment at distance 1 for {}",
            name
        )));
        let mut level = 1;
        while level < *distance {
            // take borrow to access enclosing
            let enclosing_ref = {
                let borrowed = env_rc.borrow();
                borrowed
                    .enclosing
                    .as_ref()
                    .expect(&format!(
                        "no enclosing environment at distance {} for {}",
                        level + 1,
                        name
                    ))
                    .clone()
            };
            env_rc = enclosing_ref;
            level += 1;
        }
        // finally, get the name in the found environment
        let env = env_rc.borrow();
        env.values
            .get(name)
            .expect(&format!(
                "could not find variable '{}' at distance {}",
                name, distance
            ))
            .clone()
    }

    pub fn assign_at(&mut self, distance: &usize, name: &Token<'de>, value: &LoxValue<'de>) {
        if *distance == 0 {
            self.values.insert(name.lexeme, value.clone());
        }

        // walk up the chain of enclosing environments
        let mut env_rc = Rc::clone(self.enclosing.as_ref().expect(&format!(
            "no enclosing environment at distance 1 for {}",
            name
        )));
        let mut level = 1;
        while level < *distance {
            // take borrow to access enclosing
            let enclosing_ref = {
                let borrowed = env_rc.borrow();
                borrowed
                    .enclosing
                    .as_ref()
                    .expect(&format!(
                        "no enclosing environment at distance {} for {}",
                        level + 1,
                        name
                    ))
                    .clone()
            };
            env_rc = enclosing_ref;
            level += 1;
        }
        // finally, get the name in the found environment
        let mut env = env_rc.borrow_mut();
        env.values.insert(name.lexeme, value.clone());
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
