use std::{cell::RefCell, iter::zip, rc::Rc};

use crate::{Env, Interpreter, LoxValue, Stmt, Token};

pub trait Callable {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &mut Interpreter, args: Vec<LoxValue>) -> LoxValue;
}

#[derive(Debug, Clone, PartialEq)]
// NOTE: check this lifetime + move this to another module
pub struct LoxCallable {
    pub function_body: Box<Stmt>,
    pub params: Box<Vec<Token>>, // all identifiers ?
}

impl Callable for LoxCallable {
    fn arity(&self) -> usize {
        self.params.len()
    }
    fn call(&self, interpreter: &mut Interpreter, args: Vec<LoxValue>) -> LoxValue {
        if args.len() != self.arity() {
            panic!("Wrong number of arguments passed")
        }

        // New env for the fn execution, associate args with params
        let original_env = Rc::clone(&interpreter.env);
        let mut fn_execution_env = Env::new_from(&interpreter.env);

        // binds args value to parameters names in the fn execution env
        let _ = zip(self.params.to_vec(), args)
            .map(|(fn_param, arg_value)| fn_execution_env.define(&fn_param.lexeme, arg_value));

        // execute the block using the interpreter with the correct env
        interpreter.env = Rc::new(RefCell::new(fn_execution_env));
        let final_value = interpreter.evaluate_stmt(&self.function_body);

        // reset interpreter env and return the function return value
        interpreter.env = original_env;
        final_value
    }
}
