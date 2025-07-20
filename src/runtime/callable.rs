use std::{cell::RefCell, iter::zip, rc::Rc};

use crate::{Env, EvaluationError, Interpreter, LoxValue, Stmt, Token};

pub trait Callable<'a> {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &mut Interpreter<'a>, args: Vec<LoxValue<'a>>) -> LoxValue<'a>;
}

#[derive(Debug, Clone, PartialEq)]
// NOTE: check this lifetime + move this to another module
pub struct LoxCallable<'de> {
    pub function_body: Box<Stmt<'de>>,
    pub params: Box<Vec<Token<'de>>>, // all identifiers ?
    pub closure: Rc<RefCell<Env<'de>>>,
}

impl<'de> Callable<'de> for LoxCallable<'de> {
    fn arity(&self) -> usize {
        self.params.len()
    }
    fn call(&self, interpreter: &mut Interpreter<'de>, args: Vec<LoxValue<'de>>) -> LoxValue<'de> {
        if args.len() != self.arity() {
            panic!("Wrong number of arguments passed")
        }

        // New env for the fn execution, associate args with params
        let original_env = Rc::clone(&interpreter.env);
        let mut fn_execution_env = Env::new_from(&self.closure);

        // binds args value to parameters names in the fn execution env
        let () = zip(self.params.to_vec(), args)
            .map(|(fn_param, arg_value)| fn_execution_env.define(&fn_param.lexeme, arg_value))
            .collect();

        // execute the block using the interpreter with the correct env
        interpreter.env = Rc::new(RefCell::new(fn_execution_env));

        let final_value = match interpreter.evaluate_stmt(&self.function_body) {
            Ok(val) => val,
            Err(EvaluationError::ReturnValue(return_val)) => return_val,
        };

        // reset interpreter env and return the function return value
        interpreter.env = original_env;
        final_value
    }
}
