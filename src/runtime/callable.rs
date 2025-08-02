use std::{cell::RefCell, collections::HashMap, fmt::Display, iter::zip, rc::Rc};

use crate::{Env, EvaluationError, Interpreter, LoxValue, Stmt, Token};

pub trait Callable<'a> {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &mut Interpreter<'a>, args: Vec<LoxValue<'a>>) -> LoxValue<'a>;
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoxCallable<'de> {
    pub function_body: Box<Stmt<'de>>,
    pub params: Box<Vec<Token<'de>>>, // all identifiers ?
    pub closure: Rc<RefCell<Env<'de>>>,
    pub is_initializer: bool,
}

impl<'de> Callable<'de> for LoxCallable<'de> {
    fn arity(&self) -> usize {
        self.params.len()
    }
    fn call(&self, interpreter: &mut Interpreter<'de>, args: Vec<LoxValue<'de>>) -> LoxValue<'de> {
        assert!(
            args.len() == self.arity(),
            "Wrong number of arguments passed"
        );

        // New env for the fn execution, associate args with params
        let original_env = Rc::clone(&interpreter.env);
        let mut fn_execution_env = Env::new_from(&self.closure);

        // binds args value to parameters names in the fn execution env
        let () = zip(self.params.to_vec(), args)
            .map(|(fn_param, arg_value)| fn_execution_env.define(fn_param.lexeme, arg_value))
            .collect();

        // execute the block using the interpreter with the correct env
        interpreter.env = Rc::new(RefCell::new(fn_execution_env));

        let final_value = match interpreter.evaluate_stmt(&self.function_body) {
            Ok(val) => val,
            Err(EvaluationError::ReturnValue(return_val)) => return_val,
        };

        // reset interpreter env and return the function return value
        interpreter.env = original_env;

        match self.is_initializer {
            true => self.closure.borrow().get_at(&0, "this"),
            false => final_value,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoxClass<'de> {
    pub name: &'de str,
    pub fields: HashMap<&'de str, LoxValue<'de>>,
    pub methods: HashMap<&'de str, LoxValue<'de>>, // NOTE: should store callable ?
    pub super_class: Option<Box<LoxClass<'de>>>,
}
impl Display for LoxClass<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.name;
        write!(f, "Class: {name}")
    }
}

impl<'de> LoxClass<'de> {
    pub fn get_method(&self, name: &'de str) -> &LoxValue<'de> {
        match self.methods.get(name) {
            Some(method) => method,
            None => {
                // reach for super
                match self.super_class.as_ref() {
                    Some(super_class) => super_class.get_method(name),
                    _ => panic!("Method {name} not defined on class"),
                }
            }
        }
    }
}

impl<'de> Callable<'de> for LoxClass<'de> {
    fn arity(&self) -> usize {
        match self.methods.get("init") {
            Some(LoxValue::Callable(init_method)) => init_method.arity(),
            Some(_) => panic!("expect method to be a callable"),
            None => 0,
        }
    }
    fn call(&self, interpreter: &mut Interpreter<'de>, args: Vec<LoxValue<'de>>) -> LoxValue<'de> {
        assert!(
            args.len() == self.arity(),
            "Wrong number of arguments passed"
        );
        let instance = LoxValue::Instance(Rc::new(RefCell::new(LoxInstance {
            class: self.clone(), // NOTE: cloning for now
        })));

        // execut 'init' method if any
        if let Some(LoxValue::Callable(init_method)) = self.methods.get("init") {
            let mut new_closure = Env::new_from(&init_method.closure);

            new_closure.define("this", instance); // HACK: clone
            let mut new_init_method = init_method.clone(); // HACK: clone
            new_init_method.closure = Rc::new(RefCell::new(new_closure));

            new_init_method.call(interpreter, args)
        } else {
            instance
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoxInstance<'de> {
    pub class: LoxClass<'de>,
}

impl Display for LoxInstance<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let class = &self.class;
        let name = class.name;
        write!(f, "Class Instance: {name}")
    }
}

impl<'de> LoxInstance<'de> {
    pub fn get(&self, name: &'de str) -> &LoxValue<'de> {
        match self.class.fields.get(name).or(self.class.methods.get(name)) {
            Some(val) => val,
            None => {
                // reach for super-class methods
                self.class
                    .super_class
                    .as_ref()
                    .map(|class| class.get_method(name))
                    .expect("Unknown property {name}")
            }
        }
    }

    pub fn set(&mut self, name: &'de str, value: LoxValue<'de>) {
        self.class.fields.insert(name, value);
    }
}
