use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::{BinaryOp, Declaration, Expr, LogicOp, LoxValue, Stmt, UnaryOp};
use crate::callable::{Callable, LoxCallable};
use crate::env::Env;

pub struct Interpreter {
    pub env: Rc<RefCell<Env>>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Interpreter::new()
    }
}

impl Interpreter {
    fn new() -> Interpreter {
        Interpreter {
            env: Rc::new(RefCell::new(Env::default())),
        }
    }

    pub fn evaluate(&mut self, declarations: Vec<Declaration>) {
        for decl in declarations {
            let _val = self.evaluate_decl(&decl);
        }
    }
    fn evaluate_decl(&mut self, decl: &Declaration) -> Result<LoxValue, EvaluationError> {
        match decl {
            Declaration::StmtDecl(stmt) => self.evaluate_stmt(stmt),

            Declaration::VarDecl { name, initializer } => {
                let value = initializer
                    .as_ref() // NOTE: study this as_ref, related to options and shared references ?
                    .map_or(LoxValue::Nil, |expr| self.evaluate_expr(expr));
                self.env.borrow_mut().define(&name.lexeme, value); // NOTE: borrow_mut vs get_mut
                Ok(LoxValue::Nil)
            }
            Declaration::FuncDecl { name, params, body } => {
                // register function in the environement as a callable
                let callable_fn = LoxCallable {
                    function_body: Box::new(body.clone()), // NOTE: clean this cloning mess
                    params: Box::new(params.to_vec()),
                };
                self.env
                    .borrow_mut()
                    .define(&name.lexeme, LoxValue::Callable(callable_fn));
                Ok(LoxValue::Nil)
            }
        }
    }

    pub fn evaluate_stmt(&mut self, stmt: &Stmt) -> Result<LoxValue, EvaluationError> {
        match stmt {
            Stmt::ExprStmt(expr) => {
                let _expr = self.evaluate_expr(expr);
                Ok(LoxValue::Nil)
            }
            Stmt::PrintStmt(expr) => {
                let val = self.evaluate_expr(expr);
                println!("{val:?}");
                Ok(LoxValue::Nil)
            }

            Stmt::Block(declarations) => {
                // create new env and evaluate all inner statements in it
                let original_env = Rc::clone(&self.env);
                let block_env = Env::new_from(&self.env);

                // intepret declarations in the block with the new nested env
                self.env = Rc::new(RefCell::new(block_env));
                for decl in declarations {
                    self.evaluate_decl(decl)?;
                }

                self.env = original_env; // restaure interpreter original env
                Ok(LoxValue::Nil)
            }
            Stmt::IfStmt {
                condition,
                then_branch,
                else_branch,
            } => match self.evaluate_expr(condition) {
                LoxValue::Bool(true) => self.evaluate_stmt(&then_branch),
                LoxValue::Bool(false) => {
                    if let Some(stmt) = else_branch {
                        self.evaluate_stmt(stmt)
                    } else {
                        Ok(LoxValue::Nil)
                    }
                }
                _ => panic!("expect expression to evaluate to a boolean in if statement"),
            },
            Stmt::WhileStmt { condition, body } => {
                while let LoxValue::Bool(true) = self.evaluate_expr(condition) {
                    self.evaluate_stmt(body)?;
                }
                Ok(LoxValue::Nil)
            }
            Stmt::Return(maybe_expr) => {
                let return_value = match maybe_expr {
                    Some(expr) => self.evaluate_expr(expr),
                    None => LoxValue::Nil,
                };
                Err(EvaluationError::ReturnValue(return_value))
            }
        }
    }

    pub fn evaluate_expr(&mut self, expr: &Expr) -> LoxValue {
        match expr {
            Expr::Literal(v) => v.clone(),
            Expr::Unary { op, right } => match op {
                UnaryOp::Minus => {
                    if let LoxValue::Number(n) = self.evaluate_expr(right) {
                        LoxValue::Number(-n)
                    } else {
                        panic!("Minus unary operator '-' expect a number operand")
                    }
                }

                UnaryOp::Bang => {
                    if let LoxValue::Bool(b) = self.evaluate_expr(right) {
                        LoxValue::Bool(!b)
                    } else {
                        panic!("Bang unary operator '!' expect a boolean operand")
                    }
                }
            },

            Expr::Binary { left, op, right } => match op {
                BinaryOp::Plus => match (self.evaluate_expr(left), self.evaluate_expr(right)) {
                    (LoxValue::Number(n_left), LoxValue::Number(n_right)) => {
                        LoxValue::Number(n_left + n_right)
                    }
                    (a, b) => panic!(
                        "Plus binary operator '+' expect two numbers as operands, got: {a:?}, {b:?}"
                    ),
                },
                BinaryOp::Minus => match (self.evaluate_expr(left), self.evaluate_expr(right)) {
                    (LoxValue::Number(n_left), LoxValue::Number(n_right)) => {
                        LoxValue::Number(n_left - n_right)
                    }
                    _ => panic!("Minus binary operator '-' expect two numbers as operands"),
                },
                BinaryOp::Slash => match (self.evaluate_expr(left), self.evaluate_expr(right)) {
                    (LoxValue::Number(n_left), LoxValue::Number(n_right)) => {
                        LoxValue::Number(n_left / n_right)
                    }
                    _ => panic!("Slash binary operator '/' expect two numbers as operands"),
                },
                BinaryOp::Star => match (self.evaluate_expr(left), self.evaluate_expr(right)) {
                    (LoxValue::Number(n_left), LoxValue::Number(n_right)) => {
                        LoxValue::Number(n_left * n_right)
                    }
                    _ => panic!("Star binary operator '*' expect two numbers as operands"),
                },
                BinaryOp::EqualEqual => {
                    match (self.evaluate_expr(left), self.evaluate_expr(right)) {
                        (LoxValue::Bool(b_left), LoxValue::Bool(b_right)) => {
                            LoxValue::Bool(b_left == b_right)
                        }
                        _ => {
                            panic!("Equality binary operator '==' expect two booleans as operands")
                        }
                    }
                }
                BinaryOp::BangEqual => {
                    match (self.evaluate_expr(left), self.evaluate_expr(right)) {
                        (LoxValue::Bool(b_left), LoxValue::Bool(b_right)) => {
                            LoxValue::Bool(b_left != b_right)
                        }
                        _ => panic!(
                            "Inequality binary operator '!=' expect two booleans as operands"
                        ),
                    }
                }

                BinaryOp::Greater => match (self.evaluate_expr(left), self.evaluate_expr(right)) {
                    (LoxValue::Number(b_left), LoxValue::Number(b_right)) => {
                        LoxValue::Bool(b_left > b_right)
                    }
                    _ => panic!("Greater binary operator '>' expect two numbers as operands"),
                },

                BinaryOp::GreaterEqual => {
                    match (self.evaluate_expr(left), self.evaluate_expr(right)) {
                        (LoxValue::Number(b_left), LoxValue::Number(b_right)) => {
                            LoxValue::Bool(b_left >= b_right)
                        }
                        _ => panic!(
                            "Greater or equal binary operator '>=' expect two numbers as operands"
                        ),
                    }
                }
                BinaryOp::Less => match (self.evaluate_expr(left), self.evaluate_expr(right)) {
                    (LoxValue::Number(b_left), LoxValue::Number(b_right)) => {
                        LoxValue::Bool(b_left < b_right)
                    }
                    _ => panic!("Lesser binary operator '<' expect two numbers as operands"),
                },

                BinaryOp::LessEqual => {
                    match (self.evaluate_expr(left), self.evaluate_expr(right)) {
                        (LoxValue::Number(b_left), LoxValue::Number(b_right)) => {
                            LoxValue::Bool(b_left <= b_right)
                        }
                        _ => panic!(
                            "Lesser or equal binary operator '<=' expect two numbers as operands"
                        ),
                    }
                }
            },
            Expr::Grouping(expr) => self.evaluate_expr(expr),
            Expr::Variable { name } => self.env.borrow().get(&name.lexeme).clone(),
            Expr::Assign { name, value } => {
                let final_value = self.evaluate_expr(value);
                self.env.borrow_mut().assign(name, final_value.clone());
                final_value
            }

            Expr::Logical { left, op, right } => match op {
                // short-circuiting logical operators
                LogicOp::Or => match self.evaluate_expr(left) {
                    LoxValue::Bool(true) => LoxValue::Bool(true),
                    LoxValue::Bool(false) => self.evaluate_expr(right),
                    _ => panic!(
                        "Or binary operator 'or' expect first operands to be a Lox boolean value"
                    ),
                },
                LogicOp::And => match self.evaluate_expr(left) {
                    LoxValue::Bool(true) => {
                        if let LoxValue::Bool(true) = self.evaluate_expr(right) {
                            LoxValue::Bool(true)
                        } else {
                            LoxValue::Bool(false)
                        }
                    }
                    LoxValue::Bool(false) => LoxValue::Bool(false),
                    _ => panic!(
                        "And binary operator 'and' expect first operands to be a Lox boolean value"
                    ),
                },
            },
            Expr::Call { callee, arguments } => match self.evaluate_expr(callee) {
                LoxValue::Callable(lox_callable) => {
                    let args: Vec<LoxValue> =
                        arguments.iter().map(|p| self.evaluate_expr(p)).collect();
                    lox_callable.call(self, args)
                }
                _ => panic!("expect callee to be callable"),
            },
        }
    }
}

#[derive(Debug)]
pub enum EvaluationError {
    ReturnValue(LoxValue),
}
