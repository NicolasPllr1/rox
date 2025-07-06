use crate::ast::{BinaryOp, Declaration, Expr, LoxValue, Stmt, UnaryOp};
use crate::env::Env;

pub struct Interpreter {
    env: Env,
}

impl Default for Interpreter {
    fn default() -> Self {
        Interpreter::new()
    }
}

impl Interpreter {
    fn new() -> Interpreter {
        Interpreter {
            env: Env::default(),
        }
    }

    pub fn evaluate(&mut self, declarations: Vec<Declaration>) {
        for decl in declarations {
            let _val = self.evaluate_decl(&decl);
        }
    }
    fn evaluate_decl(&mut self, decl: &Declaration) -> LoxValue {
        match decl {
            Declaration::StmtDecl(stmt) => self.evaluate_stmt(stmt),
            Declaration::VarDecl { name, initializer } => {
                let value = initializer
                    .as_ref() // NOTE: study this as_ref, related to options and shared references ?
                    .map_or(LoxValue::Nil, |expr| self.evaluate_expr(expr));
                self.env.define(&name.lexeme, value);
                LoxValue::Nil
            }
        }
    }

    pub fn evaluate_stmt(&mut self, stmt: &Stmt) -> LoxValue {
        match stmt {
            Stmt::ExprStmt(expr) => {
                let _expr = self.evaluate_expr(expr);
                LoxValue::Nil
            }
            Stmt::PrintStmt(expr) => {
                let val = self.evaluate_expr(expr);
                println!("{val:?}");
                LoxValue::Nil
            }

            Stmt::Block(declarations) => {
                // create new env and evaluate all inner statements in it
                let original_env = self.env.clone();
                let block_env = Env::new_from(&self.env);

                // intepret declarations in the block with the new nested env
                self.env = block_env;
                for decl in declarations {
                    self.evaluate_decl(decl);
                }

                self.env = original_env.clone(); // restaure interpreter original env
                LoxValue::Nil
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
                    _ => panic!("Plus binary operator '+' expect two numbers as operands"),
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
                BinaryOp::And => match (self.evaluate_expr(left), self.evaluate_expr(right)) {
                    (LoxValue::Bool(b_left), LoxValue::Bool(b_right)) => {
                        LoxValue::Bool(b_left && b_right)
                    }
                    _ => panic!("And binary operator 'and' expect two booleans as operands"),
                },

                BinaryOp::Or => match (self.evaluate_expr(left), self.evaluate_expr(right)) {
                    (LoxValue::Bool(b_left), LoxValue::Bool(b_right)) => {
                        LoxValue::Bool(b_left || b_right)
                    }
                    _ => panic!("And binary operator 'and' expect two booleans as operands"),
                },
            },
            Expr::Grouping(expr) => self.evaluate_expr(expr),
            Expr::Variable { name } => self.env.get(&name.lexeme).clone(),
            Expr::Assign { name, value } => {
                let final_value = self.evaluate_expr(value);
                self.env.assign(name, final_value.clone());
                final_value
            }
        }
    }
}

#[derive(Debug)]
pub struct EvaluationError {}
