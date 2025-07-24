use std::collections::HashMap;

use crate::{Declaration, Expr, Stmt, Token};

#[derive(Debug, Default)]
pub struct Resolver<'a> {
    pub scopes: Vec<HashMap<&'a str, bool>>, // stack of current scopes
    pub locals: HashMap<Expr<'a>, usize>,
    pub current_fn: Option<FnKind>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum FnKind {
    Function,
}

impl<'a> Resolver<'a> {
    pub fn resolve(&mut self, declarations: &[Declaration<'a>]) {
        self.begin_scope();
        for decl in declarations {
            self.resolve_decl(decl);
        }
        self.end_scope();
    }

    fn resolve_decl(&mut self, decl: &Declaration<'a>) {
        match decl {
            Declaration::FuncDecl {
                id: _,
                name,
                params: _,
                body: _,
            } => {
                self.declare(name);
                self.define(name);

                self.resolve_function(decl);
            }
            Declaration::StmtDecl { id: _, stmt } => {
                self.resolve_stmt(stmt);
            }

            Declaration::VarDecl {
                id: _,
                name,
                initializer,
            } => {
                self.declare(name);
                if let Some(ini_expr) = initializer {
                    self.resolve_expr(ini_expr);
                }
                self.define(name);
            }
            Declaration::ClassDecl {
                id: _,
                name,
                methods: _,
            } => {
                self.declare(name);
                self.define(name);
            }
        }
    }

    fn resolve_stmt(&mut self, stmt: &Stmt<'a>) {
        match stmt {
            Stmt::ExprStmt { id: _, expr } => self.resolve_expr(expr),
            Stmt::PrintStmt { id: _, expr } => self.resolve_expr(expr),
            Stmt::Return {
                id: _,
                expr: maybe_return_expr,
            } => {
                assert!(
                    self.current_fn.is_some(),
                    "return statements can only be used from within functions",
                );

                if let Some(return_expr) = maybe_return_expr {
                    self.resolve_expr(return_expr);
                }
            }
            Stmt::WhileStmt {
                id: _,
                condition,
                body,
            } => {
                self.resolve_expr(condition);
                self.resolve_stmt(body);
            }
            Stmt::IfStmt {
                id: _,
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_expr(condition);
                self.resolve_stmt(then_branch);
                if let Some(else_branch_stmt) = else_branch {
                    self.resolve_stmt(else_branch_stmt);
                }
            }
            Stmt::Block {
                id: _,
                declarations,
            } => self.resolve_block(declarations),
        }
    }

    fn resolve_block(&mut self, declarations: &[Declaration<'a>]) {
        // self.begin_scope();
        self.resolve(declarations);
        // self.end_scope();
    }

    fn resolve_function(&mut self, fn_decl: &Declaration<'a>) {
        match fn_decl {
            Declaration::FuncDecl {
                id: _,
                name: _,
                params,
                body,
            } => {
                let previous_fn_kind = self.current_fn;
                // update fn kind
                self.current_fn = Some(FnKind::Function);

                // resolve fn
                self.begin_scope();
                for tok in params {
                    self.declare(tok);
                    self.define(tok);
                }

                self.resolve_stmt(body);

                self.end_scope();

                // restore fn kind
                self.current_fn = previous_fn_kind;
            }
            _ => panic!("expect function declaration"),
        }
    }

    fn resolve_expr(&mut self, expr: &Expr<'a>) {
        match expr {
            Expr::Variable { id: _, name } => {
                if let Some(true) = self
                    .scopes
                    .last()
                    // check that the variable is declared but not resolved yet (i.e., found in
                    // the hashmap but associated to 'false')
                    .map(|scope| scope.get(&name.lexeme).is_some_and(|res| !(*res)))
                {
                    panic!("Can't read local variable in its own initializer")
                }

                self.resolve_local(expr, name);
            }
            Expr::Assign { id: _, name, value } => {
                self.resolve_expr(value);
                self.resolve_local(expr, name);
            }
            Expr::Literal { id: _, value: _ } => (),
            Expr::Unary {
                id: _,
                op: _,
                right,
            } => self.resolve_expr(right),
            Expr::Binary {
                id: _,
                left,
                op: _,
                right,
            } => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            }
            Expr::Grouping { id: _, group: expr } => self.resolve_expr(expr),
            Expr::Logical {
                id: _,
                left,
                op: _,
                right,
            } => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            }
            Expr::Call {
                id: _,
                callee,
                arguments,
            } => {
                self.resolve_expr(callee);
                for arg in arguments.iter() {
                    self.resolve_expr(arg);
                }
            }

            Expr::Get {
                id: _,
                object,
                name: _,
            } => self.resolve_expr(object),
        }
    }

    fn resolve_local(&mut self, expr: &Expr<'a>, var_tok: &Token) {
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if scope.contains_key(&var_tok.lexeme) {
                let d = self.scopes.len() - 1 - i;
                self.locals.insert(expr.clone(), d);
                return;
            }
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }
    fn end_scope(&mut self) {
        self.scopes.pop(); // NOTE: should we err here if scopes is empty?
    }

    fn declare(&mut self, var_token: &Token<'a>) {
        match self.scopes.len() {
            0 => (),
            nb_scopes => {
                self.scopes
                    .get_mut(nb_scopes - 1) // last scope
                    .expect("expect at least one scope")
                    .insert(var_token.lexeme, false);
            }
        }
    }
    fn define(&mut self, var_token: &Token<'a>) {
        match self.scopes.len() {
            0 => (),
            nb_scopes => {
                self.scopes
                    .get_mut(nb_scopes - 1) // last scope
                    .expect("expect at least one scope")
                    .insert(var_token.lexeme, true);
            }
        }
    }
}
