use std::hash::{Hash, Hasher};

use crate::parsing::ast::declaration::Declaration;
use crate::parsing::ast::expression::Expr;

#[derive(Debug, Clone)]
pub enum Stmt {
    ExprStmt {
        id: usize,
        expr: Expr,
    },
    IfStmt {
        id: usize,
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },

    WhileStmt {
        id: usize,
        condition: Expr,
        body: Box<Stmt>,
    },
    PrintStmt {
        id: usize,
        expr: Expr,
    },
    Block {
        id: usize,
        declarations: Vec<Declaration>,
    },
    Return {
        id: usize,
        expr: Option<Box<Expr>>,
    },
}

impl Stmt {
    pub fn id(&self) -> usize {
        match self {
            Stmt::ExprStmt { id, expr: _ } => *id,
            Stmt::IfStmt {
                id,
                condition: _,
                then_branch: _,
                else_branch: _,
            } => *id,
            Stmt::WhileStmt {
                id,
                condition: _,
                body: _,
            } => *id,
            Stmt::PrintStmt { id, expr: _ } => *id,
            Stmt::Block {
                id,
                declarations: _,
            } => *id,
            Stmt::Return { id, expr: _ } => *id,
        }
    }
}

impl Hash for Stmt {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // use the id to hash
        self.id().hash(state);
    }
}

impl PartialEq for Stmt {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

impl Eq for Stmt {}
