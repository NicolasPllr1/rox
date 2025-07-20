use std::hash::{Hash, Hasher};

use crate::parsing::ast::declaration::Declaration;
use crate::parsing::ast::expression::Expr;

#[derive(Debug, Clone)]
pub enum Stmt<'de> {
    ExprStmt {
        id: usize,
        expr: Expr<'de>,
    },
    IfStmt {
        id: usize,
        condition: Expr<'de>,
        then_branch: Box<Stmt<'de>>,
        else_branch: Option<Box<Stmt<'de>>>,
    },

    WhileStmt {
        id: usize,
        condition: Expr<'de>,
        body: Box<Stmt<'de>>,
    },
    PrintStmt {
        id: usize,
        expr: Expr<'de>,
    },
    Block {
        id: usize,
        declarations: Vec<Declaration<'de>>,
    },
    Return {
        id: usize,
        expr: Option<Box<Expr<'de>>>,
    },
}

impl Stmt<'_> {
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

impl Hash for Stmt<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // use the id to hash
        self.id().hash(state);
    }
}

impl PartialEq for Stmt<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

impl Eq for Stmt<'_> {}
