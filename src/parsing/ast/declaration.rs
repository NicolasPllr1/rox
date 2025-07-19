use std::hash::{Hash, Hasher};

use crate::lexing::token::Token;
use crate::parsing::ast::expression::Expr;
use crate::parsing::ast::statement::Stmt;

#[derive(Debug, Clone)]
pub enum Declaration {
    StmtDecl {
        id: usize,
        stmt: Stmt,
    },
    VarDecl {
        id: usize,
        name: Token,
        initializer: Option<Expr>,
    },
    FuncDecl {
        id: usize,
        name: Token,
        params: Vec<Token>,
        body: Stmt, // should be Stmt::Block
    },
}

impl Declaration {
    pub fn id(&self) -> usize {
        match self {
            Declaration::StmtDecl { id, stmt: _ } => *id,
            Declaration::VarDecl {
                id,
                name: _,
                initializer: _,
            } => *id,
            Declaration::FuncDecl {
                id,
                name: _,
                params: _,
                body: _,
            } => *id,
        }
    }
}

impl Hash for Declaration {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // use the id to hash
        self.id().hash(state);
    }
}

impl PartialEq for Declaration {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

impl Eq for Declaration {}
