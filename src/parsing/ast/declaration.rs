use std::hash::{Hash, Hasher};

use crate::lexing::token::Token;
use crate::parsing::ast::expression::Expr;
use crate::parsing::ast::statement::Stmt;

#[derive(Debug, Clone)]
pub enum Declaration<'de> {
    StmtDecl {
        id: usize,
        stmt: Stmt<'de>,
    },
    VarDecl {
        id: usize,
        name: Token<'de>,
        initializer: Option<Expr<'de>>,
    },
    FuncDecl {
        id: usize,
        name: Token<'de>,
        params: Vec<Token<'de>>,
        body: Stmt<'de>, // should be Stmt::Block
    },
    ClassDecl {
        id: usize,
        name: Token<'de>,
        methods: Vec<Box<Declaration<'de>>>, // vec of Declaration::FuncDecl
        super_class: Option<Expr<'de>>,
    },
}

impl Declaration<'_> {
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
            Declaration::ClassDecl {
                id,
                name: _,
                methods: _,
                super_class: _,
            } => *id,
        }
    }
}

impl Hash for Declaration<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // use the id to hash
        self.id().hash(state);
    }
}

impl PartialEq for Declaration<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

impl Eq for Declaration<'_> {}
