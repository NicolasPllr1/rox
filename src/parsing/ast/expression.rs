use std::hash::{Hash, Hasher};

use crate::lexing::token::{Token, TokenType};
use crate::runtime::callable::LoxCallable;

#[derive(Debug, Clone, PartialEq)]
pub enum LoxValue {
    Bool(bool),
    Nil,
    Number(f32),
    String(String),
    Callable(LoxCallable),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal {
        id: usize,
        value: LoxValue,
    },
    Unary {
        id: usize,
        op: UnaryOp,
        right: Box<Expr>,
    },
    Binary {
        id: usize,
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Grouping {
        id: usize,
        group: Box<Expr>,
    },
    Variable {
        id: usize,
        name: Token,
    }, // name is an identifier token at first glance
    Assign {
        id: usize,
        name: Token,
        value: Box<Expr>,
    },
    Logical {
        id: usize,
        left: Box<Expr>,
        op: LogicOp,
        right: Box<Expr>,
    },
    Call {
        id: usize,
        callee: Box<Expr>,
        arguments: Box<Vec<Expr>>, // NOTE: box of vec of vec of boxes ?
    },
}

impl Expr {
    pub fn id(&self) -> usize {
        match self {
            Expr::Literal { id, value: _ } => *id,
            Expr::Unary {
                id,
                op: _,
                right: _,
            } => *id,
            Expr::Binary {
                id,
                left: _,
                op: _,
                right: _,
            } => *id,
            Expr::Grouping { id, group: _ } => *id,
            Expr::Variable { id, name: _ } => *id,
            Expr::Assign {
                id,
                name: _,
                value: _,
            } => *id,
            Expr::Logical {
                id,
                left: _,
                op: _,
                right: _,
            } => *id,
            Expr::Call {
                id,
                callee: _,
                arguments: _,
            } => *id,
        }
    }
}

impl Hash for Expr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // use the id
        self.id().hash(state);
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

impl Eq for Expr {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Plus,
    Minus,
    Slash,
    Star,
    EqualEqual,
    BangEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl From<TokenType> for BinaryOp {
    fn from(tok_type: TokenType) -> BinaryOp {
        match tok_type {
            TokenType::Plus => BinaryOp::Plus,
            TokenType::Minus => BinaryOp::Minus,
            TokenType::Slash => BinaryOp::Slash,
            TokenType::Star => BinaryOp::Star,
            TokenType::EqualEqual => BinaryOp::EqualEqual,
            TokenType::BangEqual => BinaryOp::BangEqual,
            TokenType::Greater => BinaryOp::Greater,
            TokenType::GreaterEqual => BinaryOp::GreaterEqual,
            TokenType::Less => BinaryOp::Less,
            TokenType::LessEqual => BinaryOp::LessEqual,
            _ => panic!("Wrong token type for a binary operator: {tok_type}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Bang,
    Minus,
}
impl From<TokenType> for UnaryOp {
    fn from(tok_type: TokenType) -> UnaryOp {
        match tok_type {
            TokenType::Bang => UnaryOp::Bang,
            TokenType::Minus => UnaryOp::Minus,
            _ => panic!("Wrong token type for an unary operator: {tok_type}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LogicOp {
    Or,
    And,
}
impl From<TokenType> for LogicOp {
    fn from(tok_type: TokenType) -> LogicOp {
        match tok_type {
            TokenType::Or => LogicOp::Or,
            TokenType::And => LogicOp::And,
            _ => panic!("Wrong token type for logic operator: {tok_type}"),
        }
    }
}
