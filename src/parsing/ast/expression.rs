use std::cell::RefCell;
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use crate::lexing::token::{Token, TokenType};
use crate::runtime::callable::{LoxCallable, LoxClass, LoxInstance};

#[derive(Debug, Clone, PartialEq)]
pub enum LoxValue<'de> {
    Bool(bool),
    Nil,
    Number(f32),
    String(String),
    Callable(LoxCallable<'de>),
    Class(LoxClass<'de>),
    Instance(Rc<RefCell<LoxInstance<'de>>>),
}

impl Display for LoxValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxValue::Bool(b) => write!(f, "{b}"),
            LoxValue::Nil => write!(f, "Nil"),
            LoxValue::Number(n) => write!(f, "{n}"),
            LoxValue::String(s) => write!(f, "{s}"),
            LoxValue::Callable(lox_callable) => {
                let fn_body = &lox_callable.function_body;
                let params = &lox_callable.params;
                write!(f, "Callable:\nParameters: {params:?}\nBody: {fn_body:?}")
            }
            LoxValue::Class(lox_class) => {
                write!(f, "{lox_class}")
            }
            LoxValue::Instance(instance_ref) => {
                let instance = instance_ref.borrow();
                write!(f, "{instance}")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr<'de> {
    Literal {
        id: usize,
        value: LoxValue<'de>,
    },
    Unary {
        id: usize,
        op: UnaryOp,
        right: Box<Expr<'de>>,
    },
    Binary {
        id: usize,
        left: Box<Expr<'de>>,
        op: BinaryOp,
        right: Box<Expr<'de>>,
    },
    Grouping {
        id: usize,
        group: Box<Expr<'de>>,
    },
    Variable {
        id: usize,
        name: Token<'de>,
    }, // name is an identifier token at first glance
    Assign {
        id: usize,
        name: Token<'de>,
        value: Box<Expr<'de>>,
    },
    Logical {
        id: usize,
        left: Box<Expr<'de>>,
        op: LogicOp,
        right: Box<Expr<'de>>,
    },
    Call {
        id: usize,
        callee: Box<Expr<'de>>,
        arguments: Box<Vec<Expr<'de>>>,
    },
    Get {
        id: usize,
        object: Box<Expr<'de>>,
        name: Token<'de>,
    },
    Set {
        id: usize,
        object: Box<Expr<'de>>,
        name: Token<'de>,
        value: Box<Expr<'de>>,
    },
    This {
        id: usize,
        keyword: Token<'de>,
    },
}

impl Expr<'_> {
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
            Expr::Get {
                id,
                object: _,
                name: _,
            } => *id,
            Expr::Set {
                id,
                object: _,
                name: _,
                value: _,
            } => *id,
            Expr::This { id, keyword: _ } => *id,
        }
    }
}

impl Hash for Expr<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // use the id
        self.id().hash(state);
    }
}

impl PartialEq for Expr<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

impl Eq for Expr<'_> {}

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
