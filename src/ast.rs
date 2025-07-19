use std::hash::{Hash, Hasher};

use crate::callable::LoxCallable;
use crate::token::{Token, TokenType};

// Lox grammar, from lowest to highest precedence priority:
//
// program -> declaration* EOF ;
//
// declaration -> funcDecl | varDecl | statement;
//
// funcDecl -> "fun" function ;
// varDecl -> "var" + IDENTIFIER ( "=" expression )? ";" ;
// statement -> exprStmt | IfStmt | printstmt | whileStmt | block | returnStmt ;
//
// function -> IDENTIFIER "(" parameters? ")" block ;
// parameters -> IDENTIFIER ( "," IDENTIFIER )* ;
//
// exprStmt -> expression ";" ;
// forStmt -> "for" "(" ( varDecl | exprStmt | "," ) expression? ";" expression? ")" statement ;
// IfStmt -> "if" + "(" + expression + ")" statement ( "else" statement )? ;
// printStmt -> "print" expression ";" ;
// whileStmt -> "while" "(" expression ")" statement ;
// block -> "{" + declaration* + "}" ;
// returnStmt -> "return" + expression? + ";" ;
//
// expression -> assignement ;
// assignment -> IDENTIFIER "=" assignement | logic_or ;
//
// logic_or -> logic_and ( "or" logic_and )* ;
// logic_and -> equality ( "and" equality )* ;
// equality -> comparison ( ("!=" | "==" ) comparison )* ;
// comparison -> term ( (">" | ">=" | "<" | "<=") term )* ;
// term -> factor ( ("-" | "+" ) factor )* ;
// factor -> unary ( ("/" | "*" ) unary )* ;
// unary -> ("!" | "-") unary | call ;
//
// call -> primary ( "(" arguments? ")" )* ;
// arguments -> expression ( "," expression )* ;
//
// primary -> NUMBER | STRING | "true" | "false" | "Nil" | "(" expression ")" | IDENTIFIER ;

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
