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
// statement -> exprStmt | IfStmt | printstmt | whileStmt | block ;
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

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(LoxValue),
    Unary {
        op: UnaryOp,
        right: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Grouping(Box<Expr>),
    Variable {
        name: Token,
    }, // name is an identifier token at first glance
    Assign {
        name: Token,
        value: Box<Expr>,
    },
    Logical {
        left: Box<Expr>,
        op: LogicOp,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        arguments: Box<Vec<Expr>>, // NOTE: box of vec of vec of boxes ?
    },
}
#[derive(Debug, Clone, Copy, PartialEq)]
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

#[derive(Debug, Clone, Copy, PartialEq)]
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

#[derive(Debug, Clone, Copy, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    ExprStmt(Expr),
    IfStmt {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },

    WhileStmt {
        condition: Expr,
        body: Box<Stmt>,
    },
    PrintStmt(Expr),
    Block(Vec<Declaration>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    StmtDecl(Stmt),
    VarDecl {
        name: Token,
        initializer: Option<Expr>,
    },
    FuncDecl {
        name: Token,
        params: Vec<Token>,
        body: Stmt, // should be Stmt::Block
    },
}
