use crate::token::{Token, TokenType};

// Lox grammar, from lowest to highest precedence priority:
//
// program -> declaration* EOF ;
//
// declaration -> varDecl | statement;
//
// varDecl -> "var" + IDENTIFIER ( "=" expression )? ";" ;
// statement -> exprStmt | IfStmt | printstmt | block ;
//
// exprStmt -> expression ";" ;
// IfStmt -> "if" + "(" + expression + ")" statement ( "else" statement )? ;
// printStmt -> "print" expression ";" ;
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
// unary -> ("!" | "-") unary | primary ;
//
// primary -> NUMBER | STRING | "true" | "false" | "Nil" | "(" expression ")" | IDENTIFIER ;

#[derive(Debug, Clone)]
pub enum LoxValue {
    Bool(bool),
    Nil,
    Number(f32),
    String(String),
}

#[derive(Debug)]
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
}
#[derive(Debug)]
pub enum BinaryOp {
    Plus,
    Minus,
    Slash,
    Star,
    EqualEqual,
    BangEqual,
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
            _ => panic!("Wrong token type for a binary operator: {tok_type}"),
        }
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Stmt {
    ExprStmt(Expr),
    IfStmt {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    PrintStmt(Expr),
    Block(Vec<Declaration>),
}

#[derive(Debug)]
pub enum Declaration {
    StmtDecl(Stmt),
    VarDecl {
        name: Token,
        initializer: Option<Expr>,
    },
}
