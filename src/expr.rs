#![allow(dead_code)]
// use crate::token::{Token, TokenType};
// Lox grammer, form lowest to highest precedance priority:
//
// program -> statement* EOF ;
//
// statement -> exprStmt | printstmt ;
// exprStmt -> expression ";" ;
// printStmt -> "print" expression ";" ;
//
// expression -> equality ;
// equality -> comparaison ( ("!=" | "==" ) comparaison )* ;
// comparaison -> term ( (">" | ">=" | "<" | "<=") term )* ;
// term -> factor ( ("-" | "+" ) factor )* ;
// factor -> unary ( ("/" | "*" ) unary )* ;
// unary -> ("!" | "-") unary | primary ;
//
// primary -> NUMBER | STRING | "true" | "false" | "Nil" | "(" expression ")" ;

use std::{iter::Peekable, slice::Iter};

use crate::{token::TokenType, Token};

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
}
#[derive(Debug)]
pub enum BinaryOp {
    And,
    Plus,
    Minus,
    Or,
    Slash,
    Star,
    EqualEqual,
    BangEqual,
}

impl From<TokenType> for BinaryOp {
    fn from(tok_type: TokenType) -> BinaryOp {
        match tok_type {
            TokenType::And => BinaryOp::And,
            TokenType::Plus => BinaryOp::Plus,
            TokenType::Minus => BinaryOp::Minus,
            TokenType::Or => BinaryOp::Or,
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

impl Expr {
    pub fn evaluate(&self) -> LoxValue {
        match self {
            Expr::Literal(v) => v.clone(),
            Expr::Unary { op, right } => match op {
                UnaryOp::Minus => {
                    if let LoxValue::Number(n) = Expr::evaluate(right) {
                        LoxValue::Number(-n)
                    } else {
                        panic!("Minus unary operator '-' expect a number operand")
                    }
                }

                UnaryOp::Bang => {
                    if let LoxValue::Bool(b) = Expr::evaluate(right) {
                        LoxValue::Bool(!b)
                    } else {
                        panic!("Bang unary operator '!' expect a boolean operand")
                    }
                }
            },

            Expr::Binary { left, op, right } => match op {
                BinaryOp::Plus => match (Expr::evaluate(left), Expr::evaluate(right)) {
                    (LoxValue::Number(n_left), LoxValue::Number(n_right)) => {
                        LoxValue::Number(n_left + n_right)
                    }
                    _ => panic!("Plus binary operator '+' expect two numbers as operands"),
                },
                BinaryOp::Minus => match (Expr::evaluate(left), Expr::evaluate(right)) {
                    (LoxValue::Number(n_left), LoxValue::Number(n_right)) => {
                        LoxValue::Number(n_left - n_right)
                    }
                    _ => panic!("Minus binary operator '-' expect two numbers as operands"),
                },
                BinaryOp::Slash => match (Expr::evaluate(left), Expr::evaluate(right)) {
                    (LoxValue::Number(n_left), LoxValue::Number(n_right)) => {
                        LoxValue::Number(n_left / n_right)
                    }
                    _ => panic!("Slash binary operator '/' expect two numbers as operands"),
                },
                BinaryOp::Star => match (Expr::evaluate(left), Expr::evaluate(right)) {
                    (LoxValue::Number(n_left), LoxValue::Number(n_right)) => {
                        LoxValue::Number(n_left * n_right)
                    }
                    _ => panic!("Star binary operator '*' expect two numbers as operands"),
                },
                BinaryOp::EqualEqual => match (Expr::evaluate(left), Expr::evaluate(right)) {
                    (LoxValue::Bool(b_left), LoxValue::Bool(b_right)) => {
                        LoxValue::Bool(b_left == b_right)
                    }
                    _ => panic!("Equality binary operator '==' expect two booleans as operands"),
                },
                BinaryOp::BangEqual => match (Expr::evaluate(left), Expr::evaluate(right)) {
                    (LoxValue::Bool(b_left), LoxValue::Bool(b_right)) => {
                        LoxValue::Bool(b_left != b_right)
                    }
                    _ => panic!("Inequality binary operator '!=' expect two booleans as operands"),
                },
                BinaryOp::And => match (Expr::evaluate(left), Expr::evaluate(right)) {
                    (LoxValue::Bool(b_left), LoxValue::Bool(b_right)) => {
                        LoxValue::Bool(b_left && b_right)
                    }
                    _ => panic!("And binary operator 'and' expect two booleans as operands"),
                },

                BinaryOp::Or => match (Expr::evaluate(left), Expr::evaluate(right)) {
                    (LoxValue::Bool(b_left), LoxValue::Bool(b_right)) => {
                        LoxValue::Bool(b_left || b_right)
                    }
                    _ => panic!("And binary operator 'and' expect two booleans as operands"),
                },
            },
            Expr::Grouping(expr) => expr.evaluate(),
        }
    }
}

#[derive(Debug)]
pub struct ParserError {
    msg: String,
    tok: Option<Token>,
}

#[derive(Debug)]
pub enum Stmt {
    ExprStmt(Expr),
    PrintStmt(Expr),
}

impl Stmt {
    pub fn evaluate(&self) -> LoxValue {
        match self {
            Stmt::ExprStmt(expr) => {
                let _expr = expr.evaluate();
                LoxValue::Nil
            }
            Stmt::PrintStmt(expr) => {
                let val = expr.evaluate();
                println!("{val:?}");
                LoxValue::Nil
            }
        }
    }
}

pub struct Parser {}

impl Parser {
    pub fn parse(tokens: Vec<Token>) -> Result<Vec<Stmt>, ParserError> {
        let mut tokens = tokens.iter().peekable();
        let mut statements = Vec::new();
        while let Some(&tok) = tokens.peek() {
            if tok.token_type == TokenType::Eof {
                return Ok(statements);
            }

            let stmt = Parser::statement(&mut tokens)?;
            statements.push(stmt);
        }
        Err(ParserError {
            msg: "expects tokens to end with a EOF token".into(),
            tok: None,
        })
    }

    fn statement(tokens: &mut Peekable<Iter<Token>>) -> Result<Stmt, ParserError> {
        println!("statement");

        match tokens.peek() {
            Some(tok) if tok.token_type == TokenType::Print => {
                tokens.next();
                Ok(Stmt::PrintStmt(Parser::print_stmt(tokens)?))
            }
            Some(_) => Ok(Stmt::PrintStmt(Parser::expr_stmt(tokens)?)),
            None => panic!("Statement expects a token"),
        }
    }

    fn print_stmt(tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, ParserError> {
        let expr = Parser::expression(tokens)?;

        match tokens.peek() {
            Some(&tok) if tok.token_type == TokenType::Semicolon => {
                tokens.next();
                Ok(expr)
            }
            Some(&tok) => Err(ParserError {
                msg: "print statement expects semicolon at the end".into(),
                tok: Some(tok.clone()),
            }),
            _ => panic!("print statement expect semicolon at the end, got no token at all"),
        }
    }
    fn expr_stmt(tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, ParserError> {
        let expr = Parser::expression(tokens)?;

        match tokens.peek() {
            Some(&tok) if tok.token_type == TokenType::Semicolon => {
                tokens.next();
                Ok(expr)
            }
            Some(&tok) => Err(ParserError {
                msg: "expression statement expects semicolon at the end".into(),
                tok: Some(tok.clone()),
            }),
            _ => panic!("expression statement expect semicolon at the end, got no token at all"),
        }
    }

    fn expression(tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, ParserError> {
        println!("expression");

        Parser::equality(tokens)
    }
    fn equality(tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, ParserError> {
        println!("equality");

        let mut expr = Parser::comparaison(tokens)?;

        while let Some(&tok) = tokens.peek() {
            match tok.token_type {
                TokenType::EqualEqual | TokenType::BangEqual => {
                    tokens.next();
                    let left = Box::new(expr);
                    let op = BinaryOp::from(tok.token_type);
                    let right = Box::new(Parser::comparaison(tokens)?);
                    expr = Expr::Binary { left, op, right };
                }
                _ => break,
            };
        }

        Ok(expr)
    }

    // NOTE: why the mistake of () instead of break in both equality and comparaison make the
    // program hangs on the test example ?
    // NOTE: understand the need for references around the peekable tokens and in Some(&tok) =
    // tokens.peek()
    fn comparaison(tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, ParserError> {
        println!("comparaison");

        let mut expr = Parser::term(tokens)?;

        while let Some(&tok) = tokens.peek() {
            match tok.token_type {
                TokenType::Less
                | TokenType::LessEqual
                | TokenType::Greater
                | TokenType::GreaterEqual => {
                    tokens.next();
                    let left = Box::new(expr);
                    let op = BinaryOp::from(tok.token_type);
                    let right = Box::new(Parser::term(tokens)?);
                    expr = Expr::Binary { left, op, right };
                }
                _ => break,
            };
        }

        Ok(expr)
    }
    fn term(tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, ParserError> {
        println!("term");

        let mut expr = Parser::factor(tokens)?;
        println!("In term, after we got the expr: {expr:?}");

        while let Some(&tok) = tokens.peek() {
            match tok.token_type {
                TokenType::Plus | TokenType::Minus => {
                    tokens.next();
                    let left = Box::new(expr);
                    let op = BinaryOp::from(tok.token_type);
                    let right = Box::new(Parser::comparaison(tokens)?);
                    expr = Expr::Binary { left, op, right };
                }
                _ => break,
            };
        }

        Ok(expr)
    }
    fn factor(tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, ParserError> {
        println!("factor");

        let mut expr = Parser::unary(tokens)?;
        println!("In factor, after we got the expr: {expr:?}");

        while let Some(&tok) = tokens.peek() {
            match tok.token_type {
                TokenType::Slash | TokenType::Star => {
                    tokens.next();
                    let left = Box::new(expr);
                    let op = BinaryOp::from(tok.token_type);
                    let right = Box::new(Parser::comparaison(tokens)?);
                    expr = Expr::Binary { left, op, right };
                }
                _ => break,
            };
        }

        println!("Returning this exp to term: {expr:?}");
        tokens.next();
        Ok(expr)
    }
    fn unary(tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, ParserError> {
        println!("unary");

        match tokens.peek() {
            Some(&tok)
                if tok.token_type == TokenType::Bang || tok.token_type == TokenType::Minus =>
            {
                tokens.next();
                let op = UnaryOp::from(tok.token_type);
                let right = Box::new(Parser::unary(tokens)?);
                Ok(Expr::Unary { op, right })
            }
            _ => Parser::primary(tokens),
        }
    }
    fn primary(tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, ParserError> {
        println!("primary");

        if let Some(&tok) = tokens.peek() {
            match tok.token_type {
                TokenType::Number => Ok(Expr::Literal(LoxValue::Number(
                    tok.literal
                        .clone()
                        .expect("Number token should have a literal")
                        .parse()
                        .expect("Number token should parse to a f32"),
                ))),
                TokenType::String => Ok(Expr::Literal(LoxValue::String(
                    tok.literal.clone().expect("String should have literal"),
                ))),
                TokenType::True => Ok(Expr::Literal(LoxValue::Bool(true))),
                TokenType::False => Ok(Expr::Literal(LoxValue::Bool(false))),
                TokenType::Nil => Ok(Expr::Literal(LoxValue::Nil)),
                TokenType::LeftParen => {
                    tokens.next();
                    println!("Going to parse after the first parenth of group ...");
                    let expr = Parser::expression(tokens)?;
                    println!("After expr parsed within group");
                    match tokens.peek() {
                        Some(&tok) if tok.token_type == TokenType::RightParen => (),
                        _ => panic!("Expect right parenthesis after expression. Got {tok}"),
                    };
                    Ok(Expr::Grouping(Box::new(expr)))
                }
                _ => Err(ParserError {
                    msg: "Expect expression".into(),
                    tok: Some(tok.clone()), // NOTE: after cloning, no need to dereference with *, why?
                }),
            }
        } else {
            panic!("expect expression a token for primary, but got none")
        }
    }

    fn synchronize(tokens: &mut Peekable<Iter<Token>>) {
        println!("synchronize");

        while let Some(&tok) = tokens.peek() {
            match tok.token_type {
                TokenType::Class
                | TokenType::For
                | TokenType::Fun
                | TokenType::If
                | TokenType::Print
                | TokenType::Return
                | TokenType::Var
                | TokenType::While => {
                    return;
                }
                TokenType::Semicolon => {
                    tokens.next();
                    return;
                }
                _ => todo!(),
            }
        }
    }
}
