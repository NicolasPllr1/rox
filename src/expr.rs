#![allow(dead_code)]
// use crate::token::{Token, TokenType};
// Lox grammer, form lowest to highest precedance priority:
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

#[derive(Debug)]
pub enum Expr {
    Literal(String),
    Unary(Box<Unary>),
    Binary(Box<Binary>),
    Grouping(Box<Grouping>),
}
#[derive(Debug)]
enum BinOperator {
    Plus,
    Minus,
    Div,
    Mul,
    EqualEqual,
    BangEqual,
}

impl From<TokenType> for BinOperator {
    fn from(tok_type: TokenType) -> BinOperator {
        match tok_type {
            TokenType::Plus => BinOperator::Plus,
            TokenType::Minus => BinOperator::Minus,
            TokenType::Slash => BinOperator::Div,
            TokenType::Star => BinOperator::Mul,
            TokenType::EqualEqual => BinOperator::EqualEqual,
            TokenType::BangEqual => BinOperator::BangEqual,
            _ => panic!("Wrong token type for a binary operator: {tok_type}"),
        }
    }
}

#[derive(Debug)]
pub struct Binary {
    left: Expr,
    op: BinOperator,
    right: Expr,
}

#[derive(Debug)]
pub struct Grouping {
    expr: Expr,
}

#[derive(Debug)]
pub struct Literal {
    value: String,
}

#[derive(Debug)]
enum UnaryOp {
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
pub struct Unary {
    op: UnaryOp,
    right: Expr,
}

#[derive(Debug)]
pub struct ParserError {
    msg: String,
    tok: Token,
}

pub struct Parser {}

impl Parser {
    pub fn parse(tokens: Vec<Token>) -> Result<Expr, ParserError> {
        let mut tokens = tokens.iter().peekable();
        Parser::expression(&mut tokens)
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
                    let left = expr;
                    let op = BinOperator::from(tok.token_type);
                    let right = Parser::comparaison(tokens)?;
                    expr = Expr::Binary(Box::new(Binary { left, op, right }));
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
                    let left = expr;
                    let op = BinOperator::from(tok.token_type);
                    let right = Parser::comparaison(tokens)?;
                    expr = Expr::Binary(Box::new(Binary { left, op, right }));
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
                    let left = expr;
                    let op = BinOperator::from(tok.token_type);
                    let right = Parser::comparaison(tokens)?;
                    expr = Expr::Binary(Box::new(Binary { left, op, right }));
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
                    let left = expr;
                    let op = BinOperator::from(tok.token_type);
                    let right = Parser::comparaison(tokens)?;
                    expr = Expr::Binary(Box::new(Binary { left, op, right }));
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
                let right = Parser::unary(tokens)?;
                Ok(Expr::Unary(Box::new(Unary { op, right })))
            }
            _ => Parser::primary(tokens),
        }
    }
    fn primary(tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, ParserError> {
        println!("primary");

        if let Some(&tok) = tokens.peek() {
            println!("Token in primary");
            dbg!(&tok);
            match tok.token_type {
                TokenType::Number => Ok(Expr::Literal(
                    tok.literal.clone().expect("String should have literal"),
                )),
                TokenType::String => Ok(Expr::Literal(
                    tok.literal.clone().expect("String should have literal"),
                )),
                TokenType::True => Ok(Expr::Literal("true".to_string())),
                TokenType::False => Ok(Expr::Literal("false".to_string())),
                TokenType::Nil => Ok(Expr::Literal("Nil".to_string())),
                TokenType::LeftParen => {
                    tokens.next();
                    println!("Going to parse after the first parenth of group ...");
                    let expr = Parser::expression(tokens)?;
                    println!("After expr parsed within group");
                    match tokens.peek() {
                        Some(&tok) if tok.token_type == TokenType::RightParen => (),
                        _ => panic!("Expect right parenthesis after expression. Got {tok}"),
                    };
                    Ok(Expr::Grouping(Box::new(Grouping { expr })))
                }
                _ => Err(ParserError {
                    msg: "Expect expression".into(),
                    tok: tok.clone(), // NOTE: after cloning, no need to dereference with *, why?
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
