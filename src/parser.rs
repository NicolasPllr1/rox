#![allow(dead_code)]

use crate::token::{Token, TokenType};
use std::{iter::Peekable, slice::Iter};

use crate::ast::{BinaryOp, Declaration, Expr, LoxValue, Stmt, UnaryOp};

#[derive(Debug)]
pub struct ParserError {
    pub msg: String,
    pub tok: Option<Token>,
}

pub struct Parser {}

impl Parser {
    pub fn parse(tokens: Vec<Token>) -> Result<Vec<Declaration>, ParserError> {
        let mut tokens = tokens.iter().peekable();
        let mut declarations = Vec::new();
        while let Some(&tok) = tokens.peek() {
            if tok.token_type == TokenType::Eof {
                return Ok(declarations);
            }

            let decl = Parser::declaration(&mut tokens)?;
            declarations.push(decl);
        }
        Err(ParserError {
            msg: "expects tokens to end with a EOF token".into(),
            tok: None,
        })
    }

    fn declaration(tokens: &mut Peekable<Iter<Token>>) -> Result<Declaration, ParserError> {
        match tokens.peek() {
            Some(&tok) if tok.token_type == TokenType::Var => {
                tokens.next();
                let (name, initializer) = Parser::var_decl(tokens)?;
                let decl = Declaration::VarDecl { name, initializer };

                match tokens.peek() {
                    Some(&after_tok) if after_tok.token_type == TokenType::Semicolon => {
                        tokens.next(); // consume the semicolon
                        Ok(decl)
                    }
                    Some(&not_semicolon) => Err(ParserError {
                        msg: "variable declaration expects to end with a semicolon".to_owned(),
                        tok: Some(not_semicolon.clone()),
                    }),
                    None => Err(ParserError {
                        msg: "declaration expects a token".to_owned(),
                        tok: None,
                    }),
                }
            }
            Some(&tok) if tok.token_type == TokenType::LeftBrace => {
                tokens.next(); // consume left brace
                let stmts_in_block = Parser::block(tokens)?;

                match tokens.peek() {
                    Some(&after_tok) if after_tok.token_type == TokenType::RightBrace => {
                        tokens.next(); // consume the right brace
                        Ok(Declaration::Block(stmts_in_block))
                    }
                    Some(&not_right_brace) => Err(ParserError {
                        msg: "block declaration expects to end with a right brace".to_owned(),
                        tok: Some(not_right_brace.clone()),
                    }),
                    None => Err(ParserError {
                        msg: "block declaration expects to end with a right brace".to_owned(),
                        tok: None,
                    }),
                }
            }
            Some(_) => {
                let decl = Declaration::StmtDecl(Parser::statement(tokens)?);

                match tokens.peek() {
                    Some(&after_tok) if after_tok.token_type == TokenType::Semicolon => {
                        tokens.next(); // consume the semicolon
                        Ok(decl)
                    }
                    Some(&not_semicolon) => Err(ParserError {
                        msg: "declaration expects to end with a semicolon".to_owned(),
                        tok: Some(not_semicolon.clone()),
                    }),
                    None => Err(ParserError {
                        msg: "declaration expects a token".to_owned(),
                        tok: None,
                    }),
                }
            }
            None => Err(ParserError {
                msg: "declaration expects a token".to_owned(),
                tok: None,
            }),
        }
    }

    fn var_decl(tokens: &mut Peekable<Iter<Token>>) -> Result<(Token, Option<Expr>), ParserError> {
        // var just got consumed
        // pattern is: var IDENTIFIER (= expr)? ";"
        match tokens.peek() {
            Some(&name_tok) if name_tok.token_type == TokenType::Identifier => {
                tokens.next();
                match tokens.peek() {
                    Some(&tok) if tok.token_type == TokenType::Equal => {
                        tokens.next();
                        let initializer = Parser::expression(tokens)?;
                        Ok((name_tok.clone(), Some(initializer)))
                    }
                    _ => Ok((name_tok.clone(), None)),
                }
            }
            _ => panic!("Variable declaration expects identifier after 'var' keyword"),
        }
    }

    fn statement(tokens: &mut Peekable<Iter<Token>>) -> Result<Stmt, ParserError> {
        match tokens.peek() {
            Some(tok) if tok.token_type == TokenType::Print => {
                tokens.next();
                Ok(Stmt::PrintStmt(Parser::print_stmt(tokens)?))
            }
            Some(_) => Ok(Stmt::PrintStmt(Parser::expr_stmt(tokens)?)),
            None => panic!("Statement expects a token"),
        }
    }

    fn block(tokens: &mut Peekable<Iter<Token>>) -> Result<Vec<Declaration>, ParserError> {
        // while we don't encounter '}', we parse what is expected to be a statement
        // error if we encounter EOF before '}'
        let mut stmts = Vec::new();

        while let Some(tok) = tokens.peek() {
            if tok.token_type == TokenType::RightBrace {
                break;
            } else {
                stmts.push(Parser::declaration(tokens)?);
            }
        }
        Ok(stmts)
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

    fn print_stmt(tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, ParserError> {
        let expr = Parser::expression(tokens)?;

        match tokens.peek() {
            Some(&tok) if tok.token_type == TokenType::Semicolon => Ok(expr),
            Some(&tok) => Err(ParserError {
                msg: "print statement expects semicolon at the end".into(),
                tok: Some(tok.clone()),
            }),
            _ => panic!("print statement expect semicolon at the end, got no token at all"),
        }
    }

    fn expression(tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, ParserError> {
        Parser::assignment(tokens)
    }

    fn assignment(tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, ParserError> {
        let expr = Parser::equality(tokens)?;

        match tokens.peek() {
            Some(&tok_equal) if tok_equal.token_type == TokenType::Equal => {
                tokens.next();
                let value = Parser::assignment(tokens)?;
                if let Expr::Variable { name } = expr {
                    Ok(Expr::Assign {
                        name,
                        value: Box::new(value),
                    })
                } else {
                    Err(ParserError {
                        msg: "Invalid assignement target".to_owned(),
                        tok: Some(tok_equal.clone()),
                    })
                }
            }
            _ => Ok(expr),
        }
    }

    fn equality(tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, ParserError> {
        let mut expr = Parser::comparison(tokens)?;

        while let Some(&tok) = tokens.peek() {
            match tok.token_type {
                TokenType::EqualEqual | TokenType::BangEqual => {
                    tokens.next();
                    let left = Box::new(expr);
                    let op = BinaryOp::from(tok.token_type);
                    let right = Box::new(Parser::comparison(tokens)?);
                    expr = Expr::Binary { left, op, right };
                }
                _ => break,
            };
        }

        Ok(expr)
    }

    // NOTE: why the mistake of () instead of break in both equality and comparison make the
    // program hangs on the test example ?
    // NOTE: understand the need for references around the peekable tokens and in Some(&tok) =
    // tokens.peek()
    fn comparison(tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, ParserError> {
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
        let mut expr = Parser::factor(tokens)?;

        while let Some(&tok) = tokens.peek() {
            match tok.token_type {
                TokenType::Plus | TokenType::Minus => {
                    tokens.next();
                    let left = Box::new(expr);
                    let op = BinaryOp::from(tok.token_type);
                    let right = Box::new(Parser::comparison(tokens)?);
                    expr = Expr::Binary { left, op, right };
                }
                _ => break,
            };
        }

        Ok(expr)
    }
    fn factor(tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, ParserError> {
        let mut expr = Parser::unary(tokens)?;

        while let Some(&tok) = tokens.peek() {
            match tok.token_type {
                TokenType::Slash | TokenType::Star => {
                    tokens.next();
                    let left = Box::new(expr);
                    let op = BinaryOp::from(tok.token_type);
                    let right = Box::new(Parser::comparison(tokens)?);
                    expr = Expr::Binary { left, op, right };
                }
                _ => break,
            };
        }

        tokens.next();
        Ok(expr)
    }
    fn unary(tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, ParserError> {
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
                        // Some(&tok) if tok.token_type == TokenType::RightParen => tokens.next(), // consume right parenthesis
                        _ => panic!("Expect right parenthesis after expression. Got {tok}"),
                    };
                    Ok(Expr::Grouping(Box::new(expr)))
                }
                TokenType::Identifier => Ok(Expr::Variable { name: tok.clone() }),
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
