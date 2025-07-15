#![allow(dead_code)]

use crate::token::{Token, TokenType};
use std::{iter::Peekable, slice::Iter};

use crate::ast::{BinaryOp, Declaration, Expr, LogicOp, LoxValue, Stmt, UnaryOp};

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
                Parser::var_decl(tokens)
            }
            Some(&tok) if tok.token_type == TokenType::Fun => {
                tokens.next();
                Parser::func_decl(tokens)
            }
            Some(_) => Ok(Declaration::StmtDecl(Parser::statement(tokens)?)),
            None => Err(ParserError {
                msg: "declaration expects a token".to_owned(),
                tok: None,
            }),
        }
    }

    fn func_decl(tokens: &mut Peekable<Iter<Token>>) -> Result<Declaration, ParserError> {
        // function name
        let name =
            Parser::match_next_token_type(tokens, TokenType::Identifier).ok_or(ParserError {
                msg: "expects identifier after 'fun' for the function name".to_owned(),
                tok: tokens.next().cloned(),
            })?;

        // '('
        let _ = Parser::match_next_token_type(tokens, TokenType::LeftParen).ok_or(ParserError {
            msg: "expects '(' after function name".to_owned(),
            tok: tokens.next().cloned(),
        })?;

        // Parse parameters
        let mut params = Vec::new();
        match tokens.peek() {
            Some(tok) if tok.token_type != TokenType::RightParen => {
                // first arg
                let p = Parser::match_next_token_type(tokens, TokenType::Identifier).ok_or(
                    ParserError {
                        msg: "expects parameter name".to_owned(),
                        tok: tokens.next().cloned(),
                    },
                )?;
                params.push(p);

                // follow-up args
                while Parser::match_next_token_type(tokens, TokenType::Comma).is_some() {
                    if params.len() < 255 {
                        let p = Parser::match_next_token_type(tokens, TokenType::Identifier)
                            .ok_or(ParserError {
                                msg: "expects parameter name".to_owned(),
                                tok: tokens.next().cloned(),
                            })?;
                        params.push(p);
                    } else {
                        return Err(ParserError {
                            msg: "Can't have more than 255 arguments".to_owned(),
                            tok: tokens.next().cloned(), // NOTE: the use of cloned()
                        });
                    }
                }
            }
            _ => (),
        }

        // ')'
        let _ =
            Parser::match_next_token_type(tokens, TokenType::RightParen).ok_or(ParserError {
                msg: "function declaration expects ')' after parameters".to_owned(),
                tok: tokens.next().cloned(),
            })?;

        // Parse the function body

        // '{'
        let _ = Parser::match_next_token_type(tokens, TokenType::LeftBrace).ok_or(ParserError {
            msg: "expects '{' before function body".to_owned(),
            tok: tokens.next().cloned(),
        })?;

        let body = Stmt::Block(Parser::block(tokens)?);

        // '}'
        let _ =
            Parser::match_next_token_type(tokens, TokenType::RightBrace).ok_or(ParserError {
                msg: "expects '}' after function body".to_owned(),
                tok: tokens.next().cloned(),
            })?;

        Ok(Declaration::FuncDecl { name, params, body })
    }
    fn var_decl(tokens: &mut Peekable<Iter<Token>>) -> Result<Declaration, ParserError> {
        // var just got consumed
        // pattern is: var IDENTIFIER (= expr)? ";"
        let var_decl = match tokens.peek() {
            Some(&name_tok) if name_tok.token_type == TokenType::Identifier => {
                tokens.next();
                match tokens.peek() {
                    Some(&tok) if tok.token_type == TokenType::Equal => {
                        tokens.next();
                        let initializer = Parser::expression(tokens)?;
                        Declaration::VarDecl {
                            name: name_tok.clone(),
                            initializer: Some(initializer),
                        }
                    }
                    _ => Declaration::VarDecl {
                        name: name_tok.clone(),
                        initializer: None,
                    },
                }
            }
            _ => panic!("Variable declaration expects identifier after 'var' keyword"),
        };
        match tokens.peek() {
            Some(&after_tok) if after_tok.token_type == TokenType::Semicolon => {
                tokens.next(); // consume the semicolon
                Ok(var_decl)
            }
            Some(&not_semicolon) => Err(ParserError {
                msg: "variable declaration expects to end with a semicolon".to_owned(),
                tok: Some(not_semicolon.clone()),
            }),
            None => Err(ParserError {
                msg: "variable declaration expects to end with a semicolon, but got none"
                    .to_owned(),
                tok: None,
            }),
        }
    }

    // check if the next token matches the provided next token type.
    // WILL CONSUME THE NEXT TOKEN.
    // If the next token does not exist (.peek returns None) or does not match, returns None.
    fn match_next_token_type(
        tokens: &mut Peekable<Iter<Token>>,
        nxt_expected_type: TokenType,
    ) -> Option<Token> {
        match tokens.peek() {
            Some(&next_tok) if next_tok.token_type == nxt_expected_type => {
                tokens.next(); // consume the token
                Some(next_tok.clone()) // NOTE: remove this clone ?
            }
            _ => None,
        }
    }

    fn statement(tokens: &mut Peekable<Iter<Token>>) -> Result<Stmt, ParserError> {
        match tokens.peek() {
            Some(tok) if tok.token_type == TokenType::Print => {
                tokens.next();
                let print_stmt = Parser::print_stmt(tokens)?;
                match Parser::match_next_token_type(tokens, TokenType::Semicolon) {
                    Some(_) => Ok(Stmt::PrintStmt(print_stmt)),
                    None => Err(ParserError {
                        msg: "print statement expects to end with a semicolon".to_owned(),
                        tok: tokens.next().cloned(),
                    }),
                }
            }
            Some(&tok) if tok.token_type == TokenType::LeftBrace => {
                tokens.next(); // consume left brace
                let stmts_in_block = Parser::block(tokens)?;

                match Parser::match_next_token_type(tokens, TokenType::RightBrace) {
                    Some(_) => Ok(Stmt::Block(stmts_in_block)),
                    None => Err(ParserError {
                        msg: "block statement expects to end with a right brace".to_owned(),
                        tok: tokens.next().cloned(),
                    }),
                }
            }
            Some(&tok) if tok.token_type == TokenType::If => {
                tokens.next(); // consume "if"
                Parser::if_stmt(tokens)
            }
            Some(&tok) if tok.token_type == TokenType::While => {
                tokens.next(); // consume "while"
                Parser::while_stmt(tokens)
            }
            Some(&tok) if tok.token_type == TokenType::For => {
                tokens.next(); // consume "for"
                Parser::for_stmt(tokens)
            }

            Some(&tok) if tok.token_type == TokenType::Return => {
                tokens.next(); // consume "return"
                Parser::return_stmt(tokens)
            }
            Some(_) => {
                let expr_stmt = Parser::expr_stmt(tokens)?;
                Ok(Stmt::ExprStmt(expr_stmt))
            }
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

    fn if_stmt(tokens: &mut Peekable<Iter<Token>>) -> Result<Stmt, ParserError> {
        // "if" was already consumed
        match tokens.peek() {
            Some(tok) if tok.token_type == TokenType::LeftParen => {
                tokens.next(); // consume "("
                let condition = Parser::expression(tokens)?;
                match tokens.peek() {
                    Some(tok) if tok.token_type == TokenType::RightParen => {
                        tokens.next(); // consume ")"
                        let then_branch = Box::new(Parser::statement(tokens)?);
                        let else_branch = match tokens.peek() {
                            Some(tok) if tok.token_type == TokenType::Else => {
                                Some(Box::new(Parser::statement(tokens)?))
                            }
                            _ => None,
                        };
                        Ok(Stmt::IfStmt {
                            condition,
                            then_branch,
                            else_branch,
                        })
                    }
                    _ => Err(ParserError {
                        msg: "condition in if statement must be followed by a right parenthesis"
                            .to_owned(),
                        tok: None,
                    }),
                }
            }
            Some(&tok) => Err(ParserError {
                msg: "Expects left parenthesis after 'if' keyword".to_owned(),
                tok: Some(tok.clone()),
            }),
            None => Err(ParserError {
                msg: "Expects left parenthesis after 'if' keyword, got none".to_owned(),
                tok: None,
            }),
        }
    }

    fn while_stmt(tokens: &mut Peekable<Iter<Token>>) -> Result<Stmt, ParserError> {
        // "if" was already consumed
        match tokens.peek() {
            Some(tok) if tok.token_type == TokenType::LeftParen => {
                tokens.next(); // consume "("
                let condition = Parser::expression(tokens)?;
                match tokens.peek() {
                    Some(tok) if tok.token_type == TokenType::RightParen => {
                        tokens.next(); // consume ")"
                        let body = Box::new(Parser::statement(tokens)?);
                        Ok(Stmt::WhileStmt { condition, body })
                    }
                    _ => Err(ParserError {
                        msg: "condition in while statement must be followed by a right parenthesis"
                            .to_owned(),
                        tok: None,
                    }),
                }
            }
            Some(&tok) => Err(ParserError {
                msg: "Expects left parenthesis after 'if' keyword".to_owned(),
                tok: Some(tok.clone()),
            }),
            None => Err(ParserError {
                msg: "Expects left parenthesis after 'if' keyword, got none".to_owned(),
                tok: None,
            }),
        }
    }

    fn for_stmt(tokens: &mut Peekable<Iter<Token>>) -> Result<Stmt, ParserError> {
        if Parser::match_next_token_type(tokens, TokenType::LeftParen).is_none() {
            return Err(ParserError {
                msg: "expects a '(' after the 'for' keyword".to_owned(),
                tok: tokens.next().cloned(),
            });
        };

        // parse initializer
        let initializer = match tokens.peek() {
            Some(&tok) if tok.token_type == TokenType::Semicolon => None,
            Some(&tok) if tok.token_type == TokenType::Var => {
                tokens.next(); // consume 'var'
                Some(Parser::var_decl(tokens)?)
            }
            _ => Some(Declaration::StmtDecl(Stmt::ExprStmt(Parser::expression(
                tokens,
            )?))),
        };

        let condition = match Parser::match_next_token_type(tokens, TokenType::Semicolon) {
            Some(_) => Expr::Literal(LoxValue::Bool(true)),
            _ => {
                let expr = Parser::expression(tokens)?;
                if Parser::match_next_token_type(tokens, TokenType::Semicolon).is_some() {
                    expr
                } else {
                    return Err(ParserError {
                        msg: "Expects semicolon after the for loop condition".to_owned(),
                        tok: tokens.next().cloned(),
                    });
                }
            }
        };

        let increment = match Parser::match_next_token_type(tokens, TokenType::RightParen) {
            Some(_) => None,
            None => {
                let expr = Parser::expression(tokens)?;

                // NOTE: if/else or combinator ?
                Parser::match_next_token_type(tokens, TokenType::RightParen)
                    .map(|_| Some(expr))
                    .ok_or_else(|| ParserError {
                        msg: "Expects right parenthesis at the end of the for loop initialization"
                            .to_owned(),
                        tok: tokens.next().cloned(),
                    })?

                // if Parser::match_next_token_type(tokens, TokenType::RightParen).is_some() {
                //     Some(expr)
                // } else {
                //     return Err(ParserError {
                //         msg: "Expects right parenthesis at the end of the for loop initialization"
                //             .to_owned(),
                //         tok: tokens.next().cloned(),
                //     });
                // }
            }
        };

        let mut body = Parser::statement(tokens)?;

        // desugaring the for loop
        if increment.is_some() {
            body = Stmt::Block(
                [
                    Declaration::StmtDecl(body),
                    Declaration::StmtDecl(Stmt::ExprStmt(increment.unwrap())),
                ]
                .into(),
            );
        };

        body = Stmt::WhileStmt {
            condition,
            body: Box::new(body),
        };

        if initializer.is_some() {
            body = Stmt::Block([initializer.unwrap(), Declaration::StmtDecl(body)].into());
        };

        Ok(body)
    }

    fn return_stmt(tokens: &mut Peekable<Iter<Token>>) -> Result<Stmt, ParserError> {
        // check for the *abscence* of an expression
        let maybe_return_expr = match Parser::match_next_token_type(tokens, TokenType::Semicolon) {
            Some(_) => None, // semicolon is next => no expression after the 'return' keyword
            None => {
                // return expression to parse
                let return_expr = Box::new(Parser::expression(tokens)?);

                // check for semicolon after the return expression
                Parser::match_next_token_type(tokens, TokenType::Semicolon)
                    .map(|_| Some(return_expr))
                    .ok_or_else(|| ParserError {
                        msg: "Expects semicolon after return statement".to_owned(),
                        tok: tokens.next().cloned(),
                    })?
            }
        };

        Ok(Stmt::Return(maybe_return_expr))
    }

    fn expression(tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, ParserError> {
        Parser::assignment(tokens)
    }

    fn assignment(tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, ParserError> {
        let expr = Parser::logic_or(tokens)?;

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

    fn logic_or(tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, ParserError> {
        let mut expr = Parser::logic_and(tokens)?;

        while Parser::match_next_token_type(tokens, TokenType::Or).is_some() {
            let right = Parser::logic_and(tokens)?;
            expr = Expr::Logical {
                left: Box::new(expr),
                op: LogicOp::Or,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }
    fn logic_and(tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, ParserError> {
        let mut expr = Parser::equality(tokens)?;

        while Parser::match_next_token_type(tokens, TokenType::And).is_some() {
            let right = Parser::equality(tokens)?;
            expr = Expr::Logical {
                left: Box::new(expr),
                op: LogicOp::And,
                right: Box::new(right),
            };
        }
        Ok(expr)
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
            _ => Parser::call(tokens),
        }
    }
    fn call(tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, ParserError> {
        let mut expr = Parser::primary(tokens)?;

        while Parser::match_next_token_type(tokens, TokenType::LeftParen).is_some() {
            // left parenthesis just got consumed in the check
            let mut args = Vec::new();

            // Parse arguments
            match tokens.peek() {
                Some(tok) if tok.token_type != TokenType::RightParen => {
                    // first arg
                    args.push(Parser::expression(tokens)?);

                    // follow-up args
                    while Parser::match_next_token_type(tokens, TokenType::Comma).is_some() {
                        if args.len() < 255 {
                            args.push(Parser::expression(tokens)?);
                        } else {
                            return Err(ParserError {
                                msg: "Can't have more than 255 arguments".to_owned(),
                                tok: tokens.next().cloned(), // NOTE: the use of cloned()
                            });
                        }
                    }
                }
                _ => (),
            }

            match Parser::match_next_token_type(tokens, TokenType::RightParen) {
                Some(_) => {
                    expr = Expr::Call {
                        callee: Box::new(expr),
                        arguments: Box::new(args),
                    }
                }
                None => {
                    return Err(ParserError {
                        msg: "function call expects ')' after arguments".to_owned(),
                        tok: tokens.next().cloned(),
                    })
                }
            }
        }

        Ok(expr)
    }
    fn primary(tokens: &mut Peekable<Iter<Token>>) -> Result<Expr, ParserError> {
        if let Some(&tok) = tokens.peek() {
            tokens.next(); // consume the token
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
                    let expr = Parser::expression(tokens)?;
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fn_parsing() {
        let tokens = Vec::from([
            Token {
                token_type: TokenType::Identifier,
                lexeme: "hello_world".to_string(),
                literal: None,
                line: 1,
            },
            Token {
                token_type: TokenType::LeftParen,
                lexeme: "(".to_string(),
                literal: None,
                line: 1,
            },
            Token {
                token_type: TokenType::Number,
                lexeme: "1.0".to_string(),
                literal: Some("1.0".to_owned()),
                line: 1,
            },
            Token {
                token_type: TokenType::RightParen,
                lexeme: ")".to_string(),
                literal: None,
                line: 1,
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_string(),
                literal: None,
                line: 1,
            },
            Token {
                token_type: TokenType::Eof,
                lexeme: "".to_string(),
                literal: None,
                line: 1,
            },
        ]);

        let ast = Parser::parse(tokens).expect("expects parsing not to fail");

        let gt_fn_call_ast: Declaration = Declaration::StmtDecl(Stmt::ExprStmt(Expr::Call {
            callee: Box::new(Expr::Variable {
                name: Token {
                    token_type: TokenType::Identifier,
                    lexeme: "hello_world".to_string(),
                    literal: None,
                    line: 1,
                },
            }),
            arguments: Box::new(Vec::from([Expr::Literal(LoxValue::Number(1.0))])),
        }));

        assert!(ast[0] == gt_fn_call_ast);
    }
}
