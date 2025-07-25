#![allow(dead_code)]

use crate::lexing::token::{Token, TokenType};
use std::{iter::Peekable, slice::Iter};

use crate::parsing::ast::expression::{BinaryOp, Expr, LogicOp, LoxValue, UnaryOp};

use crate::parsing::ast::declaration::Declaration;
use crate::parsing::ast::statement::Stmt;

#[derive(Debug)]
pub struct ParserError<'de> {
    pub msg: String,
    pub tok: Option<Token<'de>>,
}

pub struct Parser {
    current_node_id: usize,
}
impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
}

impl Parser {
    pub fn new() -> Parser {
        Parser { current_node_id: 0 }
    }
    pub fn new_id(&mut self) -> usize {
        self.current_node_id += 1;
        self.current_node_id
    }

    pub fn parse<'de>(
        &mut self,
        tokens: &[Token<'de>],
    ) -> Result<Vec<Declaration<'de>>, ParserError<'de>> {
        let mut tokens = tokens.iter().peekable();
        let mut declarations = Vec::new();
        while let Some(&tok) = tokens.peek() {
            if tok.token_type == TokenType::Eof {
                return Ok(declarations);
            }

            let decl = self.declaration(&mut tokens)?;
            declarations.push(decl);
        }
        Err(ParserError {
            msg: "expects tokens to end with a EOF token".into(),
            tok: None,
        })
    }

    fn declaration<'de>(
        &mut self,
        tokens: &mut Peekable<Iter<Token<'de>>>,
    ) -> Result<Declaration<'de>, ParserError<'de>> {
        match tokens.peek() {
            Some(&tok) if tok.token_type == TokenType::Var => {
                tokens.next();
                self.var_decl(tokens)
            }
            Some(&tok) if tok.token_type == TokenType::Fun => {
                tokens.next();
                self.func_decl(tokens)
            }
            Some(&tok) if tok.token_type == TokenType::Class => {
                tokens.next();
                self.class_decl(tokens)
            }
            Some(_) => Ok(Declaration::StmtDecl {
                id: self.new_id(),
                stmt: self.statement(tokens)?,
            }),
            None => Err(ParserError {
                msg: "declaration expects a token".to_owned(),
                tok: None,
            }),
        }
    }

    fn class_decl<'de>(
        &mut self,
        tokens: &mut Peekable<Iter<Token<'de>>>,
    ) -> Result<Declaration<'de>, ParserError<'de>> {
        let name = *tokens.next().expect("class should have a name, got None");

        // check for opening '{'
        let _ = Parser::match_next_token_type(tokens, TokenType::LeftBrace).ok_or_else(|| {
            ParserError {
                msg: "expects '{' after class name".to_owned(),
                tok: tokens.next().copied(),
            }
        })?;

        let mut methods = Vec::new();
        while tokens.peek().is_some()
            & Parser::match_next_token_type(tokens, TokenType::RightBrace).is_none()
        {
            methods.push(Box::new(self.func_decl(tokens)?));
        }

        Ok(Declaration::ClassDecl {
            id: self.new_id(),
            name,
            methods,
        })
    }

    fn func_decl<'de>(
        &mut self,
        tokens: &mut Peekable<Iter<Token<'de>>>,
    ) -> Result<Declaration<'de>, ParserError<'de>> {
        // function name
        let name =
            Parser::match_next_token_type(tokens, TokenType::Identifier).ok_or_else(|| {
                ParserError {
                    msg: "expects identifier after 'fun' for the function name".to_owned(),
                    tok: tokens.next().copied(), // ok_or_else to avoid calling .next() in the Ok
                                                 // case
                }
            })?;

        // '('
        let _ = Parser::match_next_token_type(tokens, TokenType::LeftParen).ok_or_else(|| {
            ParserError {
                msg: "expects '(' after function name".to_owned(),
                tok: tokens.next().copied(),
            }
        })?;

        // Parse parameters
        let mut params = Vec::new();
        match tokens.peek() {
            Some(tok) if tok.token_type != TokenType::RightParen => {
                // first arg
                let p = Parser::match_next_token_type(tokens, TokenType::Identifier).ok_or_else(
                    || ParserError {
                        msg: "expects parameter name".to_owned(),
                        tok: tokens.next().copied(),
                    },
                )?;
                params.push(p);

                // follow-up args
                while Parser::match_next_token_type(tokens, TokenType::Comma).is_some() {
                    if params.len() < 255 {
                        let p = Parser::match_next_token_type(tokens, TokenType::Identifier)
                            .ok_or_else(|| ParserError {
                                msg: "expects parameter name".to_owned(),
                                tok: tokens.next().copied(),
                            })?;
                        params.push(p);
                    } else {
                        return Err(ParserError {
                            msg: "Can't have more than 255 arguments".to_owned(),
                            tok: tokens.next().copied(), // NOTE: the use of cloned()
                        });
                    }
                }
            }
            _ => (),
        }

        // ')'
        let _ = Parser::match_next_token_type(tokens, TokenType::RightParen).ok_or_else(|| {
            ParserError {
                msg: "function declaration expects ')' after parameters".to_owned(),
                tok: tokens.next().copied(),
            }
        })?;

        // Parse the function body

        // '{'
        let _ = Parser::match_next_token_type(tokens, TokenType::LeftBrace).ok_or_else(|| {
            ParserError {
                msg: "expects '{' before function body".to_owned(),
                tok: tokens.next().copied(),
            }
        })?;

        let body = Stmt::Block {
            id: self.new_id(),
            declarations: self.block(tokens)?,
        };

        // '}'
        let _ = Parser::match_next_token_type(tokens, TokenType::RightBrace).ok_or_else(|| {
            ParserError {
                msg: "expects '}' after function body".to_owned(),
                tok: tokens.next().copied(),
            }
        })?;

        Ok(Declaration::FuncDecl {
            id: self.new_id(),
            name,
            params,
            body,
        })
    }
    fn var_decl<'de>(
        &mut self,
        tokens: &mut Peekable<Iter<Token<'de>>>,
    ) -> Result<Declaration<'de>, ParserError<'de>> {
        // var just got consumed
        // pattern is: var IDENTIFIER (= expr)? ";"
        let var_decl = match tokens.peek() {
            Some(&name_tok) if name_tok.token_type == TokenType::Identifier => {
                tokens.next();
                match tokens.peek() {
                    Some(&tok) if tok.token_type == TokenType::Equal => {
                        tokens.next();
                        let initializer = self.expression(tokens)?;
                        Declaration::VarDecl {
                            id: self.new_id(),
                            name: *name_tok,
                            initializer: Some(initializer),
                        }
                    }
                    _ => Declaration::VarDecl {
                        id: self.new_id(),
                        name: *name_tok,
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
                tok: Some(*not_semicolon),
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
    fn match_next_token_type<'de>(
        tokens: &mut Peekable<Iter<Token<'de>>>,
        nxt_expected_type: TokenType,
    ) -> Option<Token<'de>> {
        match tokens.peek() {
            Some(&next_tok) if next_tok.token_type == nxt_expected_type => {
                tokens.next(); // consume the token
                Some(*next_tok) // NOTE: remove this clone ?
            }
            _ => None,
        }
    }

    fn statement<'de>(
        &mut self,
        tokens: &mut Peekable<Iter<Token<'de>>>,
    ) -> Result<Stmt<'de>, ParserError<'de>> {
        match tokens.peek() {
            Some(tok) if tok.token_type == TokenType::Print => {
                tokens.next();
                let print_stmt = self.print_stmt(tokens)?;
                match Parser::match_next_token_type(tokens, TokenType::Semicolon) {
                    Some(_) => Ok(Stmt::PrintStmt {
                        id: self.new_id(),
                        expr: print_stmt,
                    }),
                    None => Err(ParserError {
                        msg: "print statement expects to end with a semicolon".to_owned(),
                        tok: tokens.next().copied(),
                    }),
                }
            }
            Some(&tok) if tok.token_type == TokenType::LeftBrace => {
                tokens.next(); // consume left brace
                let stmts_in_block = self.block(tokens)?;

                match Parser::match_next_token_type(tokens, TokenType::RightBrace) {
                    Some(_) => Ok(Stmt::Block {
                        id: self.new_id(),
                        declarations: stmts_in_block,
                    }),
                    None => Err(ParserError {
                        msg: "block statement expects to end with a right brace".to_owned(),
                        tok: tokens.next().copied(),
                    }),
                }
            }
            Some(&tok) if tok.token_type == TokenType::If => {
                tokens.next(); // consume "if"
                self.if_stmt(tokens)
            }
            Some(&tok) if tok.token_type == TokenType::While => {
                tokens.next(); // consume "while"
                self.while_stmt(tokens)
            }
            Some(&tok) if tok.token_type == TokenType::For => {
                tokens.next(); // consume "for"
                self.for_stmt(tokens)
            }

            Some(&tok) if tok.token_type == TokenType::Return => {
                tokens.next(); // consume "return"
                self.return_stmt(tokens)
            }
            Some(_) => {
                let expr_stmt = self.expr_stmt(tokens)?;
                Ok(Stmt::ExprStmt {
                    id: self.new_id(),
                    expr: expr_stmt,
                })
            }
            None => panic!("Statement expects a token"),
        }
    }

    fn block<'de>(
        &mut self,
        tokens: &mut Peekable<Iter<Token<'de>>>,
    ) -> Result<Vec<Declaration<'de>>, ParserError<'de>> {
        // while we don't encounter '}', we parse what is expected to be a statement
        // error if we encounter EOF before '}'
        let mut stmts = Vec::new();

        while let Some(tok) = tokens.peek() {
            if tok.token_type == TokenType::RightBrace {
                break;
            }
            stmts.push(self.declaration(tokens)?);
        }
        Ok(stmts)
    }

    fn expr_stmt<'de>(
        &mut self,
        tokens: &mut Peekable<Iter<Token<'de>>>,
    ) -> Result<Expr<'de>, ParserError<'de>> {
        let expr = self.expression(tokens)?;

        match tokens.peek() {
            Some(&tok) if tok.token_type == TokenType::Semicolon => {
                tokens.next();
                Ok(expr)
            }
            Some(&tok) => Err(ParserError {
                msg: "expression statement expects semicolon at the end".into(),
                tok: Some(*tok),
            }),
            _ => panic!("expression statement expect semicolon at the end, got no token at all"),
        }
    }

    fn print_stmt<'de>(
        &mut self,
        tokens: &mut Peekable<Iter<Token<'de>>>,
    ) -> Result<Expr<'de>, ParserError<'de>> {
        let expr = self.expression(tokens)?;

        match tokens.peek() {
            Some(&tok) if tok.token_type == TokenType::Semicolon => Ok(expr),
            Some(&tok) => Err(ParserError {
                msg: "print statement expects semicolon at the end".into(),
                tok: Some(*tok),
            }),
            _ => panic!("print statement expect semicolon at the end, got no token at all"),
        }
    }

    fn if_stmt<'de>(
        &mut self,
        tokens: &mut Peekable<Iter<Token<'de>>>,
    ) -> Result<Stmt<'de>, ParserError<'de>> {
        // "if" was already consumed
        match tokens.peek() {
            Some(tok) if tok.token_type == TokenType::LeftParen => {
                tokens.next(); // consume "("
                let condition = self.expression(tokens)?;
                match tokens.peek() {
                    Some(tok) if tok.token_type == TokenType::RightParen => {
                        tokens.next(); // consume ")"
                        let then_branch = Box::new(self.statement(tokens)?);
                        let else_branch = match tokens.peek() {
                            Some(tok) if tok.token_type == TokenType::Else => {
                                Some(Box::new(self.statement(tokens)?))
                            }
                            _ => None,
                        };
                        Ok(Stmt::IfStmt {
                            id: self.new_id(),
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
                tok: Some(*tok),
            }),
            None => Err(ParserError {
                msg: "Expects left parenthesis after 'if' keyword, got none".to_owned(),
                tok: None,
            }),
        }
    }

    fn while_stmt<'de>(
        &mut self,
        tokens: &mut Peekable<Iter<Token<'de>>>,
    ) -> Result<Stmt<'de>, ParserError<'de>> {
        // "if" was already consumed
        match tokens.peek() {
            Some(tok) if tok.token_type == TokenType::LeftParen => {
                tokens.next(); // consume "("
                let condition = self.expression(tokens)?;
                match tokens.peek() {
                    Some(tok) if tok.token_type == TokenType::RightParen => {
                        tokens.next(); // consume ")"
                        let body = Box::new(self.statement(tokens)?);
                        Ok(Stmt::WhileStmt {
                            id: self.new_id(),
                            condition,
                            body,
                        })
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
                tok: Some(*tok),
            }),
            None => Err(ParserError {
                msg: "Expects left parenthesis after 'if' keyword, got none".to_owned(),
                tok: None,
            }),
        }
    }

    fn for_stmt<'de>(
        &mut self,
        tokens: &mut Peekable<Iter<Token<'de>>>,
    ) -> Result<Stmt<'de>, ParserError<'de>> {
        if Parser::match_next_token_type(tokens, TokenType::LeftParen).is_none() {
            return Err(ParserError {
                msg: "expects a '(' after the 'for' keyword".to_owned(),
                tok: tokens.next().copied(),
            });
        }

        // parse initializer
        let initializer = match tokens.peek() {
            Some(&tok) if tok.token_type == TokenType::Semicolon => None,
            Some(&tok) if tok.token_type == TokenType::Var => {
                tokens.next(); // consume 'var'
                Some(self.var_decl(tokens)?)
            }
            _ => Some(Declaration::StmtDecl {
                id: self.new_id(),
                stmt: Stmt::ExprStmt {
                    id: self.new_id(),
                    expr: self.expression(tokens)?,
                },
            }),
        };

        let condition = match Parser::match_next_token_type(tokens, TokenType::Semicolon) {
            Some(_) => Expr::Literal {
                id: self.new_id(),
                value: LoxValue::Bool(true),
            },
            _ => {
                let expr = self.expression(tokens)?;
                if Parser::match_next_token_type(tokens, TokenType::Semicolon).is_some() {
                    expr
                } else {
                    return Err(ParserError {
                        msg: "Expects semicolon after the for loop condition".to_owned(),
                        tok: tokens.next().copied(),
                    });
                }
            }
        };

        let increment = match Parser::match_next_token_type(tokens, TokenType::RightParen) {
            Some(_) => None,
            None => {
                let expr = self.expression(tokens)?;

                // NOTE: if/else or combinator ?
                Parser::match_next_token_type(tokens, TokenType::RightParen)
                    .map(|_| Some(expr))
                    .ok_or_else(|| ParserError {
                        msg: "Expects right parenthesis at the end of the for loop initialization"
                            .to_owned(),
                        tok: tokens.next().copied(),
                    })?

                // if Parser::match_next_token_type(tokens, TokenType::RightParen).is_some() {
                //     Some(expr)
                // } else {
                //     return Err(ParserError {
                //         msg: "Expects right parenthesis at the end of the for loop initialization"
                //             .to_owned(),
                //         tok: tokens.next().copied(),
                //     });
                // }
            }
        };

        let mut body = self.statement(tokens)?;

        // desugaring the for loop
        if increment.is_some() {
            body = Stmt::Block {
                id: self.new_id(),
                declarations: [
                    Declaration::StmtDecl {
                        id: self.new_id(),
                        stmt: body,
                    },
                    Declaration::StmtDecl {
                        id: self.new_id(),
                        stmt: Stmt::ExprStmt {
                            id: self.new_id(),
                            expr: increment.unwrap(),
                        },
                    },
                ]
                .into(),
            };
        }

        body = Stmt::WhileStmt {
            id: self.new_id(),
            condition,
            body: Box::new(body),
        };

        if initializer.is_some() {
            body = Stmt::Block {
                id: self.new_id(),
                declarations: [
                    initializer.unwrap(),
                    Declaration::StmtDecl {
                        id: self.new_id(),
                        stmt: body,
                    },
                ]
                .into(),
            };
        }

        Ok(body)
    }

    fn return_stmt<'de>(
        &mut self,
        tokens: &mut Peekable<Iter<Token<'de>>>,
    ) -> Result<Stmt<'de>, ParserError<'de>> {
        // check for the *abscence* of an expression
        let maybe_return_expr = match Parser::match_next_token_type(tokens, TokenType::Semicolon) {
            Some(_) => None, // semicolon is next => no expression after the 'return' keyword
            None => {
                // return expression to parse
                let return_expr = Box::new(self.expression(tokens)?);

                // check for semicolon after the return expression
                Parser::match_next_token_type(tokens, TokenType::Semicolon)
                    .map(|_| Some(return_expr))
                    .ok_or_else(|| ParserError {
                        msg: "Expects semicolon after return statement".to_owned(),
                        tok: tokens.next().copied(),
                    })?
            }
        };

        Ok(Stmt::Return {
            id: self.new_id(),
            expr: maybe_return_expr,
        })
    }

    fn expression<'de>(
        &mut self,
        tokens: &mut Peekable<Iter<Token<'de>>>,
    ) -> Result<Expr<'de>, ParserError<'de>> {
        self.assignment(tokens)
    }

    fn assignment<'de>(
        &mut self,
        tokens: &mut Peekable<Iter<Token<'de>>>,
    ) -> Result<Expr<'de>, ParserError<'de>> {
        let expr = self.logic_or(tokens)?;

        match tokens.peek() {
            Some(&tok_equal) if tok_equal.token_type == TokenType::Equal => {
                tokens.next();
                let value = self.assignment(tokens)?;
                match expr {
                    Expr::Variable { id: _, name } => Ok(Expr::Assign {
                        id: self.new_id(), // NOTE: new id here?
                        name,
                        value: Box::new(value),
                    }),
                    Expr::Get {
                        id: _,
                        object,
                        name,
                    } => Ok(
                        Expr::Set {
                            id: self.new_id(),
                            object: object.clone(),
                            name,
                            value: Box::new(value),
                        }, // NOTE: neccesary clone ?
                    ),
                    _ => Err(ParserError {
                        msg: "Invalid assignement target".to_owned(),
                        tok: Some(*tok_equal),
                    }),
                }
            }
            _ => Ok(expr),
        }
    }

    fn logic_or<'de>(
        &mut self,
        tokens: &mut Peekable<Iter<Token<'de>>>,
    ) -> Result<Expr<'de>, ParserError<'de>> {
        let mut expr = self.logic_and(tokens)?;

        while Parser::match_next_token_type(tokens, TokenType::Or).is_some() {
            let right = self.logic_and(tokens)?;
            expr = Expr::Logical {
                id: self.new_id(),
                left: Box::new(expr),
                op: LogicOp::Or,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }
    fn logic_and<'de>(
        &mut self,
        tokens: &mut Peekable<Iter<Token<'de>>>,
    ) -> Result<Expr<'de>, ParserError<'de>> {
        let mut expr = self.equality(tokens)?;

        while Parser::match_next_token_type(tokens, TokenType::And).is_some() {
            let right = self.equality(tokens)?;
            expr = Expr::Logical {
                id: self.new_id(),
                left: Box::new(expr),
                op: LogicOp::And,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn equality<'de>(
        &mut self,
        tokens: &mut Peekable<Iter<Token<'de>>>,
    ) -> Result<Expr<'de>, ParserError<'de>> {
        let mut expr = self.comparison(tokens)?;

        while let Some(&tok) = tokens.peek() {
            match tok.token_type {
                TokenType::EqualEqual | TokenType::BangEqual => {
                    tokens.next();
                    let left = Box::new(expr);
                    let op = BinaryOp::from(tok.token_type);
                    let right = Box::new(self.comparison(tokens)?);
                    expr = Expr::Binary {
                        id: self.new_id(),
                        left,
                        op,
                        right,
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    // NOTE: why the mistake of () instead of break in both equality and comparison make the
    // program hangs on the test example ?
    // NOTE: understand the need for references around the peekable tokens and in Some(&tok) =
    // tokens.peek()
    fn comparison<'de>(
        &mut self,
        tokens: &mut Peekable<Iter<Token<'de>>>,
    ) -> Result<Expr<'de>, ParserError<'de>> {
        let mut expr = self.term(tokens)?;

        while let Some(&tok) = tokens.peek() {
            match tok.token_type {
                TokenType::Less
                | TokenType::LessEqual
                | TokenType::Greater
                | TokenType::GreaterEqual => {
                    tokens.next();
                    let left = Box::new(expr);
                    let op = BinaryOp::from(tok.token_type);
                    let right = Box::new(self.term(tokens)?);
                    expr = Expr::Binary {
                        id: self.new_id(),
                        left,
                        op,
                        right,
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }
    fn term<'de>(
        &mut self,
        tokens: &mut Peekable<Iter<Token<'de>>>,
    ) -> Result<Expr<'de>, ParserError<'de>> {
        let mut expr = self.factor(tokens)?;

        while let Some(&tok) = tokens.peek() {
            match tok.token_type {
                TokenType::Plus | TokenType::Minus => {
                    tokens.next();
                    let left = Box::new(expr);
                    let op = BinaryOp::from(tok.token_type);
                    let id = self.new_id();
                    let right = Box::new(self.comparison(tokens)?);
                    expr = Expr::Binary {
                        id,
                        left,
                        op,
                        right,
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }
    fn factor<'de>(
        &mut self,
        tokens: &mut Peekable<Iter<Token<'de>>>,
    ) -> Result<Expr<'de>, ParserError<'de>> {
        let mut expr = self.unary(tokens)?;

        while let Some(&tok) = tokens.peek() {
            match tok.token_type {
                TokenType::Slash | TokenType::Star => {
                    tokens.next();
                    let left = Box::new(expr);
                    let op = BinaryOp::from(tok.token_type);
                    let right = Box::new(self.comparison(tokens)?); // NOTE: error goes away once
                                                                    // ParserError lifetime is annotated !
                    expr = Expr::Binary {
                        id: self.new_id(),
                        left,
                        op,
                        right,
                    }
                }
                _ => break,
            }
        }

        Ok(expr)
    }
    fn unary<'de>(
        &mut self,
        tokens: &mut Peekable<Iter<Token<'de>>>,
    ) -> Result<Expr<'de>, ParserError<'de>> {
        match tokens.peek() {
            Some(&tok)
                if tok.token_type == TokenType::Bang || tok.token_type == TokenType::Minus =>
            {
                tokens.next();
                let op = UnaryOp::from(tok.token_type);
                let right = Box::new(self.unary(tokens)?);
                Ok(Expr::Unary {
                    id: self.new_id(),
                    op,
                    right,
                })
            }
            _ => self.call(tokens),
        }
    }
    fn parse_call_args<'de>(
        &mut self,
        tokens: &mut Peekable<Iter<Token<'de>>>,
    ) -> Result<Vec<Expr<'de>>, ParserError<'de>> {
        let mut args = Vec::new();

        // Parse arguments
        match tokens.peek() {
            Some(tok) if tok.token_type != TokenType::RightParen => {
                // first arg
                args.push(self.expression(tokens)?);

                // follow-up args
                while Parser::match_next_token_type(tokens, TokenType::Comma).is_some() {
                    if args.len() < 255 {
                        args.push(self.expression(tokens)?);
                    } else {
                        return Err(ParserError {
                            msg: "Can't have more than 255 arguments".to_owned(),
                            tok: tokens.next().copied(), // NOTE: the use of cloned()
                        });
                    }
                }
            }
            _ => (),
        }
        Ok(args)
    }
    fn call<'de>(
        &mut self,
        tokens: &mut Peekable<Iter<Token<'de>>>,
    ) -> Result<Expr<'de>, ParserError<'de>> {
        let mut expr = self.primary(tokens)?;

        loop {
            match tokens.peek() {
                Some(&tok) if tok.token_type == TokenType::LeftParen => {
                    // Parse function call

                    tokens.next(); // consume left parenthesis '('
                    let args = self.parse_call_args(tokens)?;

                    match Parser::match_next_token_type(tokens, TokenType::RightParen) {
                        Some(_) => {
                            expr = Expr::Call {
                                id: self.new_id(),
                                callee: Box::new(expr),
                                arguments: Box::new(args),
                            }
                        }
                        None => {
                            return Err(ParserError {
                                msg: "function call expects ')' after arguments".to_owned(),
                                tok: tokens.next().copied(),
                            })
                        }
                    }
                }
                Some(&tok) if tok.token_type == TokenType::Dot => {
                    // Parse getter call on a class instance
                    tokens.next(); // consume the dot '.'

                    let name = match Parser::match_next_token_type(tokens, TokenType::Identifier) {
                        Some(tok) => tok,
                        _ => {
                            return Err(ParserError {
                                msg: "getter call expects an identifier after the dot".to_owned(),
                                tok: tokens.next().copied(),
                            })
                        }
                    };
                    expr = Expr::Get {
                        id: self.new_id(),
                        object: Box::new(expr),
                        name,
                    }
                }
                _ => break,
            }
        }
        Ok(expr)
    }
    fn primary<'de>(
        &mut self,
        tokens: &mut Peekable<Iter<Token<'de>>>,
    ) -> Result<Expr<'de>, ParserError<'de>> {
        if let Some(&tok) = tokens.peek() {
            tokens.next(); // consume the token
            match tok.token_type {
                TokenType::Number => Ok(Expr::Literal {
                    id: self.new_id(),
                    value: LoxValue::Number(
                        tok.literal
                            .expect("Number token should have a literal")
                            .parse()
                            .expect("Number token should parse to a f32"),
                    ),
                }),
                TokenType::String => Ok(Expr::Literal {
                    id: self.new_id(),
                    value: LoxValue::String(
                        tok.literal.expect("String should have literal").to_string(),
                    ),
                }),
                TokenType::True => Ok(Expr::Literal {
                    id: self.new_id(),
                    value: LoxValue::Bool(true),
                }),
                TokenType::False => Ok(Expr::Literal {
                    id: self.new_id(),
                    value: LoxValue::Bool(false),
                }),
                TokenType::Nil => Ok(Expr::Literal {
                    id: self.new_id(),
                    value: LoxValue::Nil,
                }),
                TokenType::LeftParen => {
                    tokens.next();
                    let expr = self.expression(tokens)?;
                    match tokens.peek() {
                        Some(&tok) if tok.token_type == TokenType::RightParen => (),
                        // Some(&tok) if tok.token_type == TokenType::RightParen => tokens.next(), // consume right parenthesis
                        _ => panic!("Expect right parenthesis after expression. Got {tok}"),
                    }
                    Ok(Expr::Grouping {
                        id: self.new_id(),
                        group: Box::new(expr),
                    })
                }
                TokenType::Identifier => Ok(Expr::Variable {
                    id: self.new_id(),
                    name: *tok,
                }),
                TokenType::This => Ok(Expr::This {
                    id: self.new_id(),
                    keyword: *tok,
                }),
                _ => Err(ParserError {
                    msg: "Expect expression".into(),
                    tok: Some(*tok), // NOTE: after cloning, no need to dereference with *, why?
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
                lexeme: "hello_world",
                literal: None,
                line: 1,
            },
            Token {
                token_type: TokenType::LeftParen,
                lexeme: "(",
                literal: None,
                line: 1,
            },
            Token {
                token_type: TokenType::Number,
                lexeme: "1.0",
                literal: Some("1.0"),
                line: 1,
            },
            Token {
                token_type: TokenType::RightParen,
                lexeme: ")",
                literal: None,
                line: 1,
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";",
                literal: None,
                line: 1,
            },
            Token {
                token_type: TokenType::Eof,
                lexeme: "",
                literal: None,
                line: 1,
            },
        ]);

        let mut parser = Parser::default();

        let ast = parser.parse(&tokens).expect("expects parsing not to fail");

        let gt_fn_call_ast: Declaration = Declaration::StmtDecl {
            id: 1,
            stmt: Stmt::ExprStmt {
                id: 2,
                expr: Expr::Call {
                    id: 3,
                    callee: Box::new(Expr::Variable {
                        id: 4,
                        name: Token {
                            token_type: TokenType::Identifier,
                            lexeme: "hello_world",
                            literal: None,
                            line: 1,
                        },
                    }),
                    arguments: Box::new(Vec::from([Expr::Literal {
                        id: 5,
                        value: LoxValue::Number(1.0),
                    }])),
                },
            },
        };

        assert!(ast[0] == gt_fn_call_ast);
    }
}
