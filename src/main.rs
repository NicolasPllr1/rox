use core::fmt;
use rox::EvaluationError;
use rox::Interpreter;
use rox::Scanner;
use rox::{Parser, ParserError};
use std::io::{self, BufRead};

use std::fs;

fn main() -> Result<(), InterpreterError> {
    let mut args = std::env::args();
    args.next();

    if let Some(filename) = args.next() {
        run_file(&filename)?;
    } else {
        run_prompt()?;
    }

    Ok(())
}

fn run(source: &str) -> Result<(), InterpreterError> {
    let scanner = Scanner::scan_tokens(source);
    let tokens = scanner.tokens;
    println!("Tokens:\n{tokens:?}\n");

    let declarations = Parser::parse(tokens)?;
    for decl in &declarations {
        println!("{decl:?}");
    }

    let mut interpreter = Interpreter::default();
    interpreter.evaluate(declarations);
    Ok(())
}

fn run_file(filename: &str) -> Result<(), InterpreterError> {
    let raw_file_content = fs::read_to_string(filename)?;
    run(&raw_file_content)
}
fn run_prompt() -> Result<(), std::io::Error> {
    let mut buffer = String::new();
    let stdin = io::stdin();
    let mut handle = stdin.lock();

    loop {
        handle.read_line(&mut buffer)?;

        buffer.clear();
    }
}

#[derive(Debug)]
enum InterpreterError {
    Io(std::io::Error),
    Parser(ParserError),
    Evaluation(EvaluationError),
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InterpreterError::Io(e) => write!(f, "Io Error: {e}")?,
            InterpreterError::Parser(e) => {
                write!(f, "Parser Error: {}", e.msg)?;
                if let Some(tok) = &e.tok {
                    // NOTE: why need to borrow here?
                    write!(f, "at token {tok}")?;
                }
            }

            InterpreterError::Evaluation(_) => write!(f, "Evaluation error")?,
        }
        Ok(())
    }
}

impl From<std::io::Error> for InterpreterError {
    fn from(e: std::io::Error) -> Self {
        InterpreterError::Io(e)
    }
}

impl From<ParserError> for InterpreterError {
    fn from(e: ParserError) -> Self {
        InterpreterError::Parser(e)
    }
}
impl From<EvaluationError> for InterpreterError {
    fn from(e: EvaluationError) -> Self {
        InterpreterError::Evaluation(e)
    }
}
