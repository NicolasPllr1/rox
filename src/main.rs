use rox::EvaluationError;
use rox::Interpreter;
use rox::Resolver;
use rox::Scanner;
use rox::{Parser, ParserError};
use std::fmt;
use std::fs;
use std::io::{self, BufRead};

fn main() {
    let mut args = std::env::args();
    args.next();

    if let Some(filename) = args.next() {
        let source = fs::read_to_string(filename).unwrap_or_else(|e| {
            eprintln!("I/O error: {e}");
            std::process::exit(1);
        });
        let _ = run(&source).unwrap_or_else(|e| {
            eprintln!("Error: {e}");
            std::process::exit(1);
        });
    } else {
        let _ = run_prompt().unwrap();
    }
}

fn run<'de>(source: &'de str) -> Result<(), InterpreterError<'de>> {
    println!("Scanning/Lexing:");
    let scanner = Scanner::scan_tokens(source);
    let tokens = scanner.tokens;
    println!("{tokens:?}");

    println!("\nParsing:");
    let mut parser = Parser::new();
    let declarations = parser.parse(tokens)?;
    for decl in &declarations {
        println!("{decl:?}");
    }

    println!("\nResolving");
    let mut resolver = Resolver::new();
    resolver.resolve(&declarations);
    dbg!(&resolver.locals);

    println!("\nEvaluation:");
    let mut interpreter = Interpreter::new(resolver.locals);
    interpreter.evaluate(declarations);
    Ok(())
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
enum InterpreterError<'de> {
    Io(std::io::Error),
    Parser(ParserError<'de>),
    Evaluation,
}

impl fmt::Display for InterpreterError<'_> {
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

            InterpreterError::Evaluation => write!(f, "Evaluation error")?,
        }
        Ok(())
    }
}

impl From<std::io::Error> for InterpreterError<'_> {
    fn from(e: std::io::Error) -> Self {
        InterpreterError::Io(e)
    }
}

impl<'de> From<ParserError<'de>> for InterpreterError<'de> {
    fn from(e: ParserError<'de>) -> Self {
        InterpreterError::Parser(e)
    }
}
impl<'de> From<EvaluationError<'de>> for InterpreterError<'de> {
    fn from(_: EvaluationError) -> Self {
        InterpreterError::Evaluation
    }
}
