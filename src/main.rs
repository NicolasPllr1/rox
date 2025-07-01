use rox::Parser;
use rox::Scanner;
use rox::Token;
use std::io::{self, BufRead};

use std::fs;

fn main() -> Result<(), std::io::Error> {
    let mut args = std::env::args();
    args.next();

    if let Some(filename) = args.next() {
        run_file(&filename)?;
    } else {
        run_prompt()?;
    }

    Ok(())
}

fn run(source: &str) -> Vec<Token> {
    let scanner = Scanner::scan_tokens(source);

    // for tok in scanner.tokens {
    //     println!("{tok}");
    // }

    scanner.tokens
}

fn run_file(filename: &str) -> Result<(), std::io::Error> {
    let raw_file_content = fs::read_to_string(filename)?;

    let tokens = run(&raw_file_content);

    println!("Tokens\n:{tokens:?}");

    parse(tokens);

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

fn parse(tokens: Vec<Token>) {
    let statements =
        Parser::parse(tokens).expect("tokens should map correctly to a list of statements");

    // let _ = statements.into_iter().map(|stmt| stmt.evaluate());
    for stmt in statements {
        let _val = stmt.evaluate();
    }
}
