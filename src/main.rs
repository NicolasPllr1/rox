use rox::Scanner;
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

fn run(source: &str) {
    let scanner = Scanner::scan_tokens(source);

    for tok in scanner.tokens {
        println!("{tok}");
    }
}

fn run_file(filename: &str) -> Result<(), std::io::Error> {
    let raw_file_content = fs::read_to_string(filename)?;

    run(&raw_file_content);
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
