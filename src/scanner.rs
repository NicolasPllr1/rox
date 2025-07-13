use crate::token::{create_keywords_map, Token, TokenType};

#[allow(dead_code)]
#[derive(Debug)]
pub struct Scanner {
    pub tokens: Vec<Token>,
    // pub keywords: HashMap<&'static str, TokenType>,
    errors: Option<Vec<ScannerError>>,
}

#[allow(dead_code)]
#[derive(Debug)]
enum ScannerError {
    UnexpectedChar((usize, char)), // usize for the line nb
    UnterminatedString(usize),     // usize for the line nb
}

impl Scanner {
    pub fn scan_tokens(source: &str) -> Scanner {
        let mut tokens: Vec<Token> = Vec::new();
        let mut errors = Vec::new();

        let str_to_keywords = create_keywords_map();

        let mut chars = source.char_indices().peekable();
        let mut start;
        let mut line = 1;
        while let Some((mut current_idx, c)) = chars.next() {
            start = current_idx;

            let mut token_type: TokenType = match c {
                '(' => TokenType::LeftParen,
                ')' => TokenType::RightParen,
                '{' => TokenType::LeftBrace,
                '}' => TokenType::RightBrace,
                '.' => TokenType::Dot,
                ',' => TokenType::Comma,
                '-' => TokenType::Minus,
                '+' => TokenType::Plus,
                ';' => TokenType::Semicolon,
                '*' => TokenType::Star,
                '!' => match chars.peek() {
                    Some((_, '=')) => {
                        chars.next(); // consume the '=' we just peeked at
                        current_idx += 1;
                        TokenType::BangEqual
                    }
                    _ => TokenType::Bang,
                },
                '=' => match chars.peek() {
                    Some((_, '=')) => {
                        chars.next(); // consume the '=' we just peeked at
                        current_idx += 1;
                        TokenType::EqualEqual
                    }
                    _ => TokenType::Equal,
                },
                '<' => match chars.peek() {
                    Some((_, '=')) => {
                        chars.next();
                        current_idx += 1;
                        TokenType::LessEqual
                    }
                    _ => TokenType::Less,
                },
                '>' => match chars.peek() {
                    Some((_, '=')) => {
                        chars.next();
                        current_idx += 1;
                        TokenType::GreaterEqual
                    }
                    _ => TokenType::Greater,
                },
                '\n' => {
                    line += 1;
                    continue;
                }
                ' ' | '\r' | '\t' => {
                    // println!("ignoring whitespace, start: {start}");
                    continue; // ignore whitespace
                }
                '/' => match chars.peek() {
                    Some((_, '/')) => {
                        // consume line (comment)
                        while chars.peek().is_some_and(|(_, c)| *c != '\n') {
                            chars.next();
                            current_idx += 1;
                        }
                        continue;
                    }
                    _ => TokenType::Slash,
                },
                '"' => {
                    // consume the string literal within quotes
                    while let Some((_, c_in_string)) = chars.peek() {
                        if *c_in_string == '\n' {
                            chars.next();
                            current_idx += 1;
                            line += 1;
                        } else if *c_in_string != '"' {
                            chars.next();
                            current_idx += 1;
                        } else {
                            assert!(*c_in_string == '"');
                            break;
                        }
                    }

                    // check and consume the end quotation mark
                    match chars.peek() {
                        Some((_, c)) => {
                            assert!(*c == '"'); // check
                            chars.next(); // consume
                            current_idx += 1;
                            TokenType::String
                        }
                        None => {
                            errors.push(ScannerError::UnterminatedString(line));
                            continue;
                        }
                    }
                }
                d if d.is_ascii_digit() => {
                    // number is: integer.decimal
                    while let Some((_, nxt_dgt)) = chars.peek() {
                        if nxt_dgt.is_ascii_digit() {
                            chars.next();
                            current_idx += 1;
                        } else {
                            break;
                        }
                    }
                    TokenType::Number
                }
                l if l.is_alphabetic() || l == '_' => {
                    while let Some((_, nxt_l)) = chars.peek() {
                        if *nxt_l != ' '
                            && *nxt_l != '/'
                            && !nxt_l.is_ascii_control()
                            && *nxt_l != ';'
                            && *nxt_l != ')'
                            && *nxt_l != '('
                        {
                            chars.next();
                            current_idx += 1;
                        } else {
                            break;
                        }
                    }
                    TokenType::Identifier
                }
                _ => {
                    errors.push(ScannerError::UnexpectedChar((line, c)));
                    continue;
                }
            };

            let lexeme = match token_type {
                TokenType::String => &source[start..current_idx + 1], // include quotations marks
                // for the lexeme
                // string literal
                TokenType::Number => {
                    // TODO: (nico) study the `ref` here
                    if tokens.len() >= 2
                        && tokens[tokens.len() - 1].token_type == TokenType::Dot
                        && tokens[tokens.len() - 2].token_type == TokenType::Number
                        && !tokens[tokens.len() - 2].lexeme.contains(".")
                    // don't want
                    // last number to already be integer.decimal
                    {
                        // Number is of the form {integer}.{decimal}
                        let _dot = tokens.pop().unwrap(); // TODO: (nico) I "know" unwrap should
                                                          // not err here as the if statement checks for existence. Better way to
                                                          // write this ?
                        let integer_part = tokens.pop().unwrap().lexeme;
                        let decimal_part = &source[start..current_idx + 1];

                        // pop the "{integer}" and the "."

                        &format!("{integer_part}.{decimal_part}") // line number did not change
                                                                  // since {integer} first got parsed, so no need to do anything special for this
                    } else {
                        &source[start..current_idx + 1]
                    }
                }
                TokenType::Identifier => {
                    let lexeme = &source[start..current_idx + 1];
                    // maximal munch: literal or reserved keywords ?
                    token_type = *str_to_keywords
                        .get(lexeme)
                        .unwrap_or(&TokenType::Identifier);
                    lexeme
                }
                _ => &source[start..current_idx + 1],
            };

            let literal = match token_type {
                TokenType::String => match lexeme.len() {
                    // goal is to strip quotation marks
                    2 => Some("".to_string()), // from "\"\"" to an empty string
                    n if n > 2 => {
                        let l = &lexeme[1..lexeme.len() - 1];
                        Some(l.to_string())
                    }
                    _ => {
                        panic!("String literals should at least be the \"\", got: {lexeme}")
                    }
                },
                TokenType::Number => {
                    if lexeme.contains(".") {
                        Some(lexeme.to_string())
                    } else {
                        Some(format!("{lexeme}.0"))
                    }
                }
                _ => None,
            };

            tokens.push(Token {
                token_type,
                lexeme: lexeme.into(),
                literal,
                line,
            });
        }

        tokens.push(Token {
            token_type: TokenType::Eof,
            lexeme: "".to_string(),
            literal: None,
            line,
        });

        // eprint!("Errors: {errors:?}");

        Scanner {
            tokens,
            errors: if errors.is_empty() {
                None
            } else {
                Some(errors)
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn open_close_parent() {
        let source = String::from("()");
        let scanner = Scanner::scan_tokens(&source);

        let toks = scanner.tokens;

        let gt = vec![
            Token {
                token_type: TokenType::LeftParen,
                lexeme: "(".to_string(),
                literal: None,
                line: 1,
            },
            Token {
                token_type: TokenType::RightParen,
                lexeme: ")".to_string(),
                literal: None,
                line: 1,
            },
            Token {
                token_type: TokenType::Eof,
                lexeme: "".to_string(),
                literal: None,
                line: 1,
            },
        ];

        assert!(toks == gt);
    }

    #[test]
    fn two_comments() {
        let source = "// this is a comment\n//another one\n";

        let scanner = Scanner::scan_tokens(&source);

        let toks = scanner.tokens;

        let gt = vec![Token {
            token_type: TokenType::Eof,
            lexeme: "".to_string(),
            literal: None,
            line: 3,
        }]; // comments are ignored by the parser (~skipped) but counts line-wise

        assert!(toks == gt);
    }

    #[test]
    fn number() {
        let source = "123.456\n";

        let scanner = Scanner::scan_tokens(&source);

        let toks = scanner.tokens;

        let gt = vec![
            Token {
                token_type: TokenType::Number,
                lexeme: "123.456".to_string(),
                literal: Some("123.456".to_string()),
                line: 1,
            },
            Token {
                token_type: TokenType::Eof,
                lexeme: "".to_string(),
                literal: None,
                line: 2,
            },
        ]; // comments are ignored by the parser (~skipped)

        assert!(toks == gt);
    }
    #[test]
    fn number_tricky() {
        let source = "123.456 .789";

        let scanner = Scanner::scan_tokens(&source);

        let toks = scanner.tokens;

        let gt = vec![
            Token {
                token_type: TokenType::Number,
                lexeme: "123.456".to_string(),
                literal: Some("123.456".to_string()),
                line: 1,
            },
            Token {
                token_type: TokenType::Dot,
                lexeme: ".".to_string(),
                literal: None,
                line: 1,
            },
            Token {
                token_type: TokenType::Number,
                lexeme: "789".to_string(),
                literal: Some("789.0".to_string()),
                line: 1,
            },
            Token {
                token_type: TokenType::Eof,
                lexeme: "".to_string(),
                literal: None,
                line: 1,
            },
        ];

        assert!(toks == gt);
    }

    #[test]
    fn bang_number() {
        let source = ".456";

        let scanner = Scanner::scan_tokens(&source);

        let toks = scanner.tokens;

        let gt = vec![
            Token {
                token_type: TokenType::Dot,
                lexeme: ".".to_string(),
                literal: None,
                line: 1,
            },
            Token {
                token_type: TokenType::Number,
                lexeme: "456".to_string(),
                literal: Some("456.0".to_string()),
                line: 1,
            },
            Token {
                token_type: TokenType::Eof,
                lexeme: "".to_string(),
                literal: None,
                line: 1,
            },
        ];

        assert!(toks == gt);
    }

    #[test]
    fn bang_equal() {
        let source = "!=";

        let scanner = Scanner::scan_tokens(&source);

        let toks = scanner.tokens;

        let gt = vec![
            Token {
                token_type: TokenType::BangEqual,
                lexeme: "!=".to_string(),
                literal: None,
                line: 1,
            },
            Token {
                token_type: TokenType::Eof,
                lexeme: "".to_string(),
                literal: None,
                line: 1,
            },
        ];

        assert!(toks == gt);
    }

    #[test]
    fn string_literal() {
        let source = "(\"hello\")";

        let scanner = Scanner::scan_tokens(&source);

        let toks = scanner.tokens;

        let gt = vec![
            Token {
                token_type: TokenType::LeftParen,
                lexeme: "(".to_string(),
                literal: None,
                line: 1,
            },
            Token {
                token_type: TokenType::String,
                lexeme: "\"hello\"".to_string(),
                literal: Some("hello".into()),
                line: 1,
            },
            Token {
                token_type: TokenType::RightParen,
                lexeme: ")".to_string(),
                literal: None,
                line: 1,
            },
            Token {
                token_type: TokenType::Eof,
                lexeme: "".to_string(),
                literal: None,
                line: 1,
            },
        ];

        assert!(toks == gt);
    }

    #[test]
    fn multi_line_string() {
        let source = "\"lorem\nipsum\"";

        let scanner = Scanner::scan_tokens(&source);

        let toks = scanner.tokens;

        let gt = vec![
            Token {
                token_type: TokenType::String,
                lexeme: "\"lorem\nipsum\"".to_string(),
                literal: Some("lorem\nipsum".to_string()),
                line: 2,
            },
            Token {
                token_type: TokenType::Eof,
                lexeme: "".to_string(),
                literal: None,
                line: 2,
            },
        ];

        assert!(toks == gt);
    }

    #[test]
    fn integer_and_decimal_nb() {
        let source = "123.456";

        let scanner = Scanner::scan_tokens(&source);

        let toks = scanner.tokens;

        let gt = vec![
            Token {
                token_type: TokenType::Number,
                lexeme: "123.456".to_string(),
                literal: Some("123.456".into()),
                line: 1,
            },
            Token {
                token_type: TokenType::Eof,
                lexeme: "".to_string(),
                literal: None,
                line: 1,
            },
        ];

        assert!(toks == gt);
    }

    #[test]
    fn reserved_keywords() {
        let source = "and andy if ifo else felse";

        let scanner = Scanner::scan_tokens(&source);

        let toks = scanner.tokens;

        let gt = vec![
            Token {
                token_type: TokenType::And,
                lexeme: "and".to_string(),
                literal: None,
                line: 1,
            },
            Token {
                token_type: TokenType::Identifier,
                lexeme: "andy".to_string(),
                literal: None,
                line: 1,
            },
            Token {
                token_type: TokenType::If,
                lexeme: "if".to_string(),
                literal: None,
                line: 1,
            },
            Token {
                token_type: TokenType::Identifier,
                lexeme: "ifo".to_string(),
                literal: None,
                line: 1,
            },
            Token {
                token_type: TokenType::Else,
                lexeme: "else".to_string(),
                literal: None,
                line: 1,
            },
            Token {
                token_type: TokenType::Identifier,
                lexeme: "felse".to_string(),
                literal: None,
                line: 1,
            },
            Token {
                token_type: TokenType::Eof,
                lexeme: "".to_string(),
                literal: None,
                line: 1,
            },
        ];

        assert!(toks == gt);
    }
}
