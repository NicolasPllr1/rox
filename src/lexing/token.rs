#![allow(dead_code)]

use core::fmt;
use std::{collections::HashMap, str::FromStr};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals
    Identifier,
    String,
    Number,
    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    //
    Eof,
}
impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let token_type_str = match self {
            Self::LeftParen => "LEFT_PAREN",
            Self::RightParen => "RIGHT_PAREN",
            Self::LeftBrace => "LEFT_BRACE",
            Self::RightBrace => "RIGHT_BRACE",
            Self::Comma => "COMMA",
            Self::Dot => "DOT",
            Self::Minus => "MINUS",
            Self::Plus => "PLUS",
            Self::Semicolon => "SEMICOLON",
            Self::Slash => "SLASH",
            Self::Star => "STAR",
            // One or two character tokens
            Self::Bang => "BANG",
            Self::BangEqual => "BANG_EQUAL",
            Self::Equal => "EQUAL",
            Self::EqualEqual => "EQUAL_EQUAL",
            Self::Greater => "GREATER",
            Self::GreaterEqual => "GREATER_EQUAL",
            Self::Less => "LESS",
            Self::LessEqual => "LESS_EQUAL",
            // Literals
            Self::Identifier => "IDENTIFIER",
            Self::String => "STRING",
            Self::Number => "NUMBER",
            // Keywords
            Self::And => "AND",
            Self::Class => "CLASS",
            Self::Else => "ELSE",
            Self::False => "FALSE",
            Self::Fun => "FUN",
            Self::For => "FOR",
            Self::If => "IF",
            Self::Nil => "NIL",
            Self::Or => "OR",
            Self::Print => "PRINT",
            Self::Return => "RETURN",
            Self::Super => "SUPER",
            Self::This => "THIS",
            Self::True => "TRUE",
            Self::Var => "VAR",
            Self::While => "WHILE",
            //
            Self::Eof => "EOF",
        };

        write!(f, "{token_type_str}")
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct Token<'de> {
    pub token_type: TokenType,
    pub lexeme: &'de str, //  raw substrings from the source code
    pub literal: Option<&'de str>,
    pub line: usize,
}

//   public String toString() {
//     return type + " " + lexeme + " " + literal;
//   }
// }
impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let lox_literal = match self.token_type {
            TokenType::Number => {
                let original_lexeme = self.lexeme;
                if original_lexeme.contains('.') {
                    original_lexeme.to_string()
                } else {
                    format!("{original_lexeme}.0") // particularity
                }
            }
            _ => self.literal.map_or(
                String::from_str("null").expect("expect valid string"),
                |s| s.to_string(),
            ),
        };

        write!(f, "{} {} {}", self.token_type, self.lexeme, lox_literal,)
    }
}

#[derive(Debug, PartialEq)]
pub enum ReservedKeyword {
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

pub fn create_keywords_map() -> HashMap<&'static str, TokenType> {
    let mut map = HashMap::new();

    map.insert("and", TokenType::And);
    map.insert("class", TokenType::Class);
    map.insert("else", TokenType::Else);
    map.insert("false", TokenType::False);
    map.insert("for", TokenType::For);
    map.insert("fun", TokenType::Fun);
    map.insert("if", TokenType::If);
    map.insert("nil", TokenType::Nil);
    map.insert("or", TokenType::Or);
    map.insert("print", TokenType::Print);
    map.insert("return", TokenType::Return);
    map.insert("super", TokenType::Super);
    map.insert("this", TokenType::This);
    map.insert("true", TokenType::True);
    map.insert("var", TokenType::Var);
    map.insert("while", TokenType::While);

    map
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keywords_map_contains_all_keywords() {
        let map = create_keywords_map();

        assert_eq!(map.len(), 16); // Check if all 16 keywords are present

        assert_eq!(map.get("and"), Some(&TokenType::And));
        assert_eq!(map.get("class"), Some(&TokenType::Class));
        assert_eq!(map.get("else"), Some(&TokenType::Else));
        assert_eq!(map.get("false"), Some(&TokenType::False));
        assert_eq!(map.get("for"), Some(&TokenType::For));
        assert_eq!(map.get("fun"), Some(&TokenType::Fun));
        assert_eq!(map.get("if"), Some(&TokenType::If));
        assert_eq!(map.get("nil"), Some(&TokenType::Nil));
        assert_eq!(map.get("or"), Some(&TokenType::Or));
        assert_eq!(map.get("print"), Some(&TokenType::Print));
        assert_eq!(map.get("return"), Some(&TokenType::Return));
        assert_eq!(map.get("super"), Some(&TokenType::Super));
        assert_eq!(map.get("this"), Some(&TokenType::This));
        assert_eq!(map.get("true"), Some(&TokenType::True));
        assert_eq!(map.get("var"), Some(&TokenType::Var));
        assert_eq!(map.get("while"), Some(&TokenType::While));

        // Test a non-keyword
        assert_eq!(map.get("notakeyword"), None);
    }
}
