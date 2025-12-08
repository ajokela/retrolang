//! Lexer/Tokenizer for RetroLang
//!
//! Uses the `logos` crate for efficient lexical analysis.

use logos::Logos;
use std::fmt;

/// Source location for error reporting
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

/// Token with associated span
#[derive(Debug, Clone, PartialEq)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

/// All tokens in the RetroLang language
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\r]+")]  // Skip whitespace (but not newlines)
#[logos(skip r"//[^\n]*")]  // Skip line comments
pub enum Token {
    // === Keywords ===
    #[token("var")]
    Var,
    #[token("const")]
    Const,
    #[token("proc")]
    Proc,
    #[token("func")]
    Func,
    #[token("return")]
    Return,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("elsif")]
    Elsif,
    #[token("else")]
    Else,
    #[token("end")]
    End,
    #[token("while")]
    While,
    #[token("do")]
    Do,
    #[token("for")]
    For,
    #[token("to")]
    To,
    #[token("downto")]
    Downto,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("and")]
    And,
    #[token("or")]
    Or,
    #[token("not")]
    Not,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("asm")]
    Asm,
    #[token("mem")]
    Mem,
    #[token("memw")]
    Memw,

    // === Types ===
    #[token("byte")]
    TypeByte,
    #[token("int")]
    TypeInt,
    #[token("bool")]
    TypeBool,
    #[token("char")]
    TypeChar,

    // === Operators ===
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("=")]
    Eq,
    #[token("<>")]
    Neq,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    Le,
    #[token(">=")]
    Ge,
    #[token("&")]
    Ampersand,
    #[token("|")]
    Pipe,
    #[token("^")]
    Caret,
    #[token("~")]
    Tilde,
    #[token("<<")]
    Shl,
    #[token(">>")]
    Shr,
    #[token("@")]
    At,
    #[token(":=")]
    Assign,

    // === Delimiters ===
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(":")]
    Colon,
    #[token(";")]
    Semi,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,

    // === Literals ===
    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i32>().ok())]
    IntLit(i32),

    #[regex(r"0x[0-9a-fA-F]+", parse_hex)]
    HexLit(i32),

    #[regex(r"0b[01]+", parse_binary)]
    BinLit(i32),

    #[regex(r"'[^'\\]'|'\\[nrt0\\']'", parse_char)]
    CharLit(u8),

    #[regex(r#""[^"]*""#, parse_string)]
    StringLit(String),

    // === Identifiers ===
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Ident(String),

    // === Special ===
    #[token("\n")]
    Newline,

    // Error token for unrecognized input
    Error,
}

fn parse_hex(lex: &logos::Lexer<Token>) -> Option<i32> {
    let s = lex.slice();
    i32::from_str_radix(&s[2..], 16).ok()
}

fn parse_binary(lex: &logos::Lexer<Token>) -> Option<i32> {
    let s = lex.slice();
    i32::from_str_radix(&s[2..], 2).ok()
}

fn parse_char(lex: &logos::Lexer<Token>) -> Option<u8> {
    let s = lex.slice();
    let inner = &s[1..s.len() - 1];
    if inner.starts_with('\\') {
        match inner.chars().nth(1)? {
            'n' => Some(b'\n'),
            'r' => Some(b'\r'),
            't' => Some(b'\t'),
            '0' => Some(0),
            '\\' => Some(b'\\'),
            '\'' => Some(b'\''),
            _ => None,
        }
    } else {
        Some(inner.bytes().next()?)
    }
}

fn parse_string(lex: &logos::Lexer<Token>) -> Option<String> {
    let s = lex.slice();
    Some(s[1..s.len() - 1].to_string())
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Var => write!(f, "var"),
            Token::Const => write!(f, "const"),
            Token::Proc => write!(f, "proc"),
            Token::Func => write!(f, "func"),
            Token::Return => write!(f, "return"),
            Token::If => write!(f, "if"),
            Token::Then => write!(f, "then"),
            Token::Elsif => write!(f, "elsif"),
            Token::Else => write!(f, "else"),
            Token::End => write!(f, "end"),
            Token::While => write!(f, "while"),
            Token::Do => write!(f, "do"),
            Token::For => write!(f, "for"),
            Token::To => write!(f, "to"),
            Token::Downto => write!(f, "downto"),
            Token::Break => write!(f, "break"),
            Token::Continue => write!(f, "continue"),
            Token::And => write!(f, "and"),
            Token::Or => write!(f, "or"),
            Token::Not => write!(f, "not"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Asm => write!(f, "asm"),
            Token::Mem => write!(f, "mem"),
            Token::Memw => write!(f, "memw"),
            Token::TypeByte => write!(f, "byte"),
            Token::TypeInt => write!(f, "int"),
            Token::TypeBool => write!(f, "bool"),
            Token::TypeChar => write!(f, "char"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::Eq => write!(f, "="),
            Token::Neq => write!(f, "<>"),
            Token::Lt => write!(f, "<"),
            Token::Gt => write!(f, ">"),
            Token::Le => write!(f, "<="),
            Token::Ge => write!(f, ">="),
            Token::Ampersand => write!(f, "&"),
            Token::Pipe => write!(f, "|"),
            Token::Caret => write!(f, "^"),
            Token::Tilde => write!(f, "~"),
            Token::Shl => write!(f, "<<"),
            Token::Shr => write!(f, ">>"),
            Token::At => write!(f, "@"),
            Token::Assign => write!(f, ":="),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::Colon => write!(f, ":"),
            Token::Semi => write!(f, ";"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::IntLit(n) => write!(f, "{}", n),
            Token::HexLit(n) => write!(f, "0x{:X}", n),
            Token::BinLit(n) => write!(f, "0b{:b}", n),
            Token::CharLit(c) => write!(f, "'{}'", *c as char),
            Token::StringLit(s) => write!(f, "\"{}\"", s),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Newline => write!(f, "\\n"),
            Token::Error => write!(f, "<error>"),
        }
    }
}

/// Tokenize source code into a vector of spanned tokens
pub fn tokenize(source: &str) -> Vec<SpannedToken> {
    let mut lexer = Token::lexer(source);
    let mut tokens = Vec::new();

    while let Some(result) = lexer.next() {
        let span = Span::new(lexer.span().start, lexer.span().end);
        let token = match result {
            Ok(tok) => tok,
            Err(_) => Token::Error,
        };
        tokens.push(SpannedToken { token, span });
    }

    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let tokens = tokenize("var x: int = 42;");
        let kinds: Vec<_> = tokens.iter().map(|t| &t.token).collect();
        assert_eq!(
            kinds,
            vec![
                &Token::Var,
                &Token::Ident("x".to_string()),
                &Token::Colon,
                &Token::TypeInt,
                &Token::Eq,
                &Token::IntLit(42),
                &Token::Semi,
            ]
        );
    }

    #[test]
    fn test_hex_literal() {
        let tokens = tokenize("0xFF");
        assert_eq!(tokens[0].token, Token::HexLit(255));
    }

    #[test]
    fn test_binary_literal() {
        let tokens = tokenize("0b1010");
        assert_eq!(tokens[0].token, Token::BinLit(10));
    }

    #[test]
    fn test_char_literal() {
        let tokens = tokenize("'A'");
        assert_eq!(tokens[0].token, Token::CharLit(65));
    }

    #[test]
    fn test_string_literal() {
        let tokens = tokenize("\"hello world\"");
        assert_eq!(tokens[0].token, Token::StringLit("hello world".to_string()));
    }

    #[test]
    fn test_operators() {
        let tokens = tokenize(":= <> <= >= << >>");
        let kinds: Vec<_> = tokens.iter().map(|t| &t.token).collect();
        assert_eq!(
            kinds,
            vec![
                &Token::Assign,
                &Token::Neq,
                &Token::Le,
                &Token::Ge,
                &Token::Shl,
                &Token::Shr,
            ]
        );
    }

    #[test]
    fn test_comment() {
        let tokens = tokenize("var x // this is a comment\nvar y");
        let kinds: Vec<_> = tokens.iter().map(|t| &t.token).collect();
        assert_eq!(
            kinds,
            vec![
                &Token::Var,
                &Token::Ident("x".to_string()),
                &Token::Newline,
                &Token::Var,
                &Token::Ident("y".to_string()),
            ]
        );
    }
}
