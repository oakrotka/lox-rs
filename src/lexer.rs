#[cfg(test)]
mod tests;

use std::{fmt, str::CharIndices};

use crate::token::{Token, TokenKind};

/// Iterator over tokens in Lox code.
pub struct Lexer<'a> {
    source: &'a str,
    stream: CharIndices<'a>,
    line: u64,
    finished: bool,
}

impl<'a> Lexer<'a> {
    #[inline]
    pub fn from(source: &'a str) -> Self {
        Lexer {
            source,
            stream: source.char_indices(),
            line: 1,
            finished: false,
        }
    }

    /// Selects the current token using the character just extracted from the stream.
    fn scan_token(&mut self) -> Result<(u64, TokenKind<'a>), LexError> {
        use TokenKind::*;

        // I would rather have a tail recursive implementation than the ugly loop-based one,
        // but rust isn't able to optimize it
        'token_scanning: loop {
            let start_line = self.line;

            // try to get a character
            let (pos, c) = match self.next_char() {
                Some(t) => t,
                None => return Ok((start_line, Eof)),
            };

            // scan a token beggining in the current character
            return Ok((
                start_line,
                match c {
                    // one character tokens
                    '(' => LeftParen,
                    ')' => RightParen,
                    '{' => LeftBrace,
                    '}' => RightBrace,
                    ',' => Comma,
                    '.' => Dot,
                    '-' => Minus,
                    '+' => Plus,
                    ';' => Semicolon,
                    '*' => Star,
                    // one or two character tokens
                    '!' if self.match_next('=') => BangEqual,
                    '=' if self.match_next('=') => EqualEqual,
                    '<' if self.match_next('=') => LessEqual,
                    '>' if self.match_next('=') => GreaterEqual,
                    '!' => Bang,
                    '=' => Equal,
                    '<' => Less,
                    '>' => Greater,
                    // a slash can be a comment or a division operator
                    '/' if self.match_next('/') => match self.skip_comment() {
                        TokenScanOutcome::ScannedWhole(()) => continue 'token_scanning,
                        TokenScanOutcome::ReachedEof => Eof,
                    },
                    '/' => Slash,
                    // whitespace
                    ' ' | '\r' | '\t' => continue 'token_scanning,
                    '\n' => {
                        self.line += 1;
                        continue 'token_scanning;
                    }
                    // string
                    '"' => match self.scan_string(pos) {
                        TokenScanOutcome::ScannedWhole(s) => LoxString(s),
                        TokenScanOutcome::ReachedEof => {
                            return Err(LexError {
                                line: start_line,
                                kind: LexErrorKind::UnterminatedString,
                            });
                        }
                    },
                    // number
                    '0'..='9' => self.scan_number(pos),
                    // identifier or keyword
                    '_' | 'a'..='z' | 'A'..='Z' => self.scan_identifier(pos),
                    // unrecognized characters
                    c => {
                        return Err(LexError {
                            line: start_line,
                            kind: LexErrorKind::UnexpectedCharater(c),
                        });
                    }
                },
            ));
        }
    }

    #[inline]
    fn skip_comment(&mut self) -> TokenScanOutcome<()> {
        loop {
            match self.next_char() {
                None => return TokenScanOutcome::ReachedEof,
                Some((_, '\n')) => {
                    self.line += 1;
                    return TokenScanOutcome::ScannedWhole(());
                }
                Some(_) => (),
            }
        }
    }

    #[inline]
    fn scan_string(&mut self, start: usize) -> TokenScanOutcome<&'a str> {
        loop {
            match self.next_char() {
                None => return TokenScanOutcome::ReachedEof,
                Some((pos, '"')) => {
                    let contents = &self.source[start + 1..pos];
                    return TokenScanOutcome::ScannedWhole(contents);
                }
                Some((_, '\n')) => {
                    self.line += 1;
                }
                Some(_) => continue,
            }
        }
    }

    #[inline]
    fn scan_number(&mut self, start: usize) -> TokenKind<'a> {
        let mut scanned_dot = false;
        loop {
            let mut peek = self.stream.clone();
            match peek.next() {
                Some((_, '0'..='9')) => (),
                // scan decimal point
                Some((_, '.'))
                    if !scanned_dot && peek.next().is_some_and(|(_, c)| c.is_ascii_digit()) =>
                {
                    scanned_dot = true;
                }
                // reached the end of the number
                Some((pos, _)) => return TokenKind::Number(&self.source[start..pos]),
                None => return TokenKind::Number(&self.source[start..]),
            }
            self.next_char();
        }
    }

    #[inline]
    fn scan_identifier(&mut self, start: usize) -> TokenKind<'a> {
        let name = loop {
            match self.peek_char_pos() {
                Some((_, c)) if c.is_ascii_alphanumeric() || c == '_' => self.next_char(),
                Some((pos, _)) => break &self.source[start..pos],
                None => break &self.source[start..],
            };
        };

        use TokenKind::{
            And, Class, Else, False, For, Fun, Identifier, If, Nil, Or, Print, Return, Super, This,
            True, Var, While,
        };

        // match clause is faster than phf apparently
        match name {
            "and" => And,
            "class" => Class,
            "else" => Else,
            "false" => False,
            "for" => For,
            "fun" => Fun,
            "if" => If,
            "nil" => Nil,
            "or" => Or,
            "print" => Print,
            "return" => Return,
            "super" => Super,
            "this" => This,
            "true" => True,
            "var" => Var,
            "while" => While,
            _ => Identifier(name),
        }
    }

    #[inline]
    fn next_char(&mut self) -> Option<(usize, char)> {
        self.stream.next()
    }

    #[inline]
    fn peek_char(&mut self) -> Option<char> {
        self.peek_char_pos().map(|(_, c)| c)
    }

    #[inline]
    fn peek_char_pos(&mut self) -> Option<(usize, char)> {
        self.stream.clone().next()
    }

    #[inline]
    fn match_next(&mut self, expected: char) -> bool {
        let matched = self.peek_char() == Some(expected);
        if matched {
            self.next_char();
        }
        matched
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexError>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }

        let token = self.scan_token();
        if token.is_ok_and(|(_, kind)| kind == TokenKind::Eof) {
            self.finished = true;
        }

        Some(match token {
            Ok((line, kind)) => Ok(Token::new(kind, line)),
            Err(error) => Err(error),
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum TokenScanOutcome<T> {
    ScannedWhole(T),
    ReachedEof,
}

#[derive(Clone, Copy, PartialEq)]
pub struct LexError {
    pub line: u64,
    pub kind: LexErrorKind,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LexErrorKind {
    UnexpectedCharater(char),
    UnterminatedString,
}

impl fmt::Debug for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let message = match self.kind {
            LexErrorKind::UnexpectedCharater(c) if c.is_control() => {
                "Unexpected control character".to_string()
            }
            LexErrorKind::UnexpectedCharater(c) => format!("Unexpected character {c}."),
            LexErrorKind::UnterminatedString => "Unterminated string.".to_string(),
        };

        write!(f, "[line {}] Error: {}", self.line, message)
    }
}
