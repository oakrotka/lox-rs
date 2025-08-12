use std::{fmt, str::Chars};

use crate::token::{Token, TokenKind};

/// Iterator over tokens in Lox code.
#[derive(Debug)]
pub struct Lexer<'a> {
    stream: DoublyPeekable<Chars<'a>>,
    line: u64,
    finished: bool,
}

type TokenMatchingResult<'a> = Result<TokenKind<'a>, LexError>;

impl<'a> Lexer<'a> {
    pub fn from(source: &'a str) -> Self {
        Lexer {
            stream: DoublyPeekable::from(source.chars()),
            line: 1,
            finished: false,
        }
    }

    /// Selects the current token using the character just extracted from the stream.
    fn scan_token(&mut self) -> TokenMatchingResult<'a> {
        use TokenKind::*;

        // I would rather have a tail recursive implementation than the ugly loop-based one,
        // but rust isn't able to optimize it
        'token_scanning: loop {
            // try to get a character
            let c = match self.next_char() {
                Some(c) => c,
                None => return Ok(Eof),
            };

            // scan a token beggining in the current character
            return Ok(match c {
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
                '/' if self.match_next('/') => 'comment: loop {
                    match self.peek_char() {
                        None => break 'comment Eof,
                        Some('\n') => continue 'token_scanning,
                        Some(_) => {
                            self.next_char();
                        }
                    }
                },
                '/' => Slash,
                // whitespace
                ' ' | '\r' | '\t' => continue 'token_scanning,
                '\n' => {
                    self.line += 1;
                    continue 'token_scanning;
                }
                // unrecognized characters
                c => {
                    return Err(LexError {
                        line: self.line,
                        kind: LexErrorKind::UnexpectedCharater(c),
                    });
                }
            });
        }
    }

    /// Eats the characters consisting of the next token and returns the token type
    #[inline]
    fn next_char(&mut self) -> Option<char> {
        self.stream.next()
    }

    #[inline]
    fn peek_char(&mut self) -> Option<&char> {
        self.stream.peek()
    }

    #[inline]
    fn peek_second_char(&mut self) -> Option<&char> {
        self.stream.peek_second()
    }

    #[inline]
    fn match_next(&mut self, expected: char) -> bool {
        let matched = self.stream.peek().is_some_and(|c| *c == expected);
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
        if token.is_ok_and(|t| t == TokenKind::Eof) {
            self.finished = true;
        }

        Some(match token {
            Ok(variant) => Ok(Token::new(variant, self.line)),
            Err(error) => Err(error),
        })
    }
}

#[derive(Clone, Copy, PartialEq)]
pub struct LexError {
    pub line: u64,
    pub kind: LexErrorKind,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LexErrorKind {
    UnexpectedCharater(char),
}

impl fmt::Debug for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let message = match self.kind {
            LexErrorKind::UnexpectedCharater(c) if c.is_control() => {
                "Unexpected control character".to_string()
            }
            LexErrorKind::UnexpectedCharater(c) => format!("Unexpected character {c}."),
        };

        write!(f, "[line {}] Error: {}", self.line, message)
    }
}

/// Iterator capable of peeking 2 items ahead.
/// Implementation based on the `Peekable` struct from rust's std.
#[derive(Clone, Debug)]
struct DoublyPeekable<I: Iterator> {
    iter: I,
    // remembered values
    peeked: Option<(Option<I::Item>, Option<Option<I::Item>>)>,
}

impl<I: Iterator> Iterator for DoublyPeekable<I> {
    type Item = I::Item;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self.peeked.take() {
            None => self.iter.next(),
            Some((val, None)) => val,
            Some((val, Some(next))) => {
                // move the queue by one
                self.peeked = Some((next, None));
                val
            }
        }
    }
}

impl<I: Iterator> DoublyPeekable<I> {
    fn from(iter: I) -> Self {
        DoublyPeekable { iter, peeked: None }
    }

    fn peek<'a>(&'a mut self) -> Option<&'a I::Item> {
        self.peeked
            .get_or_insert_with(|| (self.iter.next(), None))
            .0
            .as_ref()
    }

    fn peek_second<'a>(&'a mut self) -> Option<&'a I::Item> {
        self.peeked
            .get_or_insert_with(|| (self.iter.next(), None))
            .1
            .get_or_insert_with(|| self.iter.next())
            .as_ref()
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, LexError, LexErrorKind};

    use crate::{lexer::DoublyPeekable, token::{Token, TokenKind}};
    use TokenKind::*;

    #[test]
    fn lexer_constructor() {
        let lexer = Lexer::from("// this is code");
        assert_eq!(lexer.line, 1);
        assert!(!lexer.finished);
    }

    #[test]
    fn lexer_token_scan() {
        let mut lexer = Lexer::from("+\n");
        assert_eq!(lexer.line, 1);

        assert_eq!(lexer.scan_token(), Ok(Plus));
        assert_eq!(lexer.line, 1);

        assert_eq!(lexer.scan_token(), Ok(Eof));
        assert_eq!(lexer.line, 2);
    }

    #[test]
    fn lexer_next_char() {
        let mut lexer = Lexer::from("+\n");
        assert_eq!(lexer.next_char(), Some('+'));
        assert_eq!(lexer.next_char(), Some('\n'));
        assert_eq!(lexer.next_char(), None);
    }

    #[test]
    fn lexer_match_next() {
        // succesful matching should eat the token
        let mut lexer = Lexer::from("+\n");
        assert!(lexer.match_next('+'));
        assert_eq!(lexer.scan_token(), Ok(Eof));

        // unsuccesful matching should not eat the token
        let mut lexer = Lexer::from("+\n");
        assert!(!lexer.match_next('-'));
        assert_eq!(lexer.scan_token(), Ok(Plus));
    }

    #[test]
    fn lexer_peeking() {
        let mut lexer = Lexer::from("+\n");
        assert_eq!(lexer.peek_char(), Some(&'+'));
        assert_eq!(lexer.peek_second_char(), Some(&'\n'));

        assert_eq!(lexer.next_char(), Some('+'));
        assert_eq!(lexer.peek_char(), Some(&'\n'));
        assert_eq!(lexer.peek_second_char(), None);

        assert_eq!(lexer.next_char(), Some('\n'));
        assert_eq!(lexer.peek_char(), None);

        assert_eq!(lexer.next_char(), None);
    }

    #[test]
    fn lexer_iteration() {
        let mut lexer = Lexer::from("+\n");
        assert_eq!(lexer.next(), Some(Ok(Token::new(Plus, 1))));
        assert_eq!(lexer.next(), Some(Ok(Token::new(Eof, 2))));
        assert_eq!(lexer.next(), None);
    }

    fn tokens_from_code(code: &str) -> Vec<Token<'_>> {
        Lexer::from(code)
            .collect::<Result<Vec<_>, _>>()
            .expect("expected succesful scanning result")
    }

    fn assert_scanning_matches(code: &str, expected: Vec<(TokenKind, u64)>) {
        let result = tokens_from_code(code);
        let exp_tokens: Vec<_> = expected
            .into_iter()
            .map(|(kind, line)| Token::new(kind, line))
            .collect();

        assert_eq!(result, exp_tokens)
    }

    #[test]
    fn scan_no_code() {
        assert_scanning_matches("", vec![(Eof, 1)]);
    }

    #[test]
    fn scan_whitespace() {
        assert_scanning_matches(" \r\t\n", vec![(Eof, 2)]);
    }

    #[test]
    fn scan_one_comment() {
        assert_scanning_matches("//this is a comment", vec![(Eof, 1)]);
    }

    #[test]
    fn scan_multiple_comments() {
        let code = "
            // this is a comment
            + // this is a character that the scanner should be able to see
            // this is a comment ended by EOF";
        assert_scanning_matches(code, vec![(Plus, 3), (Eof, 4)]);
    }

    #[test]
    fn scan_grouping() {
        assert_scanning_matches(
            "(( )) {} {",
            vec![
                (LeftParen, 1),
                (LeftParen, 1),
                (RightParen, 1),
                (RightParen, 1),
                (LeftBrace, 1),
                (RightBrace, 1),
                (LeftBrace, 1),
                (Eof, 1),
            ],
        );
    }

    #[test]
    fn scan_one_character_operators() {
        assert_scanning_matches(
            "!*+-/=<>",
            vec![
                (Bang, 1),
                (Star, 1),
                (Plus, 1),
                (Minus, 1),
                (Slash, 1),
                (Equal, 1),
                (Less, 1),
                (Greater, 1),
                (Eof, 1),
            ],
        )
    }

    #[test]
    fn scan_two_character_operators() {
        assert_scanning_matches(
            "!= == <= >=",
            vec![
                (BangEqual, 1),
                (EqualEqual, 1),
                (LessEqual, 1),
                (GreaterEqual, 1),
                (Eof, 1),
            ],
        )
    }

    #[test]
    fn scan_multiline() {
        let code = "*
            +

            -
        ";
        assert_scanning_matches(code, vec![(Star, 1), (Plus, 2), (Minus, 4), (Eof, 5)])
    }

    #[test]
    fn scan_fail_unexpected_character() {
        let code = "() ą + \n|";
        let expected = vec![
            Ok(Token::new(LeftParen, 1)),
            Ok(Token::new(RightParen, 1)),
            Err(LexError { line: 1, kind: LexErrorKind::UnexpectedCharater('ą') }),
            Ok(Token::new(Plus, 1)),
            Err(LexError { line: 2, kind: LexErrorKind::UnexpectedCharater('|') }),
            Ok(Token::new(Eof, 2)),
        ];

        let tokens: Vec<_> = Lexer::from(code).collect();
        assert_eq!(tokens, expected);
    }

    // I used to have a beautiful tail recursive way of handling whitespace,
    // but sadly the compiler didn't know how to optimize it.
    // this function tests for a stack overflow on large whitespace inputs
    #[test]
    #[ignore = "will crash all testing if failing"]
    fn whitespace_tail_recursion() {
        assert_scanning_matches(" ".repeat(100_000).as_ref(), vec![(Eof, 1)]);
    }

    #[test]
    fn peekable_no_peeking() {
        let mut iter = DoublyPeekable::from(1..=3);
        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.next(), Some(2));
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn peekable_peek_first() {
        let mut iter = DoublyPeekable::from(1..=2);
        assert_eq!(iter.peek(), Some(&1));
        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.next(), Some(2));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn peekable_peek_second() {
        let mut iter = DoublyPeekable::from(1..=3);

        assert_eq!(iter.peek_second(), Some(&2));
        assert_eq!(iter.next(), Some(1));

        assert_eq!(iter.peek(), Some(&2));
        assert_eq!(iter.next(), Some(2));
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn peekable_peek_both() {
        let mut iter = DoublyPeekable::from(1..=3);

        assert_eq!(iter.peek(), Some(&1));
        assert_eq!(iter.peek_second(), Some(&2));

        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.peek(), Some(&2));

        assert_eq!(iter.next(), Some(2));
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), None);
    }
}
