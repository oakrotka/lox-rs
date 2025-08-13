use std::{fmt, str::CharIndices};

use crate::token::{self, Token, TokenKind};

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

        *token::KEYWORDS.get(name).unwrap_or(&TokenKind::Identifier(name))
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

#[cfg(test)]
mod tests {
    use super::{LexError, LexErrorKind, Lexer};

    use crate::token::{Token, TokenKind};
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
        assert_eq!(lexer.scan_token(), Ok((1, Plus)));
        assert_eq!(lexer.scan_token(), Ok((2, Eof)));
    }

    #[test]
    fn lexer_next_char() {
        let mut lexer = Lexer::from("+\n");
        assert_eq!(lexer.next_char(), Some((0, '+')));
        assert_eq!(lexer.next_char(), Some((1, '\n')));
        assert_eq!(lexer.next_char(), None);
    }

    #[test]
    fn lexer_match_next() {
        // succesful matching should eat the token
        let mut lexer = Lexer::from("+\n");
        assert!(lexer.match_next('+'));
        assert_eq!(lexer.scan_token(), Ok((2, Eof)));

        // unsuccesful matching should not eat the token
        let mut lexer = Lexer::from("+\n");
        assert!(!lexer.match_next('-'));
        assert_eq!(lexer.scan_token(), Ok((1, Plus)));
    }

    #[test]
    fn lexer_peeking() {
        let mut lexer = Lexer::from("+\n");

        assert_eq!(lexer.peek_char(), Some('+'));
        assert_eq!(lexer.next_char(), Some((0, '+')));

        assert_eq!(lexer.peek_char(), Some('\n'));
        assert_eq!(lexer.next_char(), Some((1, '\n')));

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

        assert_eq!(result, exp_tokens);
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
        let code = {
            "
            // this is a comment
            + // this is a character that the scanner should be able to see
            // this is a comment ended by EOF"
        };
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
        );
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
        );
    }

    #[test]
    fn scan_multiline() {
        let code = "*
            +

            -
        ";
        assert_scanning_matches(code, vec![(Star, 1), (Plus, 2), (Minus, 4), (Eof, 5)]);
    }

    #[test]
    fn scan_string() {
        let s = "this is a string";
        let code = format!("\"{s}\"");
        let expected = vec![(LoxString(s), 1), (Eof, 1)];
        assert_scanning_matches(&code, expected);
    }

    #[test]
    fn scan_string_empty() {
        assert_scanning_matches(r#" "" "#, vec![(LoxString(""), 1), (Eof, 1)]);
    }

    #[test]
    fn scan_string_multiline() {
        let s = "first line \n second line \n third line";
        assert_scanning_matches(&format!("\"{s}\""), vec![(LoxString(s), 1), (Eof, 3)]);
    }

    #[test]
    fn scan_number() {
        assert_scanning_matches(
            "2137 4.25",
            vec![(Number("2137"), 1), (Number("4.25"), 1), (Eof, 1)],
        );
    }

    #[test]
    fn scan_number_fake_decimal() {
        let code = {
            ".2137
            123.456.789
            2137."
        };

        let expected = vec![
            (Dot, 1),
            (Number("2137"), 1),
            (Number("123.456"), 2),
            (Dot, 2),
            (Number("789"), 2),
            (Number("2137"), 3),
            (Dot, 3),
            (Eof, 3),
        ];

        assert_scanning_matches(code, expected);
    }

    #[test]
    fn scan_keywords() {
        assert_scanning_matches(
            "and class else false for fun if nil or print return super this true var while",
            vec![
                (And, 1),
                (Class, 1),
                (Else, 1),
                (False, 1),
                (For, 1),
                (Fun, 1),
                (If, 1),
                (Nil, 1),
                (Or, 1),
                (Print, 1),
                (Return, 1),
                (Super, 1),
                (This, 1),
                (True, 1),
                (Var, 1),
                (While, 1),
                (Eof, 1),
            ],
        );
    }

    #[test]
    fn scan_identifiers() {
        let tested_words = vec!["x", "orchid", "FORTIFIED", "vAr13d__", "_", "_meow"];
        let code = tested_words.join(" ");
        let expected = tested_words
            .iter()
            .map(|name| (Identifier(name), 1))
            .chain([(Eof, 1)].into_iter())
            .collect::<Vec<_>>();
        assert_scanning_matches(&code, expected);
    }

    #[test]
    fn scan_not_identifiers_prefixed_numeric() {
        assert_scanning_matches(
            "1ndentifi3r5",
            vec![(Number("1"), 1), (Identifier("ndentifi3r5"), 1), (Eof, 1)],
        );
    }

    #[test]
    fn scan_fail_unexpected_character() {
        let code = "() ą + \n|";
        let expected = vec![
            Ok(Token::new(LeftParen, 1)),
            Ok(Token::new(RightParen, 1)),
            Err(LexError {
                line: 1,
                kind: LexErrorKind::UnexpectedCharater('ą'),
            }),
            Ok(Token::new(Plus, 1)),
            Err(LexError {
                line: 2,
                kind: LexErrorKind::UnexpectedCharater('|'),
            }),
            Ok(Token::new(Eof, 2)),
        ];

        let tokens: Vec<_> = Lexer::from(code).collect();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn scan_fail_unterminated_string() {
        let code = "> \" this is \n an unterminated \n string !";
        let expected = vec![
            Ok(Token::new(Greater, 1)),
            Err(LexError {
                line: 1,
                kind: LexErrorKind::UnterminatedString,
            }),
            Ok(Token::new(Eof, 3)),
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
}
