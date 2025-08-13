use std::{fmt, str::Chars};

use crate::token::{Token, TokenKind};

/// Iterator over tokens in Lox code.
pub struct Lexer<'a> {
    stream: Chars<'a>,
    line: u64,
    finished: bool,
}

type TokenMatchingResult<'a> = Result<TokenKind<'a>, LexError>;

impl<'a> Lexer<'a> {
    pub fn from(source: &'a str) -> Self {
        Lexer {
            stream: source.chars(),
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
                        Some('\n') => continue 'token_scanning,
                        None => {
                            self.next_char();
                            break 'comment Eof;
                        }
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
    fn peek_char(&mut self) -> Option<char> {
        self.stream.clone().next()
    }

    #[inline]
    fn peek_second_char(&mut self) -> Option<char> {
        let mut iter = self.stream.clone();
        iter.next();
        iter.next()
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
        if token == Ok(TokenKind::Eof) {
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
        assert_eq!(lexer.peek_char(), Some('+'));
        assert_eq!(lexer.peek_second_char(), Some('\n'));

        assert_eq!(lexer.next_char(), Some('+'));
        assert_eq!(lexer.peek_char(), Some('\n'));
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
            "// this is a comment
            + // this is a character that the scanner should be able to see
            // this is a comment ended by EOF"
        };
        assert_scanning_matches(code, vec![(Plus, 2), (Eof, 3)]);
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
        let expected = vec![(String(s), 1), (Eof, 1)];
        assert_scanning_matches(&code, expected);
    }

    #[test]
    fn scan_number() {
        assert_scanning_matches(
            "2137 4.25",
            vec![
                (Number(2137.0), 1),
                // woah, scary floating point number comparison!
                // don't worry, I picked numbers that are representable by the `f64` type :3
                (Number(4.25), 1),
                (Eof, 1),
            ],
        );
    }

    #[test]
    fn scan_fake_decimal() {
        let code = {
            ".2137
            2137."
        };

        let expected = vec![(Dot, 1), (Number(2137.0), 1), (Number(2137.0), 2), (Dot, 2)];

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
            vec![
                (Number(1.0), 1),
                (Identifier("ndentifi3r5"), 1),
                (Eof, 1)
            ]
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
        let code = "> \" this is an unterminated string !";
        let expected = vec![
            Ok(Token::new(Greater, 1)),
            Err(LexError {
                line: 1,
                kind: LexErrorKind::UnterminatedString,
            }),
            Ok(Token::new(Eof, 1)),
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
