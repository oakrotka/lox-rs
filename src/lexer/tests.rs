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
