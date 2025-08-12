#[derive(Clone, Copy, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub line: u64,
}

impl<'a> Token<'a> {
    pub fn new(variant: TokenKind<'a>, line: u64) -> Self {
        Token {
            kind: variant,
            line,
        }
    }
}

impl std::fmt::Debug for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{:?}", self.line, self.kind)
    }
}

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind<'a> {
    // single-character tokens
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    // one or two character tokens
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    // keywords
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,

    // literals
    Identifier(&'a str),
    String(&'a str),
    Number(f64),

    Eof
}
