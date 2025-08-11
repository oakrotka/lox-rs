#[derive(Clone, Copy)]
pub struct Token<'a> {
    variant: TokenVariant<'a>,
    // what do I actually need this for? consider deleting in the future
    lexeme: &'a str,
    line: u64,
}

impl std::fmt::Debug for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let literal = match self.variant {
            TokenVariant::Identifier(name) => name.to_string(),
            TokenVariant::String(s) => format!("\"{s}\""),
            TokenVariant::Number(x) => format!("{x}"),
            _ => "none".to_string()
        };
        write!(f, "({:?} {} {})", self.variant, self.lexeme, literal)
    }
}

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenVariant<'a> {
    // single-character tokens
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    // nne or two character tokens
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
}
