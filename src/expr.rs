// what about the cost of so frequent allocations? would using an arena allocator be better? how?
// rust is kind of bad at this (I wonder what I'll do about memory management later…)
pub enum Expr {
    Binary(Box<Expr>, BinaryOperator, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(Literal),
    Unary(UnaryOperator, Box<Expr>),
}

#[derive(Clone, PartialEq)]
pub enum Literal {
    Number(f64),
    LoxString(String),
    True,
    False,
    Nil,
}

// I chose to make use of rust's type system and make separate enums for operators instead of just
// using the lexer's tokens like in Crafting Interpreters - not only does this give me type safety,
// but it also allows me to omit lifetimes imposed by the TokenKind type
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Equals,
    NotEquals,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Plus,
    Minus,
    Times,
    Division,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Minus,
    Not,
}
