use crate::token::Token;

// what about the cost of so frequent allocations? would using an arena allocator be better? how?
// rust is kind of bad at this (I wonder what I'll do about memory management later…)
type InnerExpr<'a> = Box<Expr<'a>>;

pub enum Expr<'a> {
    Binary(InnerExpr<'a>, Token<'a>, InnerExpr<'a>),
    Grouping(InnerExpr<'a>),
    Literal(Literal),
    Unary(Token<'a>, InnerExpr<'a>),
}

pub enum Literal {
    Number(f64),
    LoxString(String),
    True,
    False,
    Nil,
}
