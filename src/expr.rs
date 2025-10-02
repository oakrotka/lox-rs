use std::fmt::Debug;

// what about the cost of so frequent allocations? would using an arena allocator be better? how?
// rust is kind of bad at this (I wonder what I'll do about memory management later…)
#[derive(Clone, PartialEq)]
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

impl Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Binary(e1, op, e2) => write!(f, "({:?} {:?} {:?})", op, e1, e2),
            Self::Grouping(e) => write!(f, "(group {:?})", e),
            Self::Literal(l) => write!(f, "{:?}", l),
            Self::Unary(op, e) => write!(f, "({:?} {:?})", op, e),
        }
    }
}

impl Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(x) => write!(f, "{}", x),
            Self::LoxString(s) => write!(f, "\"{}\"", s),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Nil => write!(f, "nil"),
        }
    }
}

impl Debug for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let repr = match self {
            Self::Equals => "==",
            Self::NotEquals => "!=",
            Self::Less => "<",
            Self::LessOrEqual => "<=",
            Self::Greater => ">",
            Self::GreaterOrEqual => ">=",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Times => "*",
            Self::Division => "/",
        };
        write!(f, "{}", repr)
    }
}

impl Debug for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let repr = match self {
            Self::Minus => "-",
            Self::Not => "!",
        };
        write!(f, "{}", repr)
    }
}

#[cfg(test)]
mod tests {
    use crate::expr::{BinaryOperator, Expr, Literal, UnaryOperator};

    // these tests aren't too broad, but they're just testing the trait for debug printing
    // expressions, so they don't have to be

    #[test]
    fn addition_repr() {
        let expr = Expr::Binary(
            Box::new(Expr::Literal(Literal::Number(1.0))),
            BinaryOperator::Plus,
            Box::new(Expr::Literal(Literal::Number(2.0))),
        );
        assert_eq!("(+ 1 2)", format!("{:?}", expr))
    }

    #[test]
    fn complex_expr_repr() {
        let expr = Expr::Binary(
            Box::new(Expr::Unary(
                UnaryOperator::Minus,
                Box::new(Expr::Literal(Literal::Number(123.0))),
            )),
            BinaryOperator::Times,
            Box::new(Expr::Grouping(Box::new(Expr::Literal(Literal::Number(
                45.67,
            ))))),
        );
        assert_eq!("(* (- 123) (group 45.67))", format!("{:?}", expr))
    }
}
