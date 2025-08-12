// TODO reconsider module visibility
mod lexer;
mod token;

/// Indicates that the input is not valid, runnable Lox code.
#[derive(Clone, Copy)]
pub struct LoxEvaluationError;

/// Run the code provided as the argument.
pub fn run_code(code: &str) -> Result<(), LoxEvaluationError> {
    let tokens = lexer::Lexer::from(code).collect::<Vec<_>>();
    println!("{tokens:?}"); // just print tokens for now
    Ok(())
}
