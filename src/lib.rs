// TODO reconsider module visibility
mod lexer;
mod token;

/// Indicates that the input is not valid, runnable Lox code.
#[derive(Clone, Copy)]
pub struct LoxEvaluationError;

/// Run the code provided as the argument.
pub fn run_code(code: &str) -> Result<(), LoxEvaluationError> {
    let mut tokens = lexer::Lexer::from(code);
    // just print tokens for now
    while let Some(token) = tokens.next() {
        print!("{:?} ", token);
    }
    Ok(())
}
