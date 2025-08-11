pub(crate) mod token;

mod lexer;

/// Indicates that the input is not valid, runnable Lox code.
#[derive(Clone, Copy)]
pub struct LoxEvaluationError;

/// Run the code provided as the argument.
pub fn run_code(code: &str) -> Result<(), LoxEvaluationError> {
    let tokens = lexer::scan_tokens(code).map_err(|_| LoxEvaluationError)?;
    println!("{tokens:?}"); // just print tokens for now
    Ok(())
}

/// Inform the user about an error in the code.
pub(crate) fn error(line: u32, message: &str) {
    eprintln!("[line {line}] Error: {message}");
}
