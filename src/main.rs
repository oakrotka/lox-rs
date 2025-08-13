use std::{fmt, fs, io::Write, path::Path, process::ExitCode};

use lox_rs::run_code;

// the return value of the program
#[derive(Clone, Copy, PartialEq, Eq)]
enum LoxExitStatus {
    Success = 0,
    InvalidArgs = 64,
    DataError = 65,
    FileError = 66,
}

impl std::process::Termination for LoxExitStatus {
    fn report(self) -> ExitCode {
        ExitCode::from(self as u8)
    }
}

// an enum for `run_file` errors, to be mapped to `LoxExitStatus` values
#[derive(Clone, Copy, PartialEq, Eq)]
enum FileRunningError {
    ParseError,
    UnreachableFile,
}

impl From<FileRunningError> for LoxExitStatus {
    fn from(value: FileRunningError) -> Self {
        match value {
            FileRunningError::ParseError => Self::DataError,
            FileRunningError::UnreachableFile => Self::FileError,
        }
    }
}

fn run_file(filename: impl AsRef<Path> + fmt::Debug) -> Result<(), FileRunningError> {
    let code = fs::read_to_string(&filename).map_err(|_| {
        eprintln!("Error: Could not read file {filename:?}");
        FileRunningError::UnreachableFile
    })?;
    run_code(&code).map_err(|_| FileRunningError::ParseError)?;
    Ok(())
}

fn run_repl() {
    let stdin = std::io::stdin();
    let mut stdout = std::io::stdout();

    let mut input = String::new();

    loop {
        print!("> ");
        stdout.flush().expect("failure writing to stdout");
        stdin.read_line(&mut input).expect("failure reading stdin");
        if !input.ends_with('\n') {
            break;
        }
        let _ = run_code(&input);
        input.clear();
    }
}

fn main() -> LoxExitStatus {
    let mut args = std::env::args();
    if args.len() > 2 {
        println!("Usage: lox-rs [script]");
        return LoxExitStatus::InvalidArgs;
    }

    match &args.nth(1) {
        // if a file was provided, try to run it as a script
        Some(filename) => {
            if let Err(error) = run_file(filename) {
                return error.into();
            }
        }
        // if a file was not provided, run the REPL
        None => run_repl(),
    };

    LoxExitStatus::Success
}
