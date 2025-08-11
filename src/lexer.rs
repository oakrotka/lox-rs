use crate::token::Token;

pub(super) fn scan_tokens(code: &str) -> Result<Vec<Token>, ScanningFailure> {
    todo!()
}

#[derive(Clone, Copy)]
pub(super) struct ScanningFailure;
