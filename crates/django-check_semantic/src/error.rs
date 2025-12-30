use std::{error::Error, fmt::Display};

#[derive(Debug)]
pub struct SourceParseError {
    error: Box<dyn Error>,
    reference: String,
}

impl SourceParseError {
    pub fn new(error: Box<dyn Error>, reference: impl Into<String>) -> Self {
        Self {
            error,
            reference: reference.into(),
        }
    }
}

impl Error for SourceParseError {}

impl From<std::io::Error> for SourceParseError {
    fn from(value: std::io::Error) -> Self {
        Self {
            reference: value.to_string(),
            error: Box::new(value),
        }
    }
}

impl Display for SourceParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = format!(
            "{filename}: {error}",
            error = self.error,
            filename = self.reference
        );
        write!(f, "{text}")
    }
}
