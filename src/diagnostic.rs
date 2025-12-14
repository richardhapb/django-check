#[derive(Debug)]
pub struct Diagnostic {
    pub filename: String,
    pub line: usize,
    pub col: usize,
    pub code: &'static str,
    pub access: String,
    pub message: String,
}

impl Diagnostic {
    pub fn new(
        filename: impl Into<String>,
        line: usize,
        col: usize,
        code: &'static str,
        access: impl Into<String>,
        message: impl Into<String>,
    ) -> Self {
        Self {
            filename: filename.into(),
            line,
            col,
            code,
            access: access.into(),
            message: message.into(),
        }
    }
}

impl std::fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}: [{}] {} \n{}",
            self.filename, self.line, self.col, self.code, self.access, self.message
        )
    }
}
