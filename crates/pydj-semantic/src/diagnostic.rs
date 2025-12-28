use lsp_types::{
    CodeDescription, Diagnostic, DiagnosticSeverity, NumberOrString, Position, Range, Url,
};

#[derive(Debug)]
pub struct NPlusOneDiagnostic {
    pub filename: String,
    pub line: usize,
    pub col: usize,
    pub code: &'static str,
    pub access: String,
    pub message: String,
}

impl NPlusOneDiagnostic {
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

impl From<NPlusOneDiagnostic> for Diagnostic {
    fn from(value: NPlusOneDiagnostic) -> Self {
        Diagnostic {
            range: Range::new(
                Position::new(value.line.saturating_sub(1) as u32, value.col.saturating_sub(1) as u32),
                Position::new(value.line.saturating_sub(1) as u32, (value.col.saturating_sub(1) + value.access.len()) as u32),
            ),
            severity: Some(DiagnosticSeverity::WARNING),
            code: Some(NumberOrString::String(value.code.to_string())),
            code_description: Some(CodeDescription {
                href: Url::parse(
                    "http://docs.djangoproject.com/en/6.0/topics/db/optimization/#use-queryset-select-related-and-prefetch-related",
                ).expect("valid str"),
            }),
            source: Some("pydjavu".into()),
            message: value.message,
            related_information: None,
            tags: None,
            data: None,
        }
    }
}

impl std::fmt::Display for NPlusOneDiagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}: [{}] {} \n{}",
            self.filename, self.line, self.col, self.code, self.access, self.message
        )
    }
}
