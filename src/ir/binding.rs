//! Binding-level intermediate representation for tracking variable states.

#[derive(Debug, Clone)]
pub struct QuerySetContext {
    pub model: Option<String>,
    pub state: QuerySetState,
}

#[derive(Debug, Clone)]
pub struct SafeMethod {
    pub name: String,
    pub prefetched_relations: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum QuerySetState {
    /// Queryset has select_related/prefetch_related with tracked fields
    Safe(SafeMethod),
    /// Queryset has no prefetching - any relation access is potentially N+1
    Unsafe,
}

#[derive(Debug, Clone)]
pub enum BindingKind {
    QuerySet(QuerySetContext),
    Unknown,
}

/// Parse Django relation field syntax (e.g., "author__profile" -> ["author", "profile"])
pub fn parse_relation_fields(fields: &[String]) -> Vec<String> {
    fields
        .iter()
        .flat_map(|field| field.split("__"))
        .map(|s| s.to_string())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_single_field() {
        let fields = vec!["ticker".to_string()];
        assert_eq!(parse_relation_fields(&fields), vec!["ticker"]);
    }

    #[test]
    fn parse_nested_field() {
        let fields = vec!["theoanalysis__ticker".to_string()];
        assert_eq!(
            parse_relation_fields(&fields),
            vec!["theoanalysis", "ticker"]
        );
    }

    #[test]
    fn parse_deeply_nested() {
        let fields = vec!["a__b__c__d".to_string()];
        assert_eq!(parse_relation_fields(&fields), vec!["a", "b", "c", "d"]);
    }

    #[test]
    fn parse_multiple_fields() {
        let fields = vec!["ticker__sector".to_string(), "analysis__report".to_string()];
        assert_eq!(
            parse_relation_fields(&fields),
            vec!["ticker", "sector", "analysis", "report"]
        );
    }
}
