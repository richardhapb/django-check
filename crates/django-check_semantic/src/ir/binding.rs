//! Binding-level intermediate representation for tracking variable states.

use std::collections::HashSet;

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
pub struct DjangoSymbolId(pub u32);

impl DjangoSymbolId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct QuerySetState {
    pub model_name: String,
    pub prefetched_relations: HashSet<String>,
    pub is_values_query: bool,
}

impl QuerySetState {
    pub fn new(model_name: String) -> Self {
        Self {
            model_name,
            prefetched_relations: HashSet::new(),
            is_values_query: false,
        }
    }

    pub fn is_access_safe(&self, relation: &str) -> bool {
        self.is_values_query || self.prefetched_relations.contains(relation)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum DjangoSymbol {
    /// A `QuerySet` instance (e.g., `User.objects.all()`)
    QuerySet(QuerySetState),
    /// A Model instance (e.g., `user` inside a loop)
    ModelInstance(QuerySetState),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum DjangoSymbolKind {
    /// A `QuerySet` instance
    QuerySet(QuerySetState),
    /// A Model instance
    ModelInstance(String),
    /// Unknown symbol
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
