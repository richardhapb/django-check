//! Binding-level intermediate representation for tracking variable states.

use std::{collections::HashSet, hash::Hash};

use ruff_python_ast::{Expr, ExprCall};

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
pub struct DjangoSymbolId(u32);

impl DjangoSymbolId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct QuerySetState {
    /// The name of the model
    pub model_name: String,
    /// Relations that are prefetched with `select_related` or `prefetch_related`
    prefetched_relations: HashSet<String>,
    /// If it is a direct access to values with `values` or `values_list`
    is_values_query: bool,
}

impl QuerySetState {
    pub fn new(model_name: String) -> Self {
        Self {
            model_name,
            prefetched_relations: HashSet::new(),
            is_values_query: false,
        }
    }

    pub fn apply_call(&mut self, call: &ExprCall) {
        let Expr::Attribute(ref attr) = *call.func else {
            return;
        };

        const SAFE_QS_METHODS: [&str; 2] = ["select_related", "prefetch_related"];
        const SAFE_NO_QS_METHODS: [&str; 2] = ["values", "values_list"];

        let name = attr.attr.id.as_str();

        if SAFE_NO_QS_METHODS.contains(&name) {
            self.is_values_query = true;
        }

        if SAFE_QS_METHODS.contains(&name) {
            let fields: Vec<String> = call
                .arguments
                .args
                .iter()
                .filter_map(|a| a.as_string_literal_expr())
                .map(|s| s.value.to_string())
                .collect();

            let prefetched_relations = parse_relation_fields(&fields);

            self.prefetched_relations.extend(prefetched_relations);
        }
    }

    pub fn is_access_safe(&self, relation: &str) -> bool {
        self.is_values_query || self.prefetched_relations.contains(relation)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DjangoSymbol {
    /// Id that identifies the QuerySet
    id: DjangoSymbolId,
    /// Kind of django symbol
    pub kind: DjangoSymbolKind,
}

impl Hash for DjangoSymbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl DjangoSymbol {
    pub fn new(id: DjangoSymbolId, kind: DjangoSymbolKind) -> Self {
        Self { id, kind }
    }

    pub fn id(&self) -> &DjangoSymbolId {
        &self.id
    }
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
