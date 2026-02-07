//! Binding-level intermediate representation for tracking variable states.

use std::{collections::HashMap, str::Split};

use crate::{ModelGraph, ir::model::ModelDef};

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
    pub prefetched_relations: HashMap<String, QuerySetState>,
    pub is_values_query: bool,
}

impl QuerySetState {
    pub fn new(model_name: String) -> Self {
        Self {
            model_name,
            prefetched_relations: HashMap::new(),
            is_values_query: false,
        }
    }

    pub fn is_access_safe(&self, relation: &str) -> bool {
        self.is_values_query || self.prefetched_relations.contains_key(relation)
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

/// Parse Django relation field syntax (e.g., "author__profile" ->
/// [PrefetchedRelation(QuerySetState)] with `profile` prefetched)
pub fn parse_relation_fields(
    model: &ModelDef,
    model_graph: &ModelGraph,
    fields: &[String],
) -> HashMap<String, QuerySetState> {
    let mut relations = HashMap::new();
    for field in fields.iter() {
        let parts = field.split("__");
        if let Some((literal, relation)) = bind_relation(model, model_graph, parts) {
            relations.insert(literal, relation);
        }
    }

    relations
}

fn bind_relation(
    model: &ModelDef,
    model_graph: &ModelGraph,
    mut parts: Split<'_, &str>,
) -> Option<(String, QuerySetState)> {
    if let Some(base) = parts.next()
        && let Some(relation) = model_graph.get_relation(&model.name, base)
        && let Some(related_model) = model_graph.get(relation)
    {
        let mut qs = QuerySetState::new(related_model.name.to_string());

        // Insert child relation
        if let Some((literal, relation)) = bind_relation(related_model, model_graph, parts) {
            qs.prefetched_relations.insert(literal, relation);
        };
        return Some((base.to_string(), qs));
    }

    None
}

#[cfg(test)]
mod tests {
    use crate::Parser;

    use super::*;

    fn get_graph() -> ModelGraph {
        let source = r#"
class User(Model):
    pass

class Photo(Model):
    user = models.ForeignKey(User, related_name="photos")


class Order(Model):
    user = models.ForeignKey(User, related_name="orders")

class Order(Model):
    user = models.ForeignKey(User, related_name="orders")

class Sale(Model):
    order = models.ForeignKey(Sale, related_name="sales")

class Transaction(Model):
    sale = models.ForeignKey(Sale, related_name="transactions")


users = User.objects.all().prefetch_related(Prefetch("orders"))
print([user.orders for user in users])
        "#;

        let parser = Parser::new();
        parser.build_graph(source, "test.py").unwrap()
    }
}
