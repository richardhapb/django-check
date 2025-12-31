//! Model-level intermediate representation for Django model dependency graphs.

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RelationType {
    ForeignKey,
    OneToOne,
    ManyToMany,
    GenericForeignKey,
}

#[derive(Debug, Clone)]
pub struct Relation {
    pub field_name: String,
    pub target_model: String,
    pub relation_type: RelationType,
    related_name: String,
}

impl Relation {
    pub fn new(
        model_name: &str,
        field_name: String,
        target_model: String,
        relation_type: RelationType,
        related_name: Option<String>,
    ) -> Self {
        Self {
            field_name,
            target_model,
            related_name: Self::resolve_related_name(related_name, &relation_type, model_name),
            relation_type,
        }
    }

    pub fn related_name(&self) -> &str {
        &self.related_name
    }

    fn resolve_related_name(
        related_name: Option<String>,
        relation_type: &RelationType,
        model_name: &str,
    ) -> String {
        let model_name = model_name.to_lowercase();
        match related_name.as_ref() {
            // django uses %(class)s to replace dynamically the class name
            Some(related_name) => related_name.replace("%(class)s", &model_name),
            None => {
                match relation_type {
                    // ManyTo relation by default uses model_name in lowercase with `_set` as a
                    // prefix
                    RelationType::ManyToMany
                    | RelationType::ForeignKey
                    | RelationType::GenericForeignKey => format!("{}_set", model_name.to_string()),
                    // One to one use the model name directly
                    RelationType::OneToOne => model_name.to_string(),
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ModelDef {
    pub name: String,
    pub file_path: String,
    pub line: usize,
    pub relations: Vec<Relation>,
    is_abstract: bool,
}

impl ModelDef {
    pub fn new(name: impl Into<String>, file_path: impl Into<String>, line: usize) -> Self {
        Self {
            name: name.into(),
            file_path: file_path.into(),
            line,
            relations: Vec::new(),
            is_abstract: false,
        }
    }

    pub fn is_abstract(&self) -> bool {
        self.is_abstract
    }

    pub fn mark_as_abstract(&mut self) {
        self.is_abstract = true
    }

    pub fn add_relation(&mut self, relation: Relation) {
        self.relations.push(relation);
    }

    /// Get all models this model directly depends on
    pub fn dependencies(&self) -> impl Iterator<Item = &str> {
        self.relations.iter().map(|r| r.target_model.as_str())
    }
}

/// Dependency graph of Django models
#[derive(Debug, Default, Clone)]
pub struct ModelGraph {
    models: HashMap<String, ModelDef>,
}

impl ModelGraph {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_model(&mut self, model: ModelDef) {
        self.models.insert(model.name.clone(), model);
    }

    pub fn get(&self, name: &str) -> Option<&ModelDef> {
        self.models.get(name)
    }

    pub fn models(&self) -> impl Iterator<Item = &ModelDef> {
        self.models.values()
    }

    pub fn model_count(&self) -> usize {
        self.models.len()
    }

    pub fn is_relation(&self, model_name: &str, related_name: &str) -> bool {
        self.get_model_relations(model_name).contains(&related_name)
    }

    pub fn get_model_relations(&self, model_name: &str) -> Vec<&str> {
        let mut result = Vec::new();

        // Forward: fields on this model pointing to others
        if let Some(model) = self.models.get(model_name) {
            for r in &model.relations {
                result.push(r.field_name.as_str());
            }
        }

        // Reverse: related_names from models pointing to this one
        for m in self.dependents(model_name) {
            for r in &m.relations {
                if r.target_model == model_name {
                    result.push(r.related_name.as_str());
                }
            }
        }

        result
    }

    pub fn get_relation(&self, model_name: &str, related_name: &str) -> Option<&str> {
        // Forward: fields on this model pointing to others
        if let Some(model) = self.models.get(model_name) {
            for r in &model.relations {
                if r.field_name == related_name {
                    return Some(r.target_model.as_str());
                }
            }
        }

        // Reverse: related_names from models pointing to this one
        for m in self.dependents(model_name) {
            for r in &m.relations {
                if r.target_model == model_name && r.related_name == related_name {
                    return Some(m.name.as_str());
                }
            }
        }

        None
    }

    /// Get all models that depend on the given model (reverse dependencies)
    pub fn dependents(&self, model_name: &str) -> Vec<&ModelDef> {
        self.models
            .values()
            .filter(|m| m.relations.iter().any(|r| r.target_model == model_name))
            .collect()
    }

    /// Get dependency chain depth for a model (useful for detecting cycles)
    pub fn dependency_depth(&self, model_name: &str, visited: &mut Vec<String>) -> Option<usize> {
        if visited.contains(&model_name.to_string()) {
            return None; // Cycle detected
        }

        let model = self.models.get(model_name)?;
        if model.relations.is_empty() {
            return Some(0);
        }

        visited.push(model_name.to_string());

        let max_depth = model
            .relations
            .iter()
            .filter_map(|r| self.dependency_depth(&r.target_model, visited))
            .max()
            .unwrap_or(0);

        visited.pop();
        Some(max_depth + 1)
    }

    /// Merge another graph into this one
    pub fn merge(&mut self, other: ModelGraph) {
        self.models.extend(other.models);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn model_dependencies() {
        let mut model = ModelDef::new("Order", "orders/models.py", 10);
        model.add_relation(Relation {
            field_name: "user".into(),
            target_model: "User".into(),
            relation_type: RelationType::ForeignKey,
            related_name: "orders".into(),
        });
        model.add_relation(Relation {
            field_name: "product".into(),
            target_model: "Product".into(),
            relation_type: RelationType::ForeignKey,
            related_name: "product_set".into(),
        });

        let deps: Vec<_> = model.dependencies().collect();
        assert_eq!(deps, vec!["User", "Product"]);
    }

    #[test]
    fn graph_dependents() {
        let mut graph = ModelGraph::new();

        let user = ModelDef::new("User", "users/models.py", 1);

        let mut order = ModelDef::new("Order", "orders/models.py", 1);
        order.add_relation(Relation {
            field_name: "user".into(),
            target_model: "User".into(),
            relation_type: RelationType::ForeignKey,
            related_name: "user_set".into(),
        });

        let mut profile = ModelDef::new("Profile", "users/models.py", 50);
        profile.add_relation(Relation {
            field_name: "user".into(),
            target_model: "User".into(),
            relation_type: RelationType::OneToOne,
            related_name: "user_set".into(),
        });

        graph.add_model(user);
        graph.add_model(order);
        graph.add_model(profile);

        let user_dependents = graph.dependents("User");
        assert_eq!(user_dependents.len(), 2);
    }

    #[test]
    fn resolve_related_name_default_set() {
        let rel = Relation::new(
            "User",
            "country".into(),
            "Country".into(),
            RelationType::ForeignKey,
            None,
        );
        assert_eq!(rel.related_name(), "user_set");
    }

    #[test]
    fn resolve_related_name_default_one() {
        let rel = Relation::new(
            "User",
            "country".into(),
            "Country".into(),
            RelationType::OneToOne,
            None,
        );
        assert_eq!(rel.related_name(), "user");
    }

    #[test]
    fn resolve_related_name_explicit() {
        let rel = Relation::new(
            "User",
            "country".into(),
            "Country".into(),
            RelationType::OneToOne,
            Some("users".into()),
        );
        assert_eq!(rel.related_name(), "users");
    }

    #[test]
    fn resolve_related_name_using_class() {
        let rel = Relation::new(
            "User",
            "country".into(),
            "Country".into(),
            RelationType::OneToOne,
            Some("%(class)s_elements".into()),
        );
        assert_eq!(rel.related_name(), "user_elements");
    }
}
