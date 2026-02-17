//! Django model dependency graph extraction pass.
//!
//! Extracts Django model definitions and their relationships by parsing:
//! - Class definitions inheriting from models.Model
//! - ForeignKey, OneToOneField, ManyToManyField declarations
//! - related_name and other field options

use std::collections::{HashMap, HashSet};

use ruff_python_ast::StmtClassDef;
use ruff_python_ast::{Expr, ModModule, Stmt, visitor::Visitor};
use ruff_text_size::Ranged;

use crate::ir::model::{ModelDef, ModelGraph, Relation, RelationType};
use crate::passes::Pass;

const RELATION_FIELDS: [(&str, RelationType); 4] = [
    ("ForeignKey", RelationType::ForeignKey),
    ("OneToOneField", RelationType::OneToOne),
    ("ManyToManyField", RelationType::ManyToMany),
    ("GenericForeignKey", RelationType::GenericForeignKey),
];

#[derive(PartialEq, Debug, Clone)]
pub(crate) struct PotentialModel(pub (usize, StmtClassDef));

impl Eq for PotentialModel {}
impl std::hash::Hash for PotentialModel {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.0.hash(state);
    }
}

pub struct ModelGraphPass<'a> {
    filename: &'a str,
    source: &'a str,
    graph: ModelGraph,
    current_model: Option<ModelDef>,
    children_count: usize,
    children: HashSet<PotentialModel>,
}

impl<'a> ModelGraphPass<'a> {
    pub fn new(filename: &'a str, source: &'a str) -> Self {
        Self {
            filename,
            source,
            graph: ModelGraph::new(),
            current_model: None,
            children_count: 0,
            children: HashSet::new(),
        }
    }

    fn line_number(&self, offset: usize) -> usize {
        self.source[..offset].chars().filter(|&c| c == '\n').count() + 1
    }

    /// Check if a class inherits from models.Model (or similar)
    fn is_django_model(&self, bases: impl Iterator<Item = &'a Expr>) -> bool {
        for base in bases {
            match base {
                // models.Model
                Expr::Attribute(attr) => {
                    if attr.attr.id.as_str() == "Model"
                        && let Expr::Name(name) = attr.value.as_ref()
                        && name.id.as_str() == "models"
                    {
                        return true;
                    }
                }
                // Direct Model import or custom base class
                Expr::Name(name) => {
                    let n = name.id.as_str();
                    // Common Django patterns: Model, BaseModel, AbstractModel, etc.
                    // Avoid generic `Base` classes (e.g. SQLAlchemy declarative base).
                    if n == "Model" || n.ends_with("Model") {
                        return true;
                    }
                }
                _ => {}
            }
        }
        false
    }

    /// Insert a new potential child and track the state of the counter
    fn insert_potential_model(&mut self, class_stmt: &'a StmtClassDef) {
        self.children
            .insert(PotentialModel((self.children_count, class_stmt.clone())));
        self.children_count += 1;
    }

    fn process_class_body_stmt_for_model(&self, model: &mut ModelDef, stmt: &Stmt) {
        process_class_body_stmt_for_model(model, stmt);
    }

    /// Process a class body statement to find relation fields
    fn process_class_body_stmt(&mut self, stmt: &Stmt) {
        if let Some(model) = self.current_model.as_mut() {
            process_class_body_stmt_for_model(model, stmt);
        }
    }

    fn resolve_potential_children(&mut self) {
        let abstract_models: HashMap<String, ModelDef> = self
            .graph
            .models()
            .filter(|&m| m.is_abstract())
            .map(|m| (m.name.clone(), m.clone()))
            .collect();

        for potential in self.children.iter() {
            let (_, class_def) = &potential.0;

            // Find parent
            let parent_name = self.find_parent_name(class_def);
            let Some(parent) = parent_name.and_then(|n| abstract_models.get(n)) else {
                continue;
            };

            // Create child model with parent's relations
            let mut child = ModelDef::new(
                class_def.name.to_string(),
                self.filename,
                self.line_number(class_def.range().start().to_usize()),
            );

            // Copy parent relations
            child.relations.extend(parent.relations.clone());

            // Parse child's own relations
            for stmt in class_def.body.iter() {
                self.process_class_body_stmt_for_model(&mut child, stmt);
            }

            self.graph.add_model(child);
        }
    }

    fn find_parent_name(&self, class_def: &'a StmtClassDef) -> Option<&'a str> {
        for arg in class_def.arguments.as_ref()?.args.as_ref() {
            if let Some(name_expr) = arg.as_name_expr() {
                let name = name_expr.id.as_str();
                if self.graph.models().any(|m| m.name == name) {
                    return Some(name);
                }
            }
        }
        None
    }
}

/// Process the body of a class, capturing if it is abstract or a django relation
fn process_class_body_stmt_for_model(model: &mut ModelDef, stmt: &Stmt) {
    // Check for Meta class
    if let Stmt::ClassDef(stmt_class) = stmt
        && stmt_class.name.as_str() == "Meta"
    {
        for meta_stmt in stmt_class.body.iter() {
            if let Stmt::Assign(assign) = meta_stmt
                && let Some(Expr::Name(name)) = assign.targets.first()
                && let Expr::BooleanLiteral(bool_lit) = assign.value.as_ref()
                && bool_lit.value
                && name.id == "abstract"
            {
                model.mark_as_abstract();
                return;
            }
        }
    }

    // Extract relation
    if let Some(relation) = extract_relation_from_stmt(stmt, &model.name) {
        model.add_relation(relation);
    }
}

/// Extract relation type from a field call
fn get_relation_type(call: &ruff_python_ast::ExprCall) -> Option<RelationType> {
    let name = match call.func.as_ref() {
        // models.ForeignKey(...)
        Expr::Attribute(attr) => attr.attr.id.as_str(),
        // ForeignKey(...) - direct import
        Expr::Name(name) => name.id.as_str(),
        _ => return None,
    };

    RELATION_FIELDS
        .iter()
        .find(|(field_name, _)| *field_name == name)
        .map(|(_, rel_type)| rel_type.clone())
}

/// Extract the target model from a relation field
fn extract_target_model(call: &ruff_python_ast::ExprCall) -> Option<String> {
    // First positional argument is the target model
    let first_arg = call.arguments.args.first()?;

    match first_arg {
        // String reference: ForeignKey('ModelName') or ForeignKey('app.ModelName')
        Expr::StringLiteral(s) => {
            let value = s.value.to_string();
            // Handle 'app.Model' format - take the model name
            Some(value.split('.').next_back().unwrap_or(&value).to_string())
        }
        // Direct reference: ForeignKey(ModelName)
        Expr::Name(name) => Some(name.id.to_string()),
        // models.Model style isn't typically used for FK targets but handle it
        Expr::Attribute(attr) => Some(attr.attr.id.to_string()),
        _ => None,
    }
}

/// Extract related_name from field kwargs
fn extract_related_name(call: &ruff_python_ast::ExprCall) -> Option<String> {
    for keyword in call.arguments.keywords.iter() {
        if let Some(arg) = &keyword.arg
            && arg.as_str() == "related_name"
            && let Expr::StringLiteral(s) = &keyword.value
        {
            return Some(s.value.to_string());
        }
    }
    None
}

/// Analyze an Stmt and capture the relation if it exists
fn extract_relation_from_stmt(stmt: &Stmt, model_name: &str) -> Option<Relation> {
    match stmt {
        Stmt::Assign(assign) => {
            let Some(Expr::Name(target)) = assign.targets.first() else {
                return None;
            };
            extract_relation_from_value(&target.id, assign.value.as_ref(), model_name)
        }
        Stmt::AnnAssign(assign) => {
            let Expr::Name(target) = assign.target.as_ref() else {
                return None;
            };
            let value = assign.value.as_ref()?;
            extract_relation_from_value(&target.id, value, model_name)
        }
        _ => None,
    }
}

fn extract_relation_from_value(
    field_name: &str,
    value: &Expr,
    model_name: &str,
) -> Option<Relation> {
    let Expr::Call(call) = value else {
        return None;
    };

    let relation_type = get_relation_type(call)?;
    let target_model = extract_target_model(call)?;
    let related_name = extract_related_name(call);

    Some(Relation::new(
        model_name,
        field_name.to_string(),
        target_model,
        relation_type,
        related_name,
    ))
}

impl<'a> Pass<'a> for ModelGraphPass<'a> {
    type Output = ModelGraph;

    fn run(&mut self, module: &'a ModModule) -> Self::Output {
        for stmt in &module.body {
            self.visit_stmt(stmt);
        }

        self.resolve_potential_children();

        std::mem::take(&mut self.graph)
    }
}

impl<'a> Visitor<'a> for ModelGraphPass<'a> {
    fn visit_stmt(&mut self, stmt: &'a Stmt) {
        match stmt {
            Stmt::ClassDef(class) => {
                let Some(ref args) = class.arguments else {
                    return;
                };

                let args = &args.args;

                if self.is_django_model(args.iter()) {
                    let line = self.line_number(class.range().start().to_usize());
                    self.current_model =
                        Some(ModelDef::new(class.name.to_string(), self.filename, line));

                    // Process class body
                    for body_stmt in class.body.iter() {
                        self.process_class_body_stmt(body_stmt);
                    }

                    // Finalize model
                    if let Some(model) = self.current_model.take() {
                        self.graph.add_model(model);
                    }
                } else if !args.is_empty() {
                    // If inherit, track it
                    self.insert_potential_model(class);
                }
            }
            _ => {
                ruff_python_ast::visitor::walk_stmt(self, stmt);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ruff_python_parser::parse_module;

    fn run_pass(source: &str) -> ModelGraph {
        let parsed = parse_module(source).expect("should parse");
        let mut pass = ModelGraphPass::new("test.py", source);
        pass.run(parsed.syntax())
    }

    #[test]
    fn extract_simple_model() {
        let source = r#"
from django.db import models

class User(models.Model):
    name = models.CharField(max_length=100)
"#;
        let graph = run_pass(source);
        assert_eq!(graph.model_count(), 1);
        assert!(graph.get("User").is_some());
    }

    #[test]
    fn doesnt_extract_no_model_class() {
        let source = r#"
class User:
    name = "test"
"#;
        let graph = run_pass(source);
        assert_eq!(graph.model_count(), 0);
    }

    #[test]
    fn extract_foreign_key() {
        let source = r#"
from django.db import models

class Order(models.Model):
    user = models.ForeignKey(User, on_delete=models.CASCADE)
    product = models.ForeignKey('Product', on_delete=models.CASCADE, related_name='orders')
"#;
        let graph = run_pass(source);
        let order = graph.get("Order").expect("Order should exist");

        assert_eq!(order.relations.len(), 2);

        let user_rel = &order.relations[0];
        assert_eq!(user_rel.field_name, "user");
        assert_eq!(user_rel.target_model, "User");
        assert_eq!(user_rel.relation_type, RelationType::ForeignKey);

        let product_rel = &order.relations[1];
        assert_eq!(product_rel.field_name, "product");
        assert_eq!(product_rel.target_model, "Product");
        assert_eq!(product_rel.related_name("Order"), "orders");
    }

    #[test]
    fn extract_multiple_relation_types() {
        let source = r#"
class Profile(models.Model):
    user = models.OneToOneField(User, on_delete=models.CASCADE)
    
class Article(models.Model):
    author = models.ForeignKey(User, on_delete=models.CASCADE)
    tags = models.ManyToManyField(Tag)
"#;
        let graph = run_pass(source);

        let profile = graph.get("Profile").unwrap();
        assert_eq!(profile.relations[0].relation_type, RelationType::OneToOne);

        let article = graph.get("Article").unwrap();
        assert_eq!(article.relations.len(), 2);
        assert_eq!(article.relations[0].relation_type, RelationType::ForeignKey);
        assert_eq!(article.relations[1].relation_type, RelationType::ManyToMany);
    }

    #[test]
    fn extract_annotated_relation_fields() {
        let source = r#"
class Profile(models.Model):
    user: User = models.ForeignKey(User, on_delete=models.CASCADE)
    "#;
        let graph = run_pass(source);
        let profile = graph.get("Profile").expect("Profile should exist");

        assert_eq!(profile.relations.len(), 1);
        assert_eq!(profile.relations[0].field_name, "user");
        assert_eq!(profile.relations[0].target_model, "User");
        assert_eq!(profile.relations[0].relation_type, RelationType::ForeignKey);
    }

    #[test]
    fn dependents_query() {
        let source = r#"
class User(models.Model):
    name = models.CharField()

class Order(models.Model):
    user = models.ForeignKey(User, on_delete=models.CASCADE)

class Profile(models.Model):
    user = models.OneToOneField(User, on_delete=models.CASCADE)
"#;
        let graph = run_pass(source);
        let dependents = graph.dependents("User");

        assert_eq!(dependents.len(), 2);
        let names: Vec<_> = dependents.iter().map(|m| m.name.as_str()).collect();
        assert!(names.contains(&"Order"));
        assert!(names.contains(&"Profile"));
    }

    #[test]
    fn string_reference_with_app_label() {
        let source = r#"
class Order(models.Model):
    user = models.ForeignKey('accounts.User', on_delete=models.CASCADE)
"#;
        let graph = run_pass(source);
        let order = graph.get("Order").unwrap();

        // Should extract just "User" from "accounts.User"
        assert_eq!(order.relations[0].target_model, "User");
    }

    #[test]
    fn detect_abstract_model() {
        let source = r#"
class Order(models.Model):
    pass

    class Meta:
        abstract = True
"#;
        let graph = run_pass(source);
        let order = graph.get("Order").unwrap();

        assert!(order.is_abstract());
    }

    #[test]
    fn detect_is_not_abstract_model() {
        let source = r#"
class Order(models.Model):
    pass
"#;
        let graph = run_pass(source);
        let order = graph.get("Order").unwrap();

        assert!(!order.is_abstract());
    }

    #[test]
    fn detect_inheritance() {
        let source = r#"

class User(models.Model):
    pass

class Seller(models.Model):
    pass

class Order(models.Model):
    user = models.ForeignKey(User)

    class Meta:
        abstract = True

class CreatedOrder(Order):
    seller = models.ForeignKey(Seller)
"#;
        let graph = run_pass(source);
        let order = graph.get("Order").unwrap();
        let created_order = graph.get("CreatedOrder").unwrap();

        assert!(order.is_abstract());
        assert!(!created_order.is_abstract());
        assert!(
            created_order
                .relations
                .iter()
                .any(|r| r.target_model == "User")
        );

        assert!(
            created_order
                .relations
                .iter()
                .any(|r| r.target_model == "Seller")
        );
    }

    #[test]
    fn inherited_related_name() {
        let source = r#"
class User(models.Model):
    pass

class Seller(models.Model):
    pass

class Order(models.Model):
    user = models.ForeignKey(User, related_name="%(class)ss")

    class Meta:
        abstract = True

class CreatedOrder(Order):
    seller = models.ForeignKey(Seller)
"#;

        let graph = run_pass(source);
        let created_order = graph.get("CreatedOrder").unwrap();

        assert!(
            created_order
                .relations
                .iter()
                .any(|r| r.target_model == "User"
                    && r.related_name("CreatedOrder") == "createdorders")
        );
    }

    #[test]
    fn doesnt_extract_sqlalchemy_base_models() {
        let source = r#"
from sqlalchemy.ext.declarative import declarative_base

Base = declarative_base()

class User(Base):
    pass
"#;
        let graph = run_pass(source);
        assert_eq!(graph.model_count(), 0);
    }
}
