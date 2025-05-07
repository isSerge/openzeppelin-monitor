//! Shared logic for parsing and evaluating expressions

mod ast;
mod evaluation;
mod parsing;

pub use ast::{ComparisonOperator, LiteralValue};
pub use evaluation::{evaluate, ConditionEvaluator, EvaluationError};
pub use parsing::parse;
