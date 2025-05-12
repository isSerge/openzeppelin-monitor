//! Shared logic for parsing and evaluating expressions

mod ast;
mod evaluation;
mod parsing;
mod utils;

pub use ast::{Accessor, ComparisonOperator, ConditionLeft, LiteralValue};
pub use evaluation::{ConditionEvaluator, EvaluationError};
pub use parsing::parse;
pub use utils::{compare_ordered_values, evaluate, resolve_path_to_json_value};
