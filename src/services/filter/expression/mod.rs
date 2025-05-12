//! Shared logic for parsing and evaluating expressions

mod ast;
mod evaluation;
mod parsing;
mod utils;

pub use ast::{ComparisonOperator, LiteralValue};
pub use evaluation::{ConditionEvaluator, EvaluationError};
pub use parsing::parse;
pub use utils::{compare_ordered_values, evaluate};
