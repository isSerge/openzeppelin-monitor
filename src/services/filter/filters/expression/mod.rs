//! Shared logic for parsing and evaluating expressions

mod evaluation;
mod parsing;
mod ast;

pub use ast::{ComparisonOperator, Condition, Expression, LogicalOperator, Value};
pub use evaluation::{evaluate, ConditionEvaluator, EvaluationError};
pub use parsing::{parse, ExpressionParseError};
