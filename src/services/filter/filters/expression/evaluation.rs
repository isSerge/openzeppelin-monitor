use thiserror::Error;

use crate::services::filter::filters::expression::ast::{
	ComparisonOperator, Expression, LogicalOperator, Value,
};

#[derive(Debug, PartialEq, Eq, Error)]
pub enum EvaluationError {
    #[error("Variable not found: {0}")]
    VariableNotFound(String),
    #[error("Type mismatch: {0}")]
    TypeMismatch(String),
    #[error("Unsupported operator '{op}' for types")]
    UnsupportedOperator { op: String },
    #[error("Failed to parse value: {0}")]
    ParseError(String),
    #[error("Evaluation logic error: {0}")]
    Internal(String),
}

pub trait ConditionEvaluator {
	fn evaluate_ast_condition(
			&self,
			variable_name: &str,
			operator: ComparisonOperator,
			value: &Value<'_>,
	) -> Result<bool, EvaluationError>;
}

/// Traverses the Expression AST and uses ConditionEvaluator to evaluate conditions
/// Returns true if the expression evaluates to true, false otherwise
/// Returns an error if the evaluation fails
pub fn evaluate<'a>(
	expression: &Expression<'a>,
	evaluator: &impl ConditionEvaluator,
) -> Result<bool, EvaluationError> {
	match expression {
			Expression::Condition(condition) => {
					evaluator.evaluate_ast_condition(
							condition.left,
							condition.operator.clone(),
							&condition.right,
					)
			}
			Expression::Logical { left, operator, right } => {
					let left_val = evaluate(left, evaluator)?;
					match operator {
							LogicalOperator::And => if !left_val { Ok(false) } else { evaluate(right, evaluator) },
							LogicalOperator::Or =>  if left_val { Ok(true) } else { evaluate(right, evaluator) },
					}
			}
	}
}
