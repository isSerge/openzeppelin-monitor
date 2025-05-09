use thiserror::Error;

use crate::services::filter::expression::ast::{
	ComparisonOperator, Expression, LogicalOperator, LiteralValue,
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
}

pub trait ConditionEvaluator {
	fn evaluate_ast_condition(
			&self,
			variable_name: &str,
			operator: ComparisonOperator,
			value: &LiteralValue<'_>,
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
							condition.operator,
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

pub fn compare_ordered_values<T: Ord>(
	left: &T,
	op: &ComparisonOperator,
	right: &T,
) -> Result<bool, EvaluationError> {
	match op {
		ComparisonOperator::Eq => Ok(left == right),
		ComparisonOperator::Ne => Ok(left != right),
		ComparisonOperator::Gt => Ok(left > right),
		ComparisonOperator::Gte => Ok(left >= right),
		ComparisonOperator::Lt => Ok(left < right),
		ComparisonOperator::Lte => Ok(left <= right),
		_ => Err(EvaluationError::UnsupportedOperator {
			op: format!("Unsupported operator for ordered types: {:?}", op),
		}),
	}
}
