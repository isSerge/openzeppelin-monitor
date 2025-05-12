//! Utility functions for evaluating expressions and resolving JSON paths

use super::evaluation::{ConditionEvaluator, EvaluationError};
use crate::services::filter::expression::ast::{
	Accessor, ComparisonOperator, ConditionLeft, Expression, LogicalOperator,
};

/// Traverses the Expression AST and uses ConditionEvaluator to evaluate conditions
/// Returns true if the expression evaluates to true, false otherwise
/// Returns an error if the evaluation fails
pub fn evaluate<'a>(
	expression: &Expression<'a>,
	evaluator: &impl ConditionEvaluator,
) -> Result<bool, EvaluationError> {
	match expression {
		Expression::Condition(condition) => {
			evaluator.evaluate_ast_condition(&condition.left, condition.operator, &condition.right)
		}
		Expression::Logical {
			left,
			operator,
			right,
		} => {
			let left_val = evaluate(left, evaluator)?;
			match operator {
				LogicalOperator::And => {
					if !left_val {
						Ok(false)
					} else {
						evaluate(right, evaluator)
					}
				}
				LogicalOperator::Or => {
					if left_val {
						Ok(true)
					} else {
						evaluate(right, evaluator)
					}
				}
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

/// Resolves a JSON path from a base variable name and accessors
/// Returns the resolved JSON value
/// Returns an error if the traversal fails
pub fn resolve_path_to_json_value<'a>(
	base_value_str: &str,
	base_kind_str: &str,
	accessors: &[Accessor],
	base_name_for_error: &str,
	full_lhs_expr_for_error: &ConditionLeft<'_>,
) -> Result<serde_json::Value, EvaluationError> {
	// Parse base value with error context
	let mut current_json_val = parse_base_value(
		base_value_str,
		base_kind_str,
		base_name_for_error,
		full_lhs_expr_for_error,
	)?;

	// Precompute all path segments for error messages
	let path_segments =
		build_path_segments(base_name_for_error, full_lhs_expr_for_error.accessors());

	for (accessor_idx, accessor) in accessors.iter().enumerate() {
		current_json_val =
			access_json_value(current_json_val, accessor, &path_segments[accessor_idx])?;
	}

	Ok(current_json_val)
}

/// Helper to parse the initial JSON value with proper error context
fn parse_base_value(
	base_value_str: &str,
	base_kind_str: &str,
	base_name: &str,
	full_expr: &ConditionLeft<'_>,
) -> Result<serde_json::Value, EvaluationError> {
	serde_json::from_str(base_value_str).map_err(|e| {
			EvaluationError::ParseError(format!(
					"Failed to parse value of base variable '{}' (kind: '{}', value: '{}') as JSON for path traversal. Error: {}. Full LHS: {:?}",
					base_name, base_kind_str, base_value_str, e, full_expr
			))
	})
}

/// Precomputes all path segments for error reporting
fn build_path_segments(base_name: &str, accessors: &[Accessor]) -> Vec<String> {
	let mut segments = Vec::with_capacity(accessors.len());
	let mut current_path = base_name.to_string();

	for accessor in accessors {
		current_path = match accessor {
			Accessor::Index(i) => format!("{}[{}]", current_path, i),
			Accessor::Key(k) => format!("{}.{}", current_path, k),
		};
		segments.push(current_path.clone());
	}

	segments
}

/// Helper to access JSON value with proper error handling
fn access_json_value(
	current_json: serde_json::Value,
	accessor: &Accessor,
	path_segment: &str,
) -> Result<serde_json::Value, EvaluationError> {
	match accessor {
		Accessor::Index(idx) => {
			let arr = current_json.as_array().ok_or_else(|| {
				EvaluationError::TypeMismatch(format!(
					"Array access on non-array at '{}'",
					path_segment
				))
			})?;

			arr.get(*idx).cloned().ok_or_else(|| {
				EvaluationError::IndexOutOfBounds(format!(
					"Index {} OOB at '{}'",
					idx, path_segment
				))
			})
		}
		Accessor::Key(key) => {
			let obj = current_json.as_object().ok_or_else(|| {
				EvaluationError::TypeMismatch(format!(
					"Key access on non-object at '{}'",
					path_segment
				))
			})?;

			obj.get(key.as_str()).cloned().ok_or_else(|| {
				EvaluationError::FieldNotFound(format!(
					"Key '{}' not found at '{}'",
					key, path_segment
				))
			})
		}
	}
}
