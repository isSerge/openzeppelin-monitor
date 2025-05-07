use alloy::primitives::U256;

use crate::{
	models::EVMMatchParamEntry,
	services::filter::expression::{
		ComparisonOperator, ConditionEvaluator, EvaluationError, LiteralValue,
	},
};
use std::str::FromStr;

use super::helpers::are_same_address;

// TODO: re-use for Stellar
fn compare_ordered_values<T: Ord>(
	left: &T,
	op: ComparisonOperator,
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

type EVMArgs = Option<Vec<EVMMatchParamEntry>>;

pub struct EVMConditionEvaluator<'a> {
	args: &'a EVMArgs,
}

impl<'a> EVMConditionEvaluator<'a> {
	pub fn new(args: &'a EVMArgs) -> Self {
		Self { args }
	}
}

impl<'a> ConditionEvaluator for EVMConditionEvaluator<'a> {
	fn evaluate_ast_condition(
		&self,
		variable_name: &str,
		operator: ComparisonOperator,
		value: &LiteralValue<'_>,
	) -> Result<bool, EvaluationError> {
		// TODO: check args when creating the evaluator
		let Some(args) = self.args else {
			return Err(EvaluationError::Internal("No args provided".to_string()));
		};

		// Find the parameter in args
		let Some(param) = args.iter().find(|p| p.name == variable_name) else {
			return Err(EvaluationError::VariableNotFound(variable_name.to_string()));
		};

		// Evaluate single condition
		match param.kind.as_str() {
			"uint64" | "uint256" | "uint" => {
				let left = U256::from_str(&param.value).map_err(|_| {
					EvaluationError::ParseError(format!(
						"Failed to parse parameter '{}' (value: {}) as U256",
						param.name, param.value,
					))
				})?;

				let right_num_str = match value {
					LiteralValue::Number(num_str) => num_str,
					_ => {
						return Err(EvaluationError::TypeMismatch(format!(
							"Expected number literal for comparison with '{}', found: {:?}",
							param.name, value
						)));
					}
				};

				let right = U256::from_str(right_num_str).map_err(|error| {
					EvaluationError::ParseError(format!(
						"Failed to parse comparison value '{}' as U256: {}",
						right_num_str, error,
					))
				})?;

				compare_ordered_values(&left, operator, &right)
			}
			"address" => {
				let left = &param.value;
				let right = match value {
					LiteralValue::Str(str) => *str,
					_ => {
						return Err(EvaluationError::TypeMismatch(format!(
							"Expected string literal for address comparison with '{}', found: {:?}",
							param.name, value
						)));
					}
				};

				match operator {
					ComparisonOperator::Eq => Ok(are_same_address(left, right)),
					ComparisonOperator::Ne => Ok(!are_same_address(left, right)),
					_ => Err(EvaluationError::UnsupportedOperator {
						op: format!("Unsupported operator for address type: {:?}", operator),
					}),
				}
			}
			"string" => {
				// Perform case-insensitive comparisons for all string operators
				let left = param.value.to_lowercase();
				let right = match value {
					LiteralValue::Str(str) => str.to_lowercase(),
					_ => {
						return Err(EvaluationError::TypeMismatch(format!(
							"Expected string literal for comparison with '{}', found: {:?}",
							param.name, value
						)));
					}
				};

				match operator {
					ComparisonOperator::Eq => Ok(left == right),
					ComparisonOperator::Ne => Ok(left != right),
					// TODO: Implement these operators
					// "starts_with" => param_lower.starts_with(&value_lower),
					// "ends_with" => param_lower.ends_with(&value_lower),
					// "contains" => param_lower.contains(&value_lower),
					_ => Err(EvaluationError::UnsupportedOperator {
						op: format!("Unsupported operator for string type: {:?}", operator),
					}),
				}
			}
			_ => Err(EvaluationError::TypeMismatch(
				"Unsupported EVM parameter type".to_string(),
			)),
		}
	}
}
