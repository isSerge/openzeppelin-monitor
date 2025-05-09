use super::helpers::are_same_address;
use crate::{
	models::EVMMatchParamEntry,
	services::filter::expression::{
		compare_ordered_values, ComparisonOperator, ConditionEvaluator, EvaluationError,
		LiteralValue,
	},
};
use alloy::primitives::U256;
use std::str::FromStr;

type EVMArgs = Vec<EVMMatchParamEntry>;

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
		tracing::debug!(
			"Evaluating EVM condition: {} {:?} {:?}",
			variable_name,
			operator,
			value
		);

		// Find the parameter in args
		let Some(param) = self.args.iter().find(|p| p.name == variable_name) else {
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

				tracing::debug!("Comparing numeric: left: {}, right: {}", left, right);

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

				tracing::debug!("Comapring addresses: left: {}, right: {}", left, right);

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

				// Make sure right is a string when using starts_with, ends_with, or contains operators
				let right = match operator {
					ComparisonOperator::StartsWith
					| ComparisonOperator::EndsWith
					| ComparisonOperator::Contains => match value {
						LiteralValue::Str(s) => s.to_lowercase(),
						LiteralValue::Number(n) => n.to_string().to_lowercase(),
						LiteralValue::Bool(b) => b.to_string().to_lowercase(),
					},
					_ => match value {
						LiteralValue::Str(str) => str.to_lowercase(),
						_ => {
							return Err(EvaluationError::TypeMismatch(format!(
								"Expected string literal for comparison with '{}', found: {:?}",
								param.name, value
							)));
						}
					},
				};

				tracing::debug!("Comparing strings: left: {}, right: {}", left, right);

				match operator {
					ComparisonOperator::Eq => Ok(left == right),
					ComparisonOperator::Ne => Ok(left != right),
					ComparisonOperator::StartsWith => Ok(left.starts_with(&right)),
					ComparisonOperator::EndsWith => Ok(left.ends_with(&right)),
					ComparisonOperator::Contains => Ok(left.contains(&right)),
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
