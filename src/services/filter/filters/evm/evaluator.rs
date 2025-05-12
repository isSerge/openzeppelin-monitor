use super::helpers::{are_same_address, string_to_u256};
use crate::{
	models::EVMMatchParamEntry,
	services::filter::expression::{
		compare_ordered_values, ComparisonOperator, ConditionEvaluator, EvaluationError,
		LiteralValue,
	},
};

type EVMArgs = Vec<EVMMatchParamEntry>;

pub struct EVMConditionEvaluator<'a> {
	args: &'a EVMArgs,
}

impl<'a> EVMConditionEvaluator<'a> {
	pub fn new(args: &'a EVMArgs) -> Self {
		Self { args }
	}

	/// Compares potential U256 LHS value with the RHS literal value
	/// Handles decimal and hex inputs for both sides
	fn compare_u256(
		&self,
		left_str: &str,
		operator: &ComparisonOperator,
		right_literal: &LiteralValue<'_>,
	) -> Result<bool, EvaluationError> {
		let left = string_to_u256(left_str).map_err(|error| {
			EvaluationError::ParseError(format!(
				"Failed to parse LHS value '{}' as U256: {}",
				left_str, error,
			))
		})?;

		let right_str = match right_literal {
			LiteralValue::Number(s) => s,
			LiteralValue::Str(s) => s,
			_ => {
				return Err(EvaluationError::TypeMismatch(format!(
					"Expected number or string literal for U256 comparison with found: {:?}",
					right_literal
				)));
			}
		};

		let right = string_to_u256(right_str).map_err(|error| {
			EvaluationError::ParseError(format!(
				"Failed to parse RHS value '{}' as U256: {}",
				right_str, error,
			))
		})?;

		tracing::debug!("Comparing U256: left: {}, op: {:?}, right: {}", left, operator, right);

		compare_ordered_values(&left, operator, &right)
	}

	fn compare_address(
		&self,
		left: &str,
		operator: &ComparisonOperator,
		right_literal: &LiteralValue<'_>,
	) -> Result<bool, EvaluationError> {
		let right = match right_literal {
			LiteralValue::Str(str) => *str,
			_ => {
				return Err(EvaluationError::TypeMismatch(format!(
					"Expected string literal for address comparison, found: {:?}",
					right_literal
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

	fn compare_string(
		&self,
		lhs_str: &str,
		operator: &ComparisonOperator,
		rhs_literal: &LiteralValue<'_>,
	) -> Result<bool, EvaluationError> {
		// Perform case-insensitive comparisons for all string operators
		let left = lhs_str.to_lowercase();

		let right = match rhs_literal {
			LiteralValue::Str(s) => s.to_lowercase(),
			_ => {
				return Err(EvaluationError::TypeMismatch(format!(
					"Expected string literal comparison, found: {:?}",
					rhs_literal
				)));
			}
		};

		tracing::debug!(
			"Comparing strings: left: {}, operator: {:?}, right: {}",
			left,
			operator,
			right,
		);

		match operator {
			ComparisonOperator::Eq => Ok(left == right),
			ComparisonOperator::Ne => Ok(left != right),
			ComparisonOperator::StartsWith => Ok(left.starts_with(&right)),
			ComparisonOperator::EndsWith => Ok(left.ends_with(&right)),
			ComparisonOperator::Contains => Ok(left.contains(&right)),
			_ => Err(EvaluationError::UnsupportedOperator {
				op: format!("Operator {:?} not supported for type String", operator),
			}),
		}
	}
}

impl ConditionEvaluator for EVMConditionEvaluator<'_> {
	fn get_base_param(&self, name: &str) -> Result<(&str, &str), EvaluationError> {
		self.args
			.iter()
			.find(|p| p.name == name)
			.map(|p| (p.value.as_str(), p.kind.as_str()))
			.ok_or_else(|| EvaluationError::VariableNotFound(name.to_string()))
	}

	fn compare_final_values(
		&self,
		lhs_kind_str: &str,
		lhs_value_str: &str, // Value after path traversal, or original base value
		operator: &ComparisonOperator,
		rhs_literal: &LiteralValue<'_>,
	) -> Result<bool, EvaluationError> {
		tracing::debug!(
			"EVM Comparing: lhs_val='{}', lhs_kind='{}', op='{:?}', rhs_lit='{:?}'",
			lhs_value_str,
			lhs_kind_str,
			operator,
			rhs_literal
		);

		match lhs_kind_str.to_lowercase().as_str() {
			// "number" if inferred from JSON number
			"uint32" | "uint64" | "uint256" | "uint" | "u256" | "number" => {
				self.compare_u256(lhs_value_str, operator, rhs_literal)
			}
			"address" => self.compare_address(lhs_value_str, operator, rhs_literal),
			"string" => self.compare_string(lhs_value_str, operator, rhs_literal),
			"bool" => {
				let lhs = lhs_value_str.parse::<bool>().map_err(|_| {
					EvaluationError::ParseError(format!("Invalid EVM bool LHS: {}", lhs_value_str))
				})?;
				let rhs = match rhs_literal {
					LiteralValue::Bool(b) => *b,
					_ => {
						return Err(EvaluationError::TypeMismatch(
							"Expected bool literal for EVM Bool comparison".into(),
						))
					}
				};
				match operator {
					ComparisonOperator::Eq => Ok(lhs == rhs),
					ComparisonOperator::Ne => Ok(lhs != rhs),
					_ => Err(EvaluationError::UnsupportedOperator {
						op: format!("Unsupported op {:?} for EVM Bool", operator),
					}),
				}
			}
			// TODO: Implement support for EVM collections (arrays, maps, etc.)
			"vec" | "map" => {
				unimplemented!("EVM collections (arrays, maps) not yet supported in comparison");
			}
			// TODO: Implement support for bytes
			"bytes" | "bytes32" => {
				unimplemented!("EVM bytes not yet supported in comparison");
			}
			_ => Err(EvaluationError::TypeMismatch(format!(
				"Unsupported EVM parameter kind for comparison: {}",
				lhs_kind_str
			))),
		}
	}

	fn get_kind_from_json_value(&self, value: &serde_json::Value) -> String {
		match value {
			serde_json::Value::String(s) => {
				let is_address = s.starts_with("0x")
					&& s.len() == 42
					&& s.chars().skip(2).all(|c| c.is_ascii_hexdigit());
				let is_bytes =
					s.starts_with("0x") && s.chars().skip(2).all(|c| c.is_ascii_hexdigit());

				if is_address {
					"Address".to_string()
				} else if is_bytes {
					// Could be Bytes or a hex-encoded U256
					if s.len() == 66 {
						// 0x + 32 bytes (64 hex chars)
						"Bytes32".to_string()
					} else {
						"Bytes".to_string()
					}
				} else {
					"String".to_string()
				}
			}
			// Use generic "Number" for JSON numbers since all map to U256
			serde_json::Value::Number(_) => "Number".to_string(),
			serde_json::Value::Bool(_) => "Bool".to_string(),
			serde_json::Value::Array(_) => "Vec".to_string(),
			serde_json::Value::Object(_) => "Map".to_string(),
			serde_json::Value::Null => "Null".to_string(),
		}
	}
}
