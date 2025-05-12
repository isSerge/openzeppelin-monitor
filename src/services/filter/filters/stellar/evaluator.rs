use super::helpers;
use crate::{
	models::StellarMatchParamEntry,
	services::filter::expression::{
		compare_ordered_values, ComparisonOperator, ConditionEvaluator, EvaluationError,
		LiteralValue,
	},
};

type StellarArgs = Vec<StellarMatchParamEntry>;

pub struct StellarConditionEvaluator<'a> {
	args: &'a StellarArgs,
}

impl<'a> StellarConditionEvaluator<'a> {
	pub fn new(args: &'a StellarArgs) -> Self {
		Self { args }
	}

	fn compare_bool(
		&self,
		param_value: &str,
		operator: &ComparisonOperator,
		compare_value: &LiteralValue<'_>,
	) -> Result<bool, EvaluationError> {
		let Ok(left) = param_value.parse::<bool>() else {
			return Err(EvaluationError::ParseError(format!(
				"Failed to parse bool parameter value: {}",
				param_value
			)));
		};

		let right = match compare_value {
			LiteralValue::Bool(b) => *b,
			_ => {
				return Err(EvaluationError::TypeMismatch(format!(
					"Expected bool literal for comparison, found: {:?}",
					compare_value
				)))
			}
		};

		tracing::debug!("Comparing bool: left: {}, right: {}", left, right);

		match operator {
			ComparisonOperator::Eq => Ok(left == right),
			ComparisonOperator::Ne => Ok(left != right),
			_ => Err(EvaluationError::UnsupportedOperator {
				op: format!("Unsupported operator: {:?}", operator),
			}),
		}
	}

	fn compare_u64(
		&self,
		param_value: &str,
		operator: &ComparisonOperator,
		compare_value: &LiteralValue,
	) -> Result<bool, EvaluationError> {
		let Ok(left) = param_value.parse::<u64>() else {
			return Err(EvaluationError::ParseError(format!(
				"Failed to parse u64 parameter value: {}",
				param_value
			)));
		};

		let right = match compare_value {
			LiteralValue::Number(num_str) => num_str.parse::<u64>().map_err(|_| {
				EvaluationError::ParseError(format!(
					"Failed to parse comparison value '{}' as u64",
					num_str
				))
			})?,
			_ => {
				return Err(EvaluationError::TypeMismatch(format!(
					"Expected number literal for comparison with '{}', found: {:?}",
					param_value, compare_value
				)))
			}
		};

		tracing::debug!("Comparing u64: left: {}, right: {}", left, right);

		compare_ordered_values(&left, operator, &right)
	}

	fn compare_u32(
		&self,
		param_value: &str,
		operator: &ComparisonOperator,
		compare_value: &LiteralValue,
	) -> Result<bool, EvaluationError> {
		let Ok(left) = param_value.parse::<u32>() else {
			return Err(EvaluationError::ParseError(format!(
				"Failed to parse u32 parameter value: {}",
				param_value
			)));
		};

		let right = match compare_value {
			LiteralValue::Number(num_str) => num_str.parse::<u32>().map_err(|_| {
				EvaluationError::ParseError(format!(
					"Failed to parse comparison value '{}' as u32",
					num_str
				))
			})?,
			_ => {
				return Err(EvaluationError::TypeMismatch(format!(
					"Expected number literal for comparison with '{}', found: {:?}",
					param_value, compare_value
				)))
			}
		};

		tracing::debug!("Comparing u32: left: {}, right: {}", left, right);

		compare_ordered_values(&left, operator, &right)
	}

	fn compare_i32(
		&self,
		param_value: &str,
		operator: &ComparisonOperator,
		compare_value: &LiteralValue<'_>,
	) -> Result<bool, EvaluationError> {
		let left = param_value.parse::<i32>().map_err(|_| {
			EvaluationError::ParseError(format!(
				"Failed to parse i32 parameter value: {}",
				param_value
			))
		})?;

		let right = match compare_value {
			LiteralValue::Number(num_str) => num_str.parse::<i32>().map_err(|_| {
				EvaluationError::ParseError(format!(
					"Failed to parse comparison value '{}' as i32",
					num_str
				))
			})?,
			_ => {
				return Err(EvaluationError::TypeMismatch(format!(
					"Expected number literal for i32 comparison with '{}', found: {:?}",
					param_value, compare_value
				)))
			}
		};

		tracing::debug!("Comparing i32: left: {}, right: {}", left, right);

		compare_ordered_values(&left, operator, &right)
	}

	fn compare_i64(
		&self,
		param_value: &str,
		operator: &ComparisonOperator,
		compare_value: &LiteralValue<'_>,
	) -> Result<bool, EvaluationError> {
		let left = param_value.parse::<i64>().map_err(|_| {
			EvaluationError::ParseError(format!(
				"Failed to parse i64 parameter value: {}",
				param_value
			))
		})?;

		let right = match compare_value {
			LiteralValue::Number(num_str) => num_str.parse::<i64>().map_err(|_| {
				EvaluationError::ParseError(format!(
					"Failed to parse comparison value '{}' as i64",
					num_str
				))
			})?,
			_ => {
				return Err(EvaluationError::TypeMismatch(format!(
					"Expected number literal for i64 comparison with '{}', found: {:?}",
					param_value, compare_value
				)))
			}
		};

		tracing::debug!("Comparing i64: left: {}, right: {}", left, right);

		compare_ordered_values(&left, operator, &right)
	}

	fn compare_u128(
		&self,
		param_value: &str,
		operator: &ComparisonOperator,
		compare_value: &LiteralValue<'_>,
	) -> Result<bool, EvaluationError> {
		let left = param_value.parse::<u128>().map_err(|_| {
			EvaluationError::ParseError(format!(
				"Failed to parse u128 parameter value: {}",
				param_value
			))
		})?;

		let right = match compare_value {
			LiteralValue::Number(num_str) => num_str.parse::<u128>().map_err(|_| {
				EvaluationError::ParseError(format!(
					"Failed to parse comparison value '{}' as u128",
					num_str
				))
			})?,
			_ => {
				return Err(EvaluationError::TypeMismatch(format!(
					"Expected number literal for u128 comparison with '{}', found: {:?}",
					param_value, compare_value
				)))
			}
		};

		tracing::debug!("Comparing u128: left: {}, right: {}", left, right);

		compare_ordered_values(&left, operator, &right)
	}

	fn compare_i128(
		&self,
		param_value: &str,
		operator: &ComparisonOperator,
		compare_value: &LiteralValue<'_>,
	) -> Result<bool, EvaluationError> {
		let left = param_value.parse::<i128>().map_err(|_| {
			EvaluationError::ParseError(format!(
				"Failed to parse i128 parameter value: {}",
				param_value
			))
		})?;

		let right = match compare_value {
			LiteralValue::Number(num_str) => num_str.parse::<i128>().map_err(|_| {
				EvaluationError::ParseError(format!(
					"Failed to parse comparison value '{}' as i128",
					num_str
				))
			})?,
			_ => {
				return Err(EvaluationError::TypeMismatch(format!(
					"Expected number literal for i128 comparison with '{}', found: {:?}",
					param_value, compare_value
				)))
			}
		};

		tracing::debug!("Comparing i128: left: {}, right: {}", left, right);

		compare_ordered_values(&left, operator, &right)
	}

	/// Compares two large integers (u256/i256) as strings.
	fn compare_large_int_as_string(
		&self,
		left: &str,
		operator: &ComparisonOperator,
		compare_value: &LiteralValue<'_>,
	) -> Result<bool, EvaluationError> {
		let right = match compare_value {
			LiteralValue::Number(s) => s,
			LiteralValue::Str(s) => s,
			_ => {
				return Err(EvaluationError::TypeMismatch(format!(
					"Expected number or string literal for i256 comparison, found: {:?}",
					compare_value
				)))
			}
		};

		tracing::debug!(
			"Comparing large integer strings: left: {}, right: {}",
			left,
			right
		);

		match operator {
			ComparisonOperator::Eq => Ok(left == *right),
			ComparisonOperator::Ne => Ok(left != *right),
			_ => Err(EvaluationError::UnsupportedOperator {
				op: format!(
					"Operator {:?} not supported for i256 string comparison",
					operator
				),
			}),
		}
	}

	fn compare_string(
		&self,
		param_kind: &str,  // "string", "address", "symbol", "bytes"
		param_value: &str, // LHS value
		operator: &ComparisonOperator,
		compare_value: &LiteralValue<'_>, // RHS value
	) -> Result<bool, EvaluationError> {
		let right_str_val = match compare_value {
			LiteralValue::Str(s) => *s,
			LiteralValue::Number(n) => {
				// Allow comparing stringy params with number literals if op is textual
				match operator {
					ComparisonOperator::StartsWith
					| ComparisonOperator::EndsWith
					| ComparisonOperator::Contains => *n,
					_ => {
						return Err(EvaluationError::TypeMismatch(format!(
						"Expected string literal for {} comparison with '{}', found number: {:?}",
						param_kind, param_value, compare_value
					)))
					}
				}
			}
			LiteralValue::Bool(b) => {
				// Allow comparing stringy params with bool literals if op is textual
				match operator {
					ComparisonOperator::StartsWith
					| ComparisonOperator::EndsWith
					| ComparisonOperator::Contains => {
						if *b {
							"true"
						} else {
							"false"
						}
					}
					_ => {
						return Err(EvaluationError::TypeMismatch(format!(
						"Expected string literal for {} comparison with '{}', found boolean: {:?}",
						param_kind, param_value, compare_value
					)))
					}
				}
			}
		};

		// Normalize based on kind
		let left_normalized;
		let right_normalized;

		if param_kind == "address" {
			// Use normalize_address for both sides if it's an address comparison
			// and the operator is Eq or Ne.
			// For other operators like Contains on an address, treat as normal string.
			match operator {
				ComparisonOperator::Eq | ComparisonOperator::Ne => {
					left_normalized = helpers::normalize_address(param_value);
					right_normalized = helpers::normalize_address(right_str_val);
				}
				_ => {
					// StartsWith, EndsWith, Contains for addresses are case-insensitive string ops
					left_normalized = param_value.to_lowercase();
					right_normalized = right_str_val.to_lowercase();
				}
			}
		} else {
			// For "string", "symbol", "bytes", and address with textual ops
			left_normalized = param_value.to_lowercase();
			right_normalized = right_str_val.to_lowercase();
		}

		match operator {
			ComparisonOperator::Eq => Ok(left_normalized == right_normalized),
			ComparisonOperator::Ne => Ok(left_normalized != right_normalized),
			ComparisonOperator::StartsWith => Ok(left_normalized.starts_with(&right_normalized)),
			ComparisonOperator::EndsWith => Ok(left_normalized.ends_with(&right_normalized)),
			ComparisonOperator::Contains => Ok(left_normalized.contains(&right_normalized)),
			_ => Err(EvaluationError::UnsupportedOperator {
				op: format!(
					"Operator {:?} not supported for type {}",
					operator, param_kind
				),
			}),
		}
	}

	fn compare_vec(
		&self,
		param_value: &str, // Comma-separated string for LHS
		operator: &ComparisonOperator,
		compare_value: &LiteralValue<'_>, // RHS
	) -> Result<bool, EvaluationError> {
		let right_str_val = match compare_value {
			LiteralValue::Str(s) => *s,
			LiteralValue::Number(n) => *n, // Allow comparing vec elements with numbers if they parse
			_ => {
				return Err(EvaluationError::TypeMismatch(format!(
					"Expected string or number literal for Vec comparison, found: {:?}",
					compare_value
				)))
			}
		};

		match operator {
			ComparisonOperator::Contains => {
				let values: Vec<&str> = param_value.split(',').map(str::trim).collect();
				Ok(values.contains(&right_str_val.trim()))
			}
			ComparisonOperator::Eq => Ok(param_value == right_str_val), // Exact string match for the whole vec
			ComparisonOperator::Ne => Ok(param_value != right_str_val),
			_ => Err(EvaluationError::UnsupportedOperator {
				op: format!("Operator {:?} not supported for Vec type", operator),
			}),
		}
	}

	/// Compares two JSON strings as maps/objects.
	fn compare_map(
		&self,
		lhs_str: &str,
		operator: &ComparisonOperator,
		rhs_literal: &LiteralValue<'_>,
	) -> Result<bool, EvaluationError> {
		// Parse as JSON
		let lhs_json: serde_json::Value = serde_json::from_str(lhs_str).map_err(|e| {
			EvaluationError::ParseError(format!(
				"Failed to parse LHS value as JSON map/object: {}. Value: '{}'",
				e, lhs_str
			))
		})?;

		match operator {
			ComparisonOperator::Eq | ComparisonOperator::Ne => {
				// RHS should be a string representing a JSON object
				let rhs_str = match rhs_literal {
					LiteralValue::Str(s) => *s,
					_ => {
						return Err(EvaluationError::TypeMismatch(format!(
							"Expected string literal for map comparison, found: {:?}",
							rhs_literal
						)))
					}
				};

				let rhs_json: serde_json::Value = serde_json::from_str(rhs_str).map_err(|e| {
					EvaluationError::ParseError(format!(
						"Failed to parse RHS value as JSON map/object: {}. Value: '{}'",
						e, rhs_str
					))
				})?;

				// TODO: this will only work if key order is guaranteed, double-check
				let result = lhs_json == rhs_json;

				Ok(if *operator == ComparisonOperator::Eq {
					result
				} else {
					!result
				})
			}
			_ => {
				return Err(EvaluationError::UnsupportedOperator {
					op: format!("Operator {:?} not supported for map comparison", operator),
				});
			}
		}
	}
}

impl ConditionEvaluator for StellarConditionEvaluator<'_> {
	fn get_base_param(&self, name: &str) -> Result<(&str, &str), EvaluationError> {
		self.args
			.iter()
			.find(|entry| entry.name == name)
			.map(|entry| (entry.value.as_str(), entry.kind.as_str()))
			.ok_or_else(|| EvaluationError::VariableNotFound(name.to_string()))
	}

	fn get_kind_from_json_value(&self, value: &serde_json::Value) -> String {
		helpers::get_kind_from_value(value)
	}

	fn compare_final_values(
		&self,
		param_type: &str,
		param_value: &str,
		operator: &ComparisonOperator,
		compare_value: &LiteralValue<'_>,
	) -> Result<bool, EvaluationError> {
		match param_type.to_lowercase().as_str() {
			"bool" => self.compare_bool(param_value, operator, compare_value),
			"u32" => self.compare_u32(param_value, operator, compare_value),
			"u64" | "timepoint" | "duration" => {
				self.compare_u64(param_value, operator, compare_value)
			}
			"i32" => self.compare_i32(param_value, operator, compare_value),
			"i64" => self.compare_i64(param_value, operator, compare_value),
			"u128" => self.compare_u128(param_value, operator, compare_value),
			"i128" => self.compare_i128(param_value, operator, compare_value),
			"u256" | "i256" => {
				self.compare_large_int_as_string(param_value, operator, compare_value)
			}
			"vec" => self.compare_vec(param_value, operator, compare_value),
			// handle "object" as potential type from JSON inference
			"map" | "object" => self.compare_map(param_value, operator, compare_value),
			"string" | "symbol" | "address" | "bytes" => self.compare_string(
				param_type.to_ascii_lowercase().as_str(),
				param_value,
				operator,
				compare_value,
			),
			unknown_type => Err(EvaluationError::TypeMismatch(format!(
				"Unknown parameter type: {}",
				unknown_type
			))),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	//////////////////////////////////////////////////////////////////////////////
	// Test cases for compare_values method:
	//////////////////////////////////////////////////////////////////////////////
	#[test]
	fn test_compare_bool() {
		let args: StellarArgs = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		assert!(evaluator
			.compare_bool("true", &ComparisonOperator::Eq, &LiteralValue::Bool(true))
			.unwrap());
		assert!(!evaluator
			.compare_bool("true", &ComparisonOperator::Eq, &LiteralValue::Bool(false))
			.unwrap());

		// Test TypeMismatch for RHS
		let type_mismatch_result = evaluator.compare_bool(
			"true",
			&ComparisonOperator::Eq,
			&LiteralValue::Number("123"),
		);
		assert!(matches!(
			type_mismatch_result,
			Err(EvaluationError::TypeMismatch(_))
		));

		// Test ParseError for LHS
		let parse_error_result = evaluator.compare_bool(
			"notabool",
			&ComparisonOperator::Eq,
			&LiteralValue::Bool(true),
		);
		assert!(matches!(
			parse_error_result,
			Err(EvaluationError::ParseError(_))
		));

		// Test UnsupportedOperator
		let unsupported_op_result = evaluator.compare_bool(
			"true",
			&ComparisonOperator::Gt, // Gt is not supported for bool
			&LiteralValue::Bool(false),
		);
		assert!(
			matches!(
				unsupported_op_result,
				Err(EvaluationError::UnsupportedOperator { .. })
			),
			"Expected UnsupportedOperator, got {:?}",
			unsupported_op_result
		);
	}

	// TODO: add macro to avoid duplicating test cases for u32, i32, u64, i64, u128, i128
	#[test]
	fn test_compare_u64() {
		let args: StellarArgs = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		assert!(evaluator
			.compare_u64("100", &ComparisonOperator::Gt, &LiteralValue::Number("50"))
			.unwrap());
		// Type Mismatch
		assert!(matches!(
			evaluator.compare_u64("100", &ComparisonOperator::Gt, &LiteralValue::Bool(true)),
			Err(EvaluationError::TypeMismatch(_))
		));
		// Parse Error LHS
		assert!(matches!(
			evaluator.compare_u64("abc", &ComparisonOperator::Gt, &LiteralValue::Number("50")),
			Err(EvaluationError::ParseError(_))
		));
		// Parse Error RHS
		assert!(matches!(
			evaluator.compare_u64("100", &ComparisonOperator::Gt, &LiteralValue::Number("xyz")),
			Err(EvaluationError::ParseError(_))
		));
	}

	#[test]
	fn test_compare_i256() {
		let args: StellarArgs = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Eq
		assert!(evaluator
			.compare_large_int_as_string(
				"12345",
				&ComparisonOperator::Eq,
				&LiteralValue::Number("12345")
			)
			.unwrap());
		assert!(evaluator
			.compare_large_int_as_string(
				"12345",
				&ComparisonOperator::Eq,
				&LiteralValue::Str("12345")
			)
			.unwrap());
		assert!(!evaluator
			.compare_large_int_as_string(
				"12345",
				&ComparisonOperator::Eq,
				&LiteralValue::Number("54321")
			)
			.unwrap());

		// Ne
		assert!(evaluator
			.compare_large_int_as_string(
				"12345",
				&ComparisonOperator::Ne,
				&LiteralValue::Number("54321")
			)
			.unwrap());
		assert!(!evaluator
			.compare_large_int_as_string(
				"12345",
				&ComparisonOperator::Ne,
				&LiteralValue::Number("12345")
			)
			.unwrap());

		// Unsupported operator
		assert!(matches!(
			evaluator.compare_large_int_as_string(
				"12345",
				&ComparisonOperator::Gt,
				&LiteralValue::Number("54321")
			),
			Err(EvaluationError::UnsupportedOperator { .. })
		));

		// Type Mismatch RHS
		assert!(matches!(
			evaluator.compare_large_int_as_string(
				"12345",
				&ComparisonOperator::Eq,
				&LiteralValue::Bool(true)
			),
			Err(EvaluationError::TypeMismatch(_))
		));
	}

	#[test]
	fn test_compare_string() {
		let args: StellarArgs = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// String Eq
		assert!(evaluator
			.compare_string(
				"string",
				"hello",
				&ComparisonOperator::Eq,
				&LiteralValue::Str("hello")
			)
			.unwrap());
		// String Contains
		assert!(evaluator
			.compare_string(
				"string",
				"hello world",
				&ComparisonOperator::Contains,
				&LiteralValue::Str("world")
			)
			.unwrap());
		// Address Eq (normalized)
		assert!(evaluator
			.compare_string(
				"address",
				"GABC...", // Assume normalize_address makes it GABC...
				&ComparisonOperator::Eq,
				&LiteralValue::Str("gabc...") // and this too
			)
			.unwrap()); // This depends on normalize_address
	}

	#[test]
	fn test_compare_vec() {
		let args: StellarArgs = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Vec Contains
		assert!(evaluator
			.compare_vec(
				"apple,banana,cherry",
				&ComparisonOperator::Contains,
				&LiteralValue::Str("banana")
			)
			.unwrap());
		// Vec Eq (full string)
		assert!(evaluator
			.compare_vec(
				"apple,banana",
				&ComparisonOperator::Eq,
				&LiteralValue::Str("apple,banana")
			)
			.unwrap());
		// Vec Ne
		assert!(evaluator
			.compare_vec(
				"apple,banana",
				&ComparisonOperator::Ne,
				&LiteralValue::Str("apple,orange")
			)
			.unwrap());
		// Type Mismatch RHS
		assert!(matches!(
			evaluator.compare_vec(
				"apple,banana",
				&ComparisonOperator::Contains,
				&LiteralValue::Bool(true) // Not Str or Number
			),
			Err(EvaluationError::TypeMismatch(_))
		));
	}

	#[test]
	fn test_compare_map() {
		let args: StellarArgs = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		let map_json_lhs = r#"{"name":"John","age":30,"city":"New York"}"#;

		// Map Eq (comparing two JSON strings)
		assert!(evaluator
			.compare_map(
				map_json_lhs,
				&ComparisonOperator::Eq,
				&LiteralValue::Str(r#"{"age":30,"name":"John","city":"New York"}"#) // Order doesn't matter for JSON obj eq
			)
			.unwrap());

		// Map Ne
		assert!(evaluator
			.compare_map(
				map_json_lhs,
				&ComparisonOperator::Ne,
				&LiteralValue::Str(r#"{"name":"Jane","age":30,"city":"New York"}"#)
			)
			.unwrap());

		// Map Contains (key)
		assert!(evaluator
			.compare_map(
				map_json_lhs,
				&ComparisonOperator::Contains, // Checks if "age" is a key
				&LiteralValue::Str("age")
			)
			.unwrap());
		assert!(!evaluator
			.compare_map(
				map_json_lhs,
				&ComparisonOperator::Contains, // "country" is not a key
				&LiteralValue::Str("country")
			)
			.unwrap());

		// Type Mismatch for RHS if not Str
		assert!(matches!(
			evaluator.compare_map(
				map_json_lhs,
				&ComparisonOperator::Eq,
				&LiteralValue::Number("123")
			),
			Err(EvaluationError::TypeMismatch(_))
		));

		// LHS not valid JSON for Contains key
		assert!(matches!(
			evaluator.compare_map(
				"not a json",
				&ComparisonOperator::Contains,
				&LiteralValue::Str("some_key")
			),
			Err(EvaluationError::TypeMismatch(_))
		));
	}

	#[test]
	fn test_compare_bool_case_sensitivity() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test TRUE (uppercase)
		assert!(evaluator
			.compare_bool("TRUE", &ComparisonOperator::Eq, &LiteralValue::Bool(true))
			.is_err());

		// Test False (mixed case)
		assert!(evaluator
			.compare_bool("False", &ComparisonOperator::Eq, &LiteralValue::Bool(false))
			.is_err());

		// Test TRUE == TRUE (both uppercase)
		assert!(evaluator
			.compare_bool("TRUE", &ComparisonOperator::Eq, &LiteralValue::Bool(true))
			.is_err());
	}

	//////////////////////////////////////////////////////////////////////////////
	// Test cases for compare_u64 method:
	//////////////////////////////////////////////////////////////////////////////

	#[test]
	fn test_compare_u64_valid_comparisons() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test greater than
		assert!(evaluator
			.compare_u64("100", &ComparisonOperator::Gt, &LiteralValue::Number("50"))
			.unwrap());
		assert!(!evaluator
			.compare_u64("50", &ComparisonOperator::Gt, &LiteralValue::Number("100"))
			.unwrap());
		assert!(!evaluator
			.compare_u64("100", &ComparisonOperator::Gt, &LiteralValue::Number("100"))
			.unwrap());

		// Test greater than or equal
		assert!(evaluator
			.compare_u64("100", &ComparisonOperator::Gte, &LiteralValue::Number("50"))
			.unwrap());
		assert!(evaluator
			.compare_u64(
				"100",
				&ComparisonOperator::Gte,
				&LiteralValue::Number("100")
			)
			.unwrap());
		assert!(!evaluator
			.compare_u64("50", &ComparisonOperator::Gte, &LiteralValue::Number("100"))
			.unwrap());

		// Test less than
		assert!(evaluator
			.compare_u64("50", &ComparisonOperator::Lt, &LiteralValue::Number("100"))
			.unwrap());
		assert!(!evaluator
			.compare_u64("100", &ComparisonOperator::Lt, &LiteralValue::Number("50"))
			.unwrap());
		assert!(!evaluator
			.compare_u64("100", &ComparisonOperator::Lt, &LiteralValue::Number("100"))
			.unwrap());

		// Test less than or equal
		assert!(evaluator
			.compare_u64("50", &ComparisonOperator::Lte, &LiteralValue::Number("100"))
			.unwrap());
		assert!(evaluator
			.compare_u64(
				"100",
				&ComparisonOperator::Lte,
				&LiteralValue::Number("100")
			)
			.unwrap());
		assert!(!evaluator
			.compare_u64("100", &ComparisonOperator::Lte, &LiteralValue::Number("50"))
			.unwrap());

		// Test equality
		assert!(evaluator
			.compare_u64("100", &ComparisonOperator::Eq, &LiteralValue::Number("100"))
			.unwrap());
		assert!(!evaluator
			.compare_u64("100", &ComparisonOperator::Eq, &LiteralValue::Number("50"))
			.unwrap());

		// Test inequality
		assert!(evaluator
			.compare_u64("100", &ComparisonOperator::Ne, &LiteralValue::Number("50"))
			.unwrap());
		assert!(!evaluator
			.compare_u64("100", &ComparisonOperator::Ne, &LiteralValue::Number("100"))
			.unwrap());
	}

	#[test]
	fn test_compare_u64_invalid_values() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test invalid param_value
		assert!(evaluator
			.compare_u64(
				"not_a_number",
				&ComparisonOperator::Gt,
				&LiteralValue::Number("100")
			)
			.is_err());
		assert!(evaluator
			.compare_u64("", &ComparisonOperator::Gt, &LiteralValue::Number("100"))
			.is_err());
		assert!(evaluator
			.compare_u64(
				"-100",
				&ComparisonOperator::Gt,
				&LiteralValue::Number("100")
			)
			.is_err()); // Negative numbers aren't valid u64
	}
}
