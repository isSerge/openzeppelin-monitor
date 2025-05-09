use crate::{
	models::StellarMatchParamEntry,
	services::filter::{
		expression::{
			compare_ordered_values, ComparisonOperator, ConditionEvaluator, EvaluationError,
			LiteralValue,
		},
		stellar_helpers::{
			compare_json_values, compare_json_values_vs_string, compare_strings,
			get_kind_from_value, get_nested_value, parse_json_safe,
		},
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

	/// Compares values based on their type and operator
	///
	/// # Arguments
	/// * `param_type` - The type of parameter being compared
	/// * `param_value` - The actual value to compare
	/// * `operator` - The comparison operator
	/// * `compare_value` - The value to compare against
	///
	/// # Returns
	/// Boolean indicating if the comparison evaluates to true
	fn compare_values(
		&self,
		param_type: &str,
		param_value: &str,
		operator: &str,
		compare_value: &str,
	) -> bool {
		// Remove quotes from the values to normalize them
		let param_value = param_value.trim_matches('"');
		let compare_value = compare_value.trim_matches('"');

		match param_type {
			"Bool" | "bool" => self.compare_bool(param_value, operator, compare_value),
			"U32" | "u32" => self.compare_u32(param_value, operator, compare_value),
			"U64" | "u64" | "Timepoint" | "timepoint" | "Duration" | "duration" => {
				self.compare_u64(param_value, operator, compare_value)
			}
			"I32" | "i32" => self.compare_i32(param_value, operator, compare_value),
			"I64" | "i64" => self.compare_i64(param_value, operator, compare_value),
			"U128" | "u128" => self.compare_u128(param_value, operator, compare_value),
			"I128" | "i128" => self.compare_i128(param_value, operator, compare_value),
			"U256" | "u256" | "I256" | "i256" => {
				self.compare_i256(param_value, operator, compare_value)
			}
			"Vec" | "vec" => self.compare_vec(param_value, operator, compare_value),
			"Map" | "map" => self.compare_map(param_value, operator, compare_value),
			"String" | "string" | "Symbol" | "symbol" | "Address" | "address" | "Bytes"
			| "bytes" => self.compare_string(param_value, operator, compare_value),
			_ => {
				tracing::warn!("Unsupported parameter type: {}", param_type);
				false
			}
		}
	}

	fn compare_bool(&self, param_value: &str, operator: &str, compare_value: &str) -> bool {
		let Ok(param_value) = param_value.parse::<bool>() else {
			tracing::warn!("Failed to parse bool parameter value: {}", param_value);
			return false;
		};
		let Ok(compare_value) = compare_value.parse::<bool>() else {
			tracing::warn!("Failed to parse bool comparison value: {}", compare_value);
			return false;
		};
		match operator {
			"==" => param_value == compare_value,
			"!=" => param_value != compare_value,
			_ => {
				tracing::warn!("Unsupported operator for bool type: {}", operator);
				false
			}
		}
	}

	fn compare_u64(&self, param_value: &str, operator: &str, compare_value: &str) -> bool {
		let Ok(param_value) = param_value.parse::<u64>() else {
			tracing::warn!("Failed to parse u64 parameter value: {}", param_value);
			return false;
		};
		let Ok(compare_value) = compare_value.parse::<u64>() else {
			tracing::warn!("Failed to parse u64 comparison value: {}", compare_value);
			return false;
		};
		match operator {
			">" => param_value > compare_value,
			">=" => param_value >= compare_value,
			"<" => param_value < compare_value,
			"<=" => param_value <= compare_value,
			"==" => param_value == compare_value,
			"!=" => param_value != compare_value,
			_ => {
				tracing::warn!("Unsupported operator: {}", operator);
				false
			}
		}
	}

	fn compare_u32(&self, param_value: &str, operator: &str, compare_value: &str) -> bool {
		let Ok(param_value) = param_value.parse::<u32>() else {
			tracing::warn!("Failed to parse u32 parameter value: {}", param_value);
			return false;
		};
		let Ok(compare_value) = compare_value.parse::<u32>() else {
			tracing::warn!("Failed to parse u32 comparison value: {}", compare_value);
			return false;
		};
		match operator {
			">" => param_value > compare_value,
			">=" => param_value >= compare_value,
			"<" => param_value < compare_value,
			"<=" => param_value <= compare_value,
			"==" => param_value == compare_value,
			"!=" => param_value != compare_value,
			_ => {
				tracing::warn!("Unsupported operator: {}", operator);
				false
			}
		}
	}

	fn compare_i32(&self, param_value: &str, operator: &str, compare_value: &str) -> bool {
		let Ok(param_value) = param_value.parse::<i32>() else {
			tracing::warn!("Failed to parse i32 parameter value: {}", param_value);
			return false;
		};
		let Ok(compare_value) = compare_value.parse::<i32>() else {
			tracing::warn!("Failed to parse i32 comparison value: {}", compare_value);
			return false;
		};
		match operator {
			">" => param_value > compare_value,
			">=" => param_value >= compare_value,
			"<" => param_value < compare_value,
			"<=" => param_value <= compare_value,
			"==" => param_value == compare_value,
			"!=" => param_value != compare_value,
			_ => {
				tracing::warn!("Unsupported operator: {}", operator);
				false
			}
		}
	}

	fn compare_i64(&self, param_value: &str, operator: &str, compare_value: &str) -> bool {
		let Ok(param_value) = param_value.parse::<i64>() else {
			tracing::warn!("Failed to parse i64 parameter value: {}", param_value);
			return false;
		};
		let Ok(compare_value) = compare_value.parse::<i64>() else {
			tracing::warn!("Failed to parse i64 comparison value: {}", compare_value);
			return false;
		};
		match operator {
			">" => param_value > compare_value,
			">=" => param_value >= compare_value,
			"<" => param_value < compare_value,
			"<=" => param_value <= compare_value,
			"==" => param_value == compare_value,
			"!=" => param_value != compare_value,
			_ => {
				tracing::warn!("Unsupported operator: {}", operator);
				false
			}
		}
	}

	fn compare_u128(&self, param_value: &str, operator: &str, compare_value: &str) -> bool {
		let Ok(param_value) = param_value.parse::<u128>() else {
			tracing::warn!("Failed to parse u128 parameter value: {}", param_value);
			return false;
		};
		let Ok(compare_value) = compare_value.parse::<u128>() else {
			tracing::warn!("Failed to parse u128 comparison value: {}", compare_value);
			return false;
		};
		match operator {
			">" => param_value > compare_value,
			">=" => param_value >= compare_value,
			"<" => param_value < compare_value,
			"<=" => param_value <= compare_value,
			"==" => param_value == compare_value,
			"!=" => param_value != compare_value,
			_ => {
				tracing::warn!("Unsupported operator: {}", operator);
				false
			}
		}
	}

	fn compare_i128(&self, param_value: &str, operator: &str, compare_value: &str) -> bool {
		let Ok(param_value) = param_value.parse::<i128>() else {
			tracing::warn!("Failed to parse i128 parameter value: {}", param_value);
			return false;
		};
		let Ok(compare_value) = compare_value.parse::<i128>() else {
			tracing::warn!("Failed to parse i128 comparison value: {}", compare_value);
			return false;
		};
		match operator {
			">" => param_value > compare_value,
			">=" => param_value >= compare_value,
			"<" => param_value < compare_value,
			"<=" => param_value <= compare_value,
			"==" => param_value == compare_value,
			"!=" => param_value != compare_value,
			_ => {
				tracing::warn!("Unsupported operator: {}", operator);
				false
			}
		}
	}

	fn compare_i256(&self, param_value: &str, operator: &str, compare_value: &str) -> bool {
		match operator {
			"==" => param_value == compare_value,
			"!=" => param_value != compare_value,
			_ => {
				tracing::warn!(
					"Only == and != operators are supported for i256: {}",
					operator
				);
				false
			}
		}
	}

	fn compare_string(&self, param_value: &str, operator: &str, compare_value: &str) -> bool {
		let normalized_param = param_value.trim().to_lowercase();
		let normalized_compare = compare_value.trim().to_lowercase();
		match operator {
			"==" => normalized_param == normalized_compare,
			"!=" => normalized_param != normalized_compare,
			_ => {
				tracing::warn!(
					"Only == and != operators are supported for string types: {}",
					operator
				);
				false
			}
		}
	}

	fn compare_vec(&self, param_value: &str, operator: &str, compare_value: &str) -> bool {
		// Split by comma and trim whitespace
		let values: Vec<&str> = param_value.split(',').map(|s| s.trim()).collect();

		// arguments[0] contains "some_value"
		// arguments[0] == "value1,value2,value3"
		match operator {
			"contains" => values.contains(&compare_value),
			"==" => param_value == compare_value, // For exact array match
			"!=" => param_value != compare_value,
			_ => {
				tracing::warn!(
					"Only contains, == and != operators are supported for vec type: {}",
					operator
				);
				false
			}
		}
	}

	/// Compares two values that might be JSON or plain strings using the specified operator.
	///
	/// # Arguments
	/// * `param_value` - The first value to compare, which could be a JSON string or plain string
	/// * `operator` - The comparison operator ("==", "!=", ">", ">=", "<", "<=")
	/// * `compare_value` - The second value to compare against, which could be a JSON string or
	///   plain string
	///
	/// # Supported Comparison Cases
	/// 1. **JSON vs JSON**: Both values are valid JSON
	///    - Supports equality (==, !=)
	///    - Supports numeric comparisons (>, >=, <, <=) when both values are numbers
	///
	/// 2. **JSON vs String**: First value is JSON, second is plain string
	///    - Supports dot notation to access nested JSON values (e.g., "user.address.city")
	///    - Can check if the string matches a key in a JSON object
	///    - Falls back to direct string comparison if above checks fail
	///
	/// 3. **String vs JSON**: First value is string, second is JSON
	///    - Currently returns false as this is an invalid comparison
	///
	/// 4. **String vs String**: Neither value is valid JSON
	///    - Performs direct string comparison
	///
	/// # Returns
	/// * `bool` - True if the comparison succeeds, false otherwise
	pub fn compare_map(&self, param_value: &str, operator: &str, compare_value: &str) -> bool {
		let param_json = parse_json_safe(param_value);
		let compare_json = parse_json_safe(compare_value);

		match (param_json, compare_json) {
			(Some(ref param_val), Some(ref compare_val)) => {
				compare_json_values(param_val, operator, compare_val)
			}

			(Some(param_val), None) => {
				if compare_value.contains('.') {
					return get_nested_value(&param_val, compare_value)
						.map(|nested_val| {
							compare_json_values_vs_string(nested_val, operator, compare_value)
						})
						.unwrap_or(false);
				}

				if let Some(obj) = param_val.as_object() {
					if let Some(value) = obj.get(compare_value) {
						return compare_json_values_vs_string(value, operator, compare_value);
					}
				}

				compare_strings(param_value, operator, compare_value)
			}

			(None, Some(_)) => {
				tracing::debug!("Invalid comparison: non-JSON value compared against JSON value");
				false
			}

			(None, None) => compare_strings(param_value, operator, compare_value),
		}
	}
}

impl<'a> ConditionEvaluator for StellarConditionEvaluator<'a> {
	fn evaluate_ast_condition(
		&self,
		param_expr: &str,
		operator: ComparisonOperator,
		value: &LiteralValue<'_>,
	) -> Result<bool, EvaluationError> {
		tracing::debug!(
			"Evaluating Stellar condition: {} {:?} {:?}",
			param_expr,
			operator,
			value
		);

		// Find the parameter in args
		let Some(param) = self.args.iter().find(|p| p.name == param_expr) else {
			return Err(EvaluationError::VariableNotFound(param_expr.to_string()));
		};

		// Find the parameter and its type
		if param_expr.contains('[') {
			// Array indexing: arguments[0][0]
			let indices: Vec<usize> = param_expr
				.split('[')
				.skip(1)
				.filter_map(|s| s.trim_end_matches(']').parse::<usize>().ok())
				.collect();

			if indices.len() != 2 || indices[0] >= self.args.len() {
				tracing::debug!("Invalid array indices: {:?}", indices);
				return Ok(false);
			}

			let param = &self.args[indices[0]];
			let array_values: Vec<&str> = param.value.split(',').collect();
			if indices[1] >= array_values.len() {
				tracing::debug!("Array index out of bounds: {}", indices[1]);
				return Ok(false);
			}

			Ok(self.compare_values(
				&param.kind,
				array_values[indices[1]].trim(),
				operator,
				value,
			))
		} else if param_expr.contains('.') {
			// Map access: map.key
			let parts: Vec<&str> = param_expr.split('.').collect();
			if parts.len() != 2 {
				tracing::debug!("Invalid map access format: {}", param_expr);
				return Ok(false);
			}

			let [map_name, key] = [parts[0], parts[1]];

			let Some(param) = self.args.iter().find(|p| p.name == map_name) else {
				tracing::debug!("Map {} not found", map_name);
				return Ok(false);
			};

			let Ok(mut map_value) = serde_json::from_str::<serde_json::Value>(&param.value) else {
				tracing::debug!("Failed to parse map: {}", param.value);
				return Ok(false);
			};

			// Unescape the keys in the map_value
			if let serde_json::Value::Object(ref mut map) = map_value {
				let unescaped_map: serde_json::Map<String, serde_json::Value> = map
					.iter()
					.map(|(k, v)| (k.trim_matches('"').to_string(), v.clone()))
					.collect();
				*map = unescaped_map;
			}

			let Some(key_value) = map_value.get(key) else {
				tracing::debug!("Key {} not found in map", key);
				return Ok(false);
			};

			Ok(self.compare_values(
				&get_kind_from_value(key_value),
				&key_value.to_string(),
				operator,
				value,
			))
		} else {
			// Regular parameter
			let Some(param) = self.args.iter().find(|p| p.name == param_expr) else {
				tracing::warn!("Parameter {} not found", param_expr);
				return Ok(false);
			};

			Ok(self.compare_values(&param.kind, &param.value, operator, value))
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
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test true/false equality
		assert!(evaluator.compare_values("bool", "true", "==", "true"));
		assert!(evaluator.compare_values("Bool", "false", "==", "false"));
		assert!(!evaluator.compare_values("bool", "true", "==", "false"));

		// Test inequality
		assert!(evaluator.compare_values("bool", "true", "!=", "false"));
		assert!(!evaluator.compare_values("bool", "true", "!=", "true"));

		// Test invalid operator
		assert!(!evaluator.compare_values("bool", "true", ">", "false"));

		// Test invalid bool values
		assert!(!evaluator.compare_values("bool", "invalid", "==", "true"));
		assert!(!evaluator.compare_values("bool", "true", "==", "invalid"));
	}

	#[test]
	fn test_compare_integers() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test u32
		assert!(evaluator.compare_values("u32", "100", ">", "50"));
		assert!(evaluator.compare_values("U32", "50", "<", "100"));
		assert!(evaluator.compare_values("u32", "100", "==", "100"));
		assert!(evaluator.compare_values("u32", "50", "!=", "100"));
		assert!(!evaluator.compare_values("u32", "invalid", ">", "50"));

		// Test i32
		assert!(evaluator.compare_values("i32", "-10", "<", "0"));
		assert!(evaluator.compare_values("I32", "0", ">", "-10"));
		assert!(!evaluator.compare_values("i32", "invalid", "<", "0"));

		// Test u64
		assert!(evaluator.compare_values("u64", "1000000", ">", "999999"));
		assert!(evaluator.compare_values("Timepoint", "100", "<", "200"));
		assert!(evaluator.compare_values("duration", "50", "==", "50"));

		// Test i64
		assert!(evaluator.compare_values("i64", "-1000000", "<", "0"));
		assert!(evaluator.compare_values("I64", "0", ">", "-1000000"));

		// Test u128
		assert!(evaluator.compare_values(
			"u128",
			"340282366920938463463374607431768211455",
			"==",
			"340282366920938463463374607431768211455"
		));
		assert!(evaluator.compare_values("U128", "100", "<", "200"));

		// Test i128
		assert!(evaluator.compare_values(
			"i128",
			"-170141183460469231731687303715884105728",
			"<",
			"0"
		));
		assert!(evaluator.compare_values("I128", "0", ">", "-100"));
	}

	#[test]
	fn test_compare_strings() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);
		// Test basic string equality
		assert!(evaluator.compare_values("string", "hello", "==", "hello"));
		assert!(evaluator.compare_values("String", "HELLO", "==", "hello")); // Case insensitive
		assert!(evaluator.compare_values("string", "  hello  ", "==", "hello")); // Trim whitespace

		// Test string inequality
		assert!(evaluator.compare_values("string", "hello", "!=", "world"));
		assert!(!evaluator.compare_values("String", "hello", "!=", "HELLO")); // Case insensitive

		// Test address comparison
		assert!(evaluator.compare_values("address", "0x123", "==", "0x123"));
		assert!(evaluator.compare_values("Address", "0x123", "!=", "0x456"));

		// Test symbol comparison
		assert!(evaluator.compare_values("symbol", "SYM", "==", "sym"));
		assert!(evaluator.compare_values("Symbol", "sym1", "!=", "sym2"));

		// Test invalid operators
		assert!(!evaluator.compare_values("string", "hello", ">", "world"));
		assert!(!evaluator.compare_values("string", "hello", "<", "world"));
	}

	#[test]
	fn test_compare_vectors() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test vector contains
		assert!(evaluator.compare_values("vec", "value1,value2,value3", "contains", "value2"));
		assert!(!evaluator.compare_values("Vec", "value1,value2,value3", "contains", "value4"));

		// Test vector equality
		assert!(evaluator.compare_values("vec", "1,2,3", "==", "1,2,3"));
		assert!(evaluator.compare_values("Vec", "1,2,3", "!=", "1,2,4"));

		// Test invalid operators
		assert!(!evaluator.compare_values("vec", "1,2,3", ">", "1,2,3"));
	}

	#[test]
	fn test_unsupported_type() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test unsupported type
		assert!(!evaluator.compare_values("unsupported_type", "value", "==", "value"));
		assert!(!evaluator.compare_values("float", "1.0", "==", "1.0"));
	}

	//////////////////////////////////////////////////////////////////////////////
	// Test cases for compare_bool method:
	//////////////////////////////////////////////////////////////////////////////

	#[test]
	fn test_compare_bool_valid_equality() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test true == true
		assert!(evaluator.compare_bool("true", "==", "true"));

		// Test false == false
		assert!(evaluator.compare_bool("false", "==", "false"));

		// Test true != false
		assert!(evaluator.compare_bool("true", "!=", "false"));

		// Test false != true
		assert!(evaluator.compare_bool("false", "!=", "true"));

		// Test false == true (should be false)
		assert!(!evaluator.compare_bool("false", "==", "true"));

		// Test true == false (should be false)
		assert!(!evaluator.compare_bool("true", "==", "false"));
	}

	#[test]
	fn test_compare_bool_invalid_values() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test invalid param_value
		assert!(!evaluator.compare_bool("not_a_bool", "==", "true"));

		// Test invalid compare_value
		assert!(!evaluator.compare_bool("true", "==", "not_a_bool"));

		// Test both invalid values
		assert!(!evaluator.compare_bool("invalid1", "==", "invalid2"));

		// Test empty strings
		assert!(!evaluator.compare_bool("", "==", "true"));
		assert!(!evaluator.compare_bool("true", "==", ""));
		assert!(!evaluator.compare_bool("", "==", ""));
	}

	#[test]
	fn test_compare_bool_unsupported_operators() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test greater than operator
		assert!(!evaluator.compare_bool("true", ">", "false"));

		// Test less than operator
		assert!(!evaluator.compare_bool("false", "<", "true"));

		// Test greater than or equal operator
		assert!(!evaluator.compare_bool("true", ">=", "false"));

		// Test less than or equal operator
		assert!(!evaluator.compare_bool("false", "<=", "true"));

		// Test empty operator
		assert!(!evaluator.compare_bool("true", "", "false"));

		// Test invalid operator
		assert!(!evaluator.compare_bool("true", "invalid", "false"));
	}

	#[test]
	fn test_compare_bool_case_sensitivity() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test TRUE (uppercase)
		assert!(!evaluator.compare_bool("TRUE", "==", "true"));

		// Test False (mixed case)
		assert!(!evaluator.compare_bool("False", "==", "false"));

		// Test TRUE == TRUE (both uppercase)
		assert!(!evaluator.compare_bool("TRUE", "==", "TRUE"));
	}

	//////////////////////////////////////////////////////////////////////////////
	// Test cases for compare_u64 method:
	//////////////////////////////////////////////////////////////////////////////

	#[test]
	fn test_compare_u64_valid_comparisons() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test greater than
		assert!(evaluator.compare_u64("100", ">", "50"));
		assert!(!evaluator.compare_u64("50", ">", "100"));
		assert!(!evaluator.compare_u64("100", ">", "100"));

		// Test greater than or equal
		assert!(evaluator.compare_u64("100", ">=", "50"));
		assert!(evaluator.compare_u64("100", ">=", "100"));
		assert!(!evaluator.compare_u64("50", ">=", "100"));

		// Test less than
		assert!(evaluator.compare_u64("50", "<", "100"));
		assert!(!evaluator.compare_u64("100", "<", "50"));
		assert!(!evaluator.compare_u64("100", "<", "100"));

		// Test less than or equal
		assert!(evaluator.compare_u64("50", "<=", "100"));
		assert!(evaluator.compare_u64("100", "<=", "100"));
		assert!(!evaluator.compare_u64("100", "<=", "50"));

		// Test equality
		assert!(evaluator.compare_u64("100", "==", "100"));
		assert!(!evaluator.compare_u64("100", "==", "50"));

		// Test inequality
		assert!(evaluator.compare_u64("100", "!=", "50"));
		assert!(!evaluator.compare_u64("100", "!=", "100"));
	}

	#[test]
	fn test_compare_u64_invalid_values() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test invalid param_value
		assert!(!evaluator.compare_u64("not_a_number", ">", "100"));
		assert!(!evaluator.compare_u64("", ">", "100"));
		assert!(!evaluator.compare_u64("-100", ">", "100")); // Negative numbers aren't valid u64

		// Test invalid compare_value
		assert!(!evaluator.compare_u64("100", ">", "not_a_number"));
		assert!(!evaluator.compare_u64("100", ">", ""));
		assert!(!evaluator.compare_u64("100", ">", "-100")); // Negative numbers aren't valid u64

		// Test values exceeding u64::MAX
		assert!(!evaluator.compare_u64("18446744073709551616", ">", "100")); // u64::MAX + 1
		assert!(!evaluator.compare_u64("100", ">", "18446744073709551616")); // u64::MAX + 1
	}

	#[test]
	fn test_compare_u64_invalid_operators() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test unsupported operators
		assert!(!evaluator.compare_u64("100", "<<", "50")); // Bit shift operator
		assert!(!evaluator.compare_u64("100", "contains", "50")); // String operator
		assert!(!evaluator.compare_u64("100", "", "50")); // Empty operator
		assert!(!evaluator.compare_u64("100", "invalid", "50")); // Invalid operator
	}

	#[test]
	fn test_compare_u64_boundary_values() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);
		let max = u64::MAX.to_string();
		let zero = "0";

		// Test with u64::MAX
		assert!(evaluator.compare_u64(&max, "==", &max));
		assert!(evaluator.compare_u64(&max, ">=", zero));
		assert!(evaluator.compare_u64(zero, "<=", &max));
		assert!(!evaluator.compare_u64(&max, "<", &max));
		assert!(!evaluator.compare_u64(&max, ">", &max));

		// Test with zero
		assert!(evaluator.compare_u64(zero, "==", zero));
		assert!(evaluator.compare_u64(zero, "<=", zero));
		assert!(evaluator.compare_u64(zero, ">=", zero));
		assert!(!evaluator.compare_u64(zero, "<", zero));
		assert!(!evaluator.compare_u64(zero, ">", zero));
	}

	//////////////////////////////////////////////////////////////////////////////
	// Test cases for compare_i32 method:
	//////////////////////////////////////////////////////////////////////////////

	#[test]
	fn test_compare_i32_valid_comparisons() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test greater than
		assert!(evaluator.compare_i32("100", ">", "50"));
		assert!(!evaluator.compare_i32("50", ">", "100"));
		assert!(!evaluator.compare_i32("100", ">", "100"));

		// Test greater than or equal
		assert!(evaluator.compare_i32("100", ">=", "50"));
		assert!(evaluator.compare_i32("100", ">=", "100"));
		assert!(!evaluator.compare_i32("50", ">=", "100"));

		// Test less than
		assert!(evaluator.compare_i32("50", "<", "100"));
		assert!(!evaluator.compare_i32("100", "<", "50"));
		assert!(!evaluator.compare_i32("100", "<", "100"));

		// Test less than or equal
		assert!(evaluator.compare_i32("50", "<=", "100"));
		assert!(evaluator.compare_i32("100", "<=", "100"));
		assert!(!evaluator.compare_i32("100", "<=", "50"));

		// Test equality
		assert!(evaluator.compare_i32("100", "==", "100"));
		assert!(!evaluator.compare_i32("100", "==", "50"));

		// Test inequality
		assert!(evaluator.compare_i32("100", "!=", "50"));
		assert!(!evaluator.compare_i32("100", "!=", "100"));
	}

	#[test]
	fn test_compare_i32_negative_numbers() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test negative numbers
		assert!(evaluator.compare_i32("-100", ">", "-200"));
		assert!(evaluator.compare_i32("-200", "<", "-100"));
		assert!(evaluator.compare_i32("-100", "==", "-100"));
		assert!(evaluator.compare_i32("0", ">", "-100"));
		assert!(evaluator.compare_i32("-100", "<", "0"));
	}

	#[test]
	fn test_compare_i32_invalid_values() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test invalid param_value
		assert!(!evaluator.compare_i32("not_a_number", ">", "100"));
		assert!(!evaluator.compare_i32("", ">", "100"));
		assert!(!evaluator.compare_i32("2147483648", ">", "100")); // i32::MAX + 1

		// Test invalid compare_value
		assert!(!evaluator.compare_i32("100", ">", "not_a_number"));
		assert!(!evaluator.compare_i32("100", ">", ""));
		assert!(!evaluator.compare_i32("100", ">", "2147483648")); // i32::MAX + 1

		// Test floating point numbers (invalid for i32)
		assert!(!evaluator.compare_i32("100.5", ">", "100"));
		assert!(!evaluator.compare_i32("100", ">", "99.9"));
	}

	#[test]
	fn test_compare_i32_boundary_values() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test i32::MAX and i32::MIN
		assert!(evaluator.compare_i32("2147483647", ">", "0")); // i32::MAX
		assert!(evaluator.compare_i32("-2147483648", "<", "0")); // i32::MIN
		assert!(evaluator.compare_i32("2147483647", "==", "2147483647")); // i32::MAX == i32::MAX
		assert!(evaluator.compare_i32("-2147483648", "==", "-2147483648")); // i32::MIN == i32::MIN
		assert!(evaluator.compare_i32("2147483647", ">", "-2147483648")); // i32::MAX > i32::MIN
	}

	#[test]
	fn test_compare_i32_invalid_operators() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test unsupported operators
		assert!(!evaluator.compare_i32("100", "<<", "50")); // Bit shift operator
		assert!(!evaluator.compare_i32("100", "contains", "50")); // String operator
		assert!(!evaluator.compare_i32("100", "", "50")); // Empty operator
		assert!(!evaluator.compare_i32("100", "&", "50")); // Bitwise operator
		assert!(!evaluator.compare_i32("100", "===", "50")); // JavaScript-style equality
	}

	//////////////////////////////////////////////////////////////////////////////
	// Test cases for compare_u32 method:
	//////////////////////////////////////////////////////////////////////////////

	#[test]
	fn test_compare_u32_valid_comparisons() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test greater than
		assert!(evaluator.compare_u32("100", ">", "50"));
		assert!(!evaluator.compare_u32("50", ">", "100"));
		assert!(!evaluator.compare_u32("100", ">", "100"));

		// Test greater than or equal
		assert!(evaluator.compare_u32("100", ">=", "50"));
		assert!(evaluator.compare_u32("100", ">=", "100"));
		assert!(!evaluator.compare_u32("50", ">=", "100"));

		// Test less than
		assert!(evaluator.compare_u32("50", "<", "100"));
		assert!(!evaluator.compare_u32("100", "<", "50"));
		assert!(!evaluator.compare_u32("100", "<", "100"));

		// Test less than or equal
		assert!(evaluator.compare_u32("50", "<=", "100"));
		assert!(evaluator.compare_u32("100", "<=", "100"));
		assert!(!evaluator.compare_u32("100", "<=", "50"));

		// Test equality
		assert!(evaluator.compare_u32("100", "==", "100"));
		assert!(!evaluator.compare_u32("100", "==", "50"));

		// Test inequality
		assert!(evaluator.compare_u32("100", "!=", "50"));
		assert!(!evaluator.compare_u32("100", "!=", "100"));
	}

	#[test]
	fn test_compare_u32_boundary_values() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test with u32::MAX
		assert!(evaluator.compare_u32(&u32::MAX.to_string(), ">", "0"));
		assert!(evaluator.compare_u32("0", "<", &u32::MAX.to_string()));
		assert!(evaluator.compare_u32(&u32::MAX.to_string(), "==", &u32::MAX.to_string()));

		// Test with u32::MIN (0)
		assert!(evaluator.compare_u32("0", "==", "0"));
		assert!(evaluator.compare_u32("1", ">", "0"));
		assert!(evaluator.compare_u32("0", "<", "1"));
	}

	#[test]
	fn test_compare_u32_invalid_values() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test invalid param_value
		assert!(!evaluator.compare_u32("not_a_number", ">", "100"));
		assert!(!evaluator.compare_u32("", ">", "100"));
		assert!(!evaluator.compare_u32("-100", ">", "100")); // Negative numbers aren't valid u32

		// Test invalid compare_value
		assert!(!evaluator.compare_u32("100", ">", "not_a_number"));
		assert!(!evaluator.compare_u32("100", ">", ""));
		assert!(!evaluator.compare_u32("100", ">", "-100")); // Negative numbers aren't valid u32

		// Test values exceeding u32::MAX
		assert!(!evaluator.compare_u32("4294967296", ">", "100")); // u32::MAX + 1
		assert!(!evaluator.compare_u32("100", ">", "4294967296")); // u32::MAX + 1

		// Test floating point numbers
		assert!(!evaluator.compare_u32("100.5", ">", "100"));
		assert!(!evaluator.compare_u32("100", ">", "99.9"));
	}

	#[test]
	fn test_compare_u32_invalid_operators() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test unsupported operators
		assert!(!evaluator.compare_u32("100", "invalid", "50"));
		assert!(!evaluator.compare_u32("100", "", "50"));
		assert!(!evaluator.compare_u32("100", ">>", "50"));
		assert!(!evaluator.compare_u32("100", "=", "50"));
		assert!(!evaluator.compare_u32("100", "===", "50"));
	}

	//////////////////////////////////////////////////////////////////////////////
	// Test cases for compare_i64 method:
	//////////////////////////////////////////////////////////////////////////////

	#[test]
	fn test_compare_i64_valid_comparisons() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test greater than
		assert!(evaluator.compare_i64("100", ">", "50"));
		assert!(!evaluator.compare_i64("50", ">", "100"));
		assert!(!evaluator.compare_i64("100", ">", "100"));

		// Test greater than or equal
		assert!(evaluator.compare_i64("100", ">=", "50"));
		assert!(evaluator.compare_i64("100", ">=", "100"));
		assert!(!evaluator.compare_i64("50", ">=", "100"));

		// Test less than
		assert!(evaluator.compare_i64("50", "<", "100"));
		assert!(!evaluator.compare_i64("100", "<", "50"));
		assert!(!evaluator.compare_i64("100", "<", "100"));

		// Test less than or equal
		assert!(evaluator.compare_i64("50", "<=", "100"));
		assert!(evaluator.compare_i64("100", "<=", "100"));
		assert!(!evaluator.compare_i64("100", "<=", "50"));

		// Test equality
		assert!(evaluator.compare_i64("100", "==", "100"));
		assert!(!evaluator.compare_i64("100", "==", "50"));

		// Test inequality
		assert!(evaluator.compare_i64("100", "!=", "50"));
		assert!(!evaluator.compare_i64("100", "!=", "100"));
	}

	#[test]
	fn test_compare_i64_negative_numbers() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test negative numbers
		assert!(evaluator.compare_i64("-100", ">", "-200"));
		assert!(evaluator.compare_i64("-200", "<", "-100"));
		assert!(evaluator.compare_i64("-100", "==", "-100"));
		assert!(evaluator.compare_i64("-100", "!=", "-200"));
		assert!(evaluator.compare_i64("-100", ">=", "-100"));
		assert!(evaluator.compare_i64("-100", "<=", "-100"));

		// Test negative vs positive
		assert!(evaluator.compare_i64("-100", "<", "100"));
		assert!(evaluator.compare_i64("100", ">", "-100"));
		assert!(!evaluator.compare_i64("-100", "==", "100"));
	}

	#[test]
	fn test_compare_i64_invalid_values() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test invalid param_value
		assert!(!evaluator.compare_i64("not_a_number", ">", "100"));
		assert!(!evaluator.compare_i64("", ">", "100"));
		assert!(!evaluator.compare_i64("9223372036854775808", ">", "100")); // i64::MAX + 1

		// Test invalid compare_value
		assert!(!evaluator.compare_i64("100", ">", "not_a_number"));
		assert!(!evaluator.compare_i64("100", ">", ""));
		assert!(!evaluator.compare_i64("100", ">", "9223372036854775808")); // i64::MAX + 1

		// Test floating point numbers (invalid for i64)
		assert!(!evaluator.compare_i64("100.5", ">", "100"));
		assert!(!evaluator.compare_i64("100", ">", "99.9"));
	}

	#[test]
	fn test_compare_i64_boundary_values() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test with i64::MAX and i64::MIN
		assert!(evaluator.compare_i64("9223372036854775807", ">", "0"));
		assert!(evaluator.compare_i64("-9223372036854775808", "<", "0"));
		assert!(evaluator.compare_i64("9223372036854775807", "==", "9223372036854775807"));
		assert!(evaluator.compare_i64("-9223372036854775808", "==", "-9223372036854775808"));
		assert!(evaluator.compare_i64("9223372036854775807", ">=", "9223372036854775807"));
		assert!(evaluator.compare_i64("-9223372036854775808", "<=", "-9223372036854775808"));
	}

	#[test]
	fn test_compare_i64_invalid_operators() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test unsupported operators
		assert!(!evaluator.compare_i64("100", "<<", "50"));
		assert!(!evaluator.compare_i64("100", "contains", "50"));
		assert!(!evaluator.compare_i64("100", "", "50"));
		assert!(!evaluator.compare_i64("100", "invalid", "50"));
	}

	#[test]
	fn test_compare_u128_valid_comparisons() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test basic comparisons
		assert!(evaluator.compare_u128("100", ">", "50"));
		assert!(evaluator.compare_u128("100", ">=", "50"));
		assert!(evaluator.compare_u128("50", "<", "100"));
		assert!(evaluator.compare_u128("50", "<=", "100"));
		assert!(evaluator.compare_u128("100", "==", "100"));
		assert!(evaluator.compare_u128("100", "!=", "50"));

		// Test equality edge cases
		assert!(evaluator.compare_u128("0", "==", "0"));
		assert!(evaluator.compare_u128(
			"340282366920938463463374607431768211455",
			"==",
			"340282366920938463463374607431768211455"
		)); // max u128

		// Test boundary values
		assert!(evaluator.compare_u128("0", "<=", "0"));
		assert!(evaluator.compare_u128(
			"340282366920938463463374607431768211455",
			">=",
			"340282366920938463463374607431768211455"
		));

		// Test false conditions
		assert!(!evaluator.compare_u128("50", ">", "100"));
		assert!(!evaluator.compare_u128("100", "<", "50"));
		assert!(!evaluator.compare_u128("100", "==", "50"));
		assert!(!evaluator.compare_u128("100", "!=", "100"));
	}

	#[test]
	fn test_compare_u128_invalid_inputs() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test invalid number formats
		assert!(!evaluator.compare_u128("not_a_number", ">", "100"));
		assert!(!evaluator.compare_u128("100", ">", "not_a_number"));
		assert!(!evaluator.compare_u128("", ">", "100"));
		assert!(!evaluator.compare_u128("100", ">", ""));

		// Test negative numbers (invalid for u128)
		assert!(!evaluator.compare_u128("-100", ">", "100"));
		assert!(!evaluator.compare_u128("100", ">", "-100"));

		// Test invalid operator
		assert!(!evaluator.compare_u128("100", "invalid_operator", "50"));
		assert!(!evaluator.compare_u128("100", "", "50"));
	}

	#[test]
	fn test_compare_i128_valid_comparisons() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test basic comparisons
		assert!(evaluator.compare_i128("100", ">", "50"));
		assert!(evaluator.compare_i128("100", ">=", "50"));
		assert!(evaluator.compare_i128("50", "<", "100"));
		assert!(evaluator.compare_i128("50", "<=", "100"));
		assert!(evaluator.compare_i128("100", "==", "100"));
		assert!(evaluator.compare_i128("100", "!=", "50"));

		// Test negative numbers
		assert!(evaluator.compare_i128("-100", "<", "0"));
		assert!(evaluator.compare_i128("0", ">", "-100"));
		assert!(evaluator.compare_i128("-100", "==", "-100"));
		assert!(evaluator.compare_i128("-50", ">", "-100"));

		// Test equality edge cases
		assert!(evaluator.compare_i128("0", "==", "0"));
		assert!(evaluator.compare_i128(
			"-170141183460469231731687303715884105728",
			"==",
			"-170141183460469231731687303715884105728"
		)); // min i128
		assert!(evaluator.compare_i128(
			"170141183460469231731687303715884105727",
			"==",
			"170141183460469231731687303715884105727"
		)); // max i128

		// Test false conditions
		assert!(!evaluator.compare_i128("50", ">", "100"));
		assert!(!evaluator.compare_i128("-100", ">", "0"));
		assert!(!evaluator.compare_i128("100", "==", "-100"));
		assert!(!evaluator.compare_i128("-100", "!=", "-100"));
	}

	#[test]
	fn test_compare_i128_invalid_inputs() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);
		// Test invalid number formats
		assert!(!evaluator.compare_i128("not_a_number", ">", "100"));
		assert!(!evaluator.compare_i128("100", ">", "not_a_number"));
		assert!(!evaluator.compare_i128("", ">", "100"));
		assert!(!evaluator.compare_i128("100", ">", ""));

		// Test numbers exceeding i128 bounds
		assert!(!evaluator.compare_i128("170141183460469231731687303715884105728", ">", "0")); // > max i128
		assert!(!evaluator.compare_i128("-170141183460469231731687303715884105729", "<", "0")); // < min i128

		// Test invalid operator
		assert!(!evaluator.compare_i128("100", "invalid_operator", "50"));
		assert!(!evaluator.compare_i128("100", "", "50"));
	}

	// Tests for compare_i256
	#[test]
	fn test_compare_i256() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test equality operator
		assert!(evaluator.compare_i256("12345", "==", "12345"));
		assert!(!evaluator.compare_i256("12345", "==", "54321"));

		// Test inequality operator
		assert!(evaluator.compare_i256("12345", "!=", "54321"));
		assert!(!evaluator.compare_i256("12345", "!=", "12345"));

		// Test unsupported operators
		assert!(!evaluator.compare_i256("12345", ">", "54321"));
		assert!(!evaluator.compare_i256("12345", "<", "54321"));
		assert!(!evaluator.compare_i256("12345", ">=", "54321"));
		assert!(!evaluator.compare_i256("12345", "<=", "54321"));

		// Test with large numbers
		assert!(evaluator.compare_i256(
			"115792089237316195423570985008687907853269984665640564039457584007913129639935",
			"==",
			"115792089237316195423570985008687907853269984665640564039457584007913129639935"
		));
		assert!(evaluator.compare_i256(
			"115792089237316195423570985008687907853269984665640564039457584007913129639935",
			"!=",
			"0"
		));
	}

	// Tests for compare_string
	#[test]
	fn test_compare_string() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);
		// Test basic equality
		assert!(evaluator.compare_string("hello", "==", "hello"));
		assert!(!evaluator.compare_string("hello", "==", "world"));

		// Test case insensitivity
		assert!(evaluator.compare_string("Hello", "==", "hello"));
		assert!(evaluator.compare_string("HELLO", "==", "hello"));
		assert!(evaluator.compare_string("HeLLo", "==", "hEllO"));

		// Test whitespace trimming
		assert!(evaluator.compare_string("  hello  ", "==", "hello"));
		assert!(evaluator.compare_string("hello", "==", "  hello  "));
		assert!(evaluator.compare_string("  hello  ", "==", "  hello  "));

		// Test inequality
		assert!(evaluator.compare_string("hello", "!=", "world"));
		assert!(!evaluator.compare_string("hello", "!=", "hello"));
		assert!(!evaluator.compare_string("Hello", "!=", "hello"));

		// Test empty strings
		assert!(evaluator.compare_string("", "==", ""));
		assert!(evaluator.compare_string("  ", "==", ""));
		assert!(evaluator.compare_string("hello", "!=", ""));

		// Test unsupported operators
		assert!(!evaluator.compare_string("hello", ">", "world"));
		assert!(!evaluator.compare_string("hello", "<", "world"));
		assert!(!evaluator.compare_string("hello", ">=", "world"));
		assert!(!evaluator.compare_string("hello", "<=", "world"));
	}

	// Tests for compare_vec
	#[test]
	fn test_compare_vec() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test contains operator
		assert!(evaluator.compare_vec("value1,value2,value3", "contains", "value2"));
		assert!(!evaluator.compare_vec("value1,value2,value3", "contains", "value4"));

		// Test with whitespace
		assert!(evaluator.compare_vec("value1, value2, value3", "contains", "value2"));
		assert!(evaluator.compare_vec("value1,  value2  ,value3", "contains", "value2"));

		// Test exact equality
		assert!(evaluator.compare_vec("value1,value2,value3", "==", "value1,value2,value3"));
		assert!(!evaluator.compare_vec("value1,value2,value3", "==", "value1,value2"));
		assert!(!evaluator.compare_vec("value1,value2", "==", "value1,value2,value3"));

		// Test inequality
		assert!(evaluator.compare_vec("value1,value2,value3", "!=", "value1,value2"));
		assert!(!evaluator.compare_vec("value1,value2,value3", "!=", "value1,value2,value3"));

		// Test empty vectors
		assert!(evaluator.compare_vec("", "==", ""));
		assert!(!evaluator.compare_vec("", "contains", "value1"));
		assert!(evaluator.compare_vec("value1", "!=", ""));

		// Test single value
		assert!(evaluator.compare_vec("value1", "contains", "value1"));
		assert!(evaluator.compare_vec("value1", "==", "value1"));

		// Test unsupported operators
		assert!(!evaluator.compare_vec("value1,value2,value3", ">", "value1"));
		assert!(!evaluator.compare_vec("value1,value2,value3", "<", "value1"));
		assert!(!evaluator.compare_vec("value1,value2,value3", ">=", "value1"));
		assert!(!evaluator.compare_vec("value1,value2,value3", "<=", "value1"));
	}

	//////////////////////////////////////////////////////////////////////////////
	// Test cases for compare_map method:
	//////////////////////////////////////////////////////////////////////////////

	#[test]
	fn test_compare_map_json_vs_json() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test equality comparisons
		assert!(evaluator.compare_map(r#"{"a": 1}"#, "==", r#"{"a": 1}"#));
		assert!(!evaluator.compare_map(r#"{"a": 1}"#, "==", r#"{"a": 2}"#));
		assert!(evaluator.compare_map(r#"{"a": 1}"#, "!=", r#"{"a": 2}"#));

		// Test numeric comparisons
		assert!(evaluator.compare_map("42", ">", "10"));
		assert!(evaluator.compare_map("10", "<", "42"));
		assert!(evaluator.compare_map("42", ">=", "42"));
		assert!(evaluator.compare_map("42", "<=", "42"));
		assert!(!evaluator.compare_map("10", ">", "42"));

		// Test invalid numeric comparisons
		assert!(!evaluator.compare_map(r#"{"a": "string"}"#, ">", r#"{"b": 42}"#));
	}

	#[test]
	fn test_compare_map_string_vs_json() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// This case should always return false
		assert!(!evaluator.compare_map("plain string", "==", r#"{"any": "json"}"#));
	}

	#[test]
	fn test_compare_map_string_vs_string() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test basic string comparisons
		assert!(evaluator.compare_map("hello", "==", "hello"));
		assert!(evaluator.compare_map("hello", "!=", "world"));
		assert!(!evaluator.compare_map("hello", "==", "world"));

		// Test case sensitivity
		assert!(!evaluator.compare_map("Hello", "==", "hello"));

		// Test with spaces and special characters
		assert!(evaluator.compare_map("hello world", "==", "hello world"));
		assert!(evaluator.compare_map("special!@#$", "==", "special!@#$"));
	}

	#[test]
	fn test_compare_map_edge_cases() {
		let args = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		// Test empty strings
		assert!(evaluator.compare_map("", "==", ""));
		assert!(!evaluator.compare_map("", "==", "non-empty"));

		// Test invalid JSON
		assert!(!evaluator.compare_map("{invalid json}", "==", "{}"));

		// Test unsupported operators
		assert!(!evaluator.compare_map(r#"{"a": 1}"#, "invalid_operator", r#"{"a": 1}"#));

		// Test with whitespace
		assert!(evaluator.compare_map(" hello ", "==", " hello "));

		// Test with null JSON values
		assert!(evaluator.compare_map(r#"{"a": null}"#, "==", r#"{"a": null}"#));
	}
}
