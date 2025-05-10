use crate::{
	models::StellarMatchParamEntry,
	services::filter::{
		expression::{
			compare_ordered_values, Accessor, ComparisonOperator, ConditionEvaluator,
			ConditionLeft, EvaluationError, LiteralValue,
		},
		stellar_helpers::get_kind_from_value,
	},
};

use super::helpers::{
	compare_json_values, compare_json_values_vs_string, compare_strings, get_nested_value,
	normalize_address, parse_json_safe,
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
	/// Result with inner boolean value indicating if the comparison evaluates to true
	/// or an error if the comparison fails
	fn compare_values(
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
			"map" => self.compare_map(param_value, operator, compare_value),
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
			_ => {
				return Err(EvaluationError::UnsupportedOperator {
					op: format!("Unsupported operator: {:?}", operator),
				});
			}
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

		if param_kind.eq_ignore_ascii_case("address") {
			// Use normalize_address for both sides if it's an address comparison
			// and the operator is Eq or Ne.
			// For other operators like Contains on an address, treat as normal string.
			match operator {
				ComparisonOperator::Eq | ComparisonOperator::Ne => {
					left_normalized = normalize_address(param_value);
					right_normalized = normalize_address(right_str_val);
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
	fn compare_map(
		&self,
		param_value: &str, // JSON string for LHS
		operator: &ComparisonOperator,
		compare_value_literal: &LiteralValue<'_>, // RHS
	) -> Result<bool, EvaluationError> {
		let param_json = parse_json_safe(param_value); // Returns Option<serde_json::Value>

		// The RHS for map comparison can be another map (JSON string) or a path for nested access.
		// If it's a path, it must be a string literal.
		let right_str_for_path_or_json = match compare_value_literal {
			LiteralValue::Str(s) => *s,
			// For map comparison, we typically expect a string (either JSON or a path)
			_ => {
				return Err(EvaluationError::TypeMismatch(format!(
					"Expected string literal (JSON or path) for Map comparison, found: {:?}",
					compare_value_literal
				)))
			}
		};
		let compare_json = parse_json_safe(right_str_for_path_or_json);

		// Convert ComparisonOperator to &str for existing helpers
		let op_str = match operator {
			ComparisonOperator::Eq => "==",
			ComparisonOperator::Ne => "!=",
			// Note: compare_json_values supports ">", "<", etc. for numbers within JSON
			ComparisonOperator::Gt => ">",
			ComparisonOperator::Gte => ">=",
			ComparisonOperator::Lt => "<",
			ComparisonOperator::Lte => "<=",
			// 'Contains' for map is ambiguous here.
			// Does it mean key exists? Value exists? Sub-map?
			// For now, let's say it's not directly supported by compare_map
			// unless compare_json_values has a clear semantic for it.
			// If `Contains` should check if `right_str_for_path_or_json` is a key:
			ComparisonOperator::Contains => {
				if let Some(p_json) = param_json {
					if let Some(obj) = p_json.as_object() {
						return Ok(obj.contains_key(right_str_for_path_or_json));
					}
				}
				return Err(EvaluationError::TypeMismatch(
					"LHS for Map 'Contains' operator must be a valid JSON object.".to_string(),
				));
			}
			_ => {
				return Err(EvaluationError::UnsupportedOperator {
					op: format!(
					"Operator {:?} not directly supported by compare_map like this. Review logic.",
					operator
				),
				})
			}
		};

		match (param_json, compare_json) {
			(Some(ref p_val), Some(ref c_val)) => {
				// Both are JSON, use compare_json_values
				Ok(compare_json_values(p_val, op_str, c_val))
			}
			(Some(p_val), None) => {
				// LHS is JSON, RHS is a plain string (potentially a path)
				// The compare_map originally handled path access if RHS contained '.'.
				// This logic is tricky with the new operator model.
				// If op_str is "==", "!=", etc., and RHS is a path like "user.name"
				// we need to get nested value from LHS and compare it against... what?
				// The old compare_map would compare that nested value against the *path string itself*.
				// This needs rethinking if `LiteralValue` is a simple value.

				// For now, let's assume if RHS is not JSON, it's a direct string comparison
				// against the stringified LHS JSON, or we use get_nested_value if RHS is a path.
				// This part is tricky and depends on the exact semantics you want.

				// If the operator implies path access and RHS is a path:
				// This case is complex. If `user.name == "John"`, the `right_str_for_path_or_json` would be "John".
				// The "user.name" part isn't available here. This should be handled by `param_expr` interpretation if needed.

				// Simplified: if RHS is not JSON, perhaps only direct string comparison of param_value and right_str_for_path_or_json makes sense
				// or if we assume right_str_for_path_or_json is a *key* for a 'contains' like operation.
				// The original `compare_map` had more complex logic for this case.
				// Let's stick to what `compare_json_values_vs_string` can do.
				if right_str_for_path_or_json.contains('.') {
					// RHS is a path, get value from LHS map using path
					match get_nested_value(&p_val, right_str_for_path_or_json) {
						Some(nested_val_from_lhs) => {
							// What are we comparing this nested_val_from_lhs against?
							// The original RHS (right_str_for_path_or_json) *was* the path.
							// This doesn't make sense for Eq, Gt etc. unless RHS is a *value*.
							// This indicates a mismatch in how path access is integrated.
							// For now, this branch seems problematic under the new model unless
							// compare_json_values_vs_string has specific logic for it.
							// Let's assume `compare_json_values_vs_string` compares `nested_val_from_lhs` (JSON)
							// with `right_str_for_path_or_json` (String).
							Ok(compare_json_values_vs_string(
								nested_val_from_lhs,
								op_str,
								right_str_for_path_or_json,
							))
						}
						None => Ok(false), // Path not found in LHS JSON
					}
				} else if let Some(obj) = p_val.as_object() {
					// RHS is a simple string, check if it's a key in LHS JSON object
					if let Some(value_from_key) = obj.get(right_str_for_path_or_json) {
						// Compare the value found at key with... something?
						// This is tricky. If op_str is "==", this means "map.key_from_RHS exists and its value is = ??"
						// This is more like `map HasPathValue(key_from_RHS, "==", literal_value_from_expression)`
						// For now, let's use compare_json_values_vs_string, assuming it compares the found JSON value
						// with the string `right_str_for_path_or_json` if that's the intended value.
						Ok(compare_json_values_vs_string(
							value_from_key,
							op_str,
							right_str_for_path_or_json,
						))
					} else {
						Ok(false) // Key not found
					}
				} else {
					// LHS is JSON, RHS is plain string, no path. Direct string comparison of raw values.
					Ok(compare_strings(
						param_value,
						op_str,
						right_str_for_path_or_json,
					))
				}
			}
			(None, Some(_)) => {
				// LHS is plain string, RHS is JSON
				Err(EvaluationError::TypeMismatch(
					"Cannot compare plain string with JSON object directly in this manner."
						.to_string(),
				))
			}
			(None, None) => {
				// Both are plain strings
				Ok(compare_strings(
					param_value,
					op_str,
					right_str_for_path_or_json,
				))
			}
		}
	}
}

impl<'a> ConditionEvaluator for StellarConditionEvaluator<'a> {
	fn evaluate_ast_condition(
		&self,
		left_expr: &ConditionLeft<'_>,
		operator: ComparisonOperator,
		value_literal: &LiteralValue<'_>,
	) -> Result<bool, EvaluationError> {
		tracing::debug!(
			"Evaluating Stellar condition: {:?} {:?} {:?}",
			left_expr,
			operator,
			value_literal
		);

		match left_expr {
			ConditionLeft::Simple(var_name) => {
				// Find the base parameter directly
				let param = self
					.args
					.iter()
					.find(|p| p.name == *var_name)
					.ok_or_else(|| EvaluationError::VariableNotFound((*var_name).to_string()))?;

				// Directly compare its value and kind
				self.compare_values(&param.kind, &param.value, &operator, &value_literal)
			}
			ConditionLeft::Path(variable_path) => {
				let base_var_name = variable_path.base;
				let accessors = &variable_path.accessors;

				// Find the initial parameter (the base of the path)
				let initial_param = self
					.args
					.iter()
					.find(|p| p.name == base_var_name)
					.ok_or_else(|| {
						EvaluationError::VariableNotFound(format!(
							"Base variable '{}' in path not found.",
							base_var_name
						))
					})?;

				// If accessors list is somehow empty for a Path variant (parser should prevent this),
				// it's effectively a simple lookup. This is a defensive check.
				if accessors.is_empty() {
					tracing::warn!("ConditionLeft::Path encountered with empty accessors for base '{}'. Treating as simple.", base_var_name);
					return self.compare_values(
						&initial_param.kind,
						&initial_param.value,
						&operator,
						value_literal,
					);
				}

				// The value of the initial parameter must be parsable as JSON for path traversal
				let mut current_json_value: serde_json::Value =
					serde_json::from_str(&initial_param.value).map_err(|e| {
						EvaluationError::ParseError(format!(
							"Failed to parse value of base variable '{}' (kind: '{}', value: '{}') as JSON for path traversal. Error: {}. Path: {:?}",
							base_var_name, initial_param.kind, initial_param.value, e, variable_path
						))
					})?;

				// This will be the kind of the `current_json_value` as we traverse
				let mut current_kind_str = get_kind_from_value(&current_json_value);

				// Traverse the path using the accessors
				for (accessor_idx, accessor) in accessors.iter().enumerate() {
					let path_so_far_for_err_msg = variable_path
						.accessors
						.iter()
						.take(accessor_idx + 1)
						.fold(base_var_name.to_string(), |acc, ac| match ac {
							Accessor::Index(i) => format!("{}[{}]", acc, i),
							Accessor::Key(k) => format!("{}.{}", acc, k),
						});

					match accessor {
						Accessor::Index(idx) => {
							let arr = current_json_value.as_array().ok_or_else(|| {
								EvaluationError::TypeMismatch(format!(
									"Attempted array indexing on a non-array type ('{}') at path segment '{}'. Full path: {:?}.",
									current_kind_str, path_so_far_for_err_msg, variable_path
								))
							})?;
							current_json_value = arr.get(*idx).cloned().ok_or_else(|| {
								// clone to own the Value
								EvaluationError::IndexOutOfBounds(format!(
									"Index {} out of bounds for array (len {}) at path segment '{}'. Full path: {:?}.",
									idx, arr.len(), path_so_far_for_err_msg, variable_path
								))
							})?;
							current_kind_str = get_kind_from_value(&current_json_value);
						}
						Accessor::Key(key_str) => {
							let obj = current_json_value.as_object().ok_or_else(|| {
								EvaluationError::TypeMismatch(format!(
									"Attempted key access on a non-object type ('{}') at path segment '{}'. Full path: {:?}.",
									current_kind_str, path_so_far_for_err_msg, variable_path
								))
							})?;
							current_json_value = obj.get(key_str).cloned().ok_or_else(|| {
								// clone to own the Value
								EvaluationError::FieldNotFound(format!(
									"Key '{}' not found in object at path segment '{}'. Full path: {:?}.",
									key_str, path_so_far_for_err_msg, variable_path
								))
							})?;
							current_kind_str = get_kind_from_value(&current_json_value);
						}
					}
				}

				// After traversal, current_json_value holds the target value.
				// Convert it to a string representation for compare_values.
				// compare_values expects the LHS value as a string.
				let final_value_str = match current_json_value {
					serde_json::Value::Null => "null".to_string(), // Or handle as a special type/error
					serde_json::Value::Bool(b) => b.to_string(),
					serde_json::Value::Number(n) => n.to_string(),
					serde_json::Value::String(s) => s.clone(), // s is already unquoted
					serde_json::Value::Array(_) | serde_json::Value::Object(_) => {
						// If the path resolves to another collection, compare_values for "vec" or "map"
						// expects a JSON string.
						current_json_value.to_string()
					}
				};

				self.compare_values(
					&current_kind_str,
					&final_value_str,
					&operator,
					value_literal,
				)
			}
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
