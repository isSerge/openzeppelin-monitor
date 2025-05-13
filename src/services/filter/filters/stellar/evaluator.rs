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
		lhs_str: &str,
		operator: &ComparisonOperator,
		rhs_literal: &LiteralValue<'_>,
	) -> Result<bool, EvaluationError> {
		let Ok(left) = lhs_str.parse::<bool>() else {
			return Err(EvaluationError::ParseError(format!(
				"Failed to parse bool parameter value: {}",
				lhs_str
			)));
		};

		let right = match rhs_literal {
			LiteralValue::Bool(b) => *b,
			_ => {
				return Err(EvaluationError::TypeMismatch(format!(
					"Expected bool literal for comparison, found: {:?}",
					rhs_literal
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

	/// Compares two numeric values (u64/i64/u32/i32) using the specified operator.
	fn compare_numeric<T: std::str::FromStr + Ord + std::fmt::Display>(
		&self,
		lhs_str: &str,
		operator: &ComparisonOperator,
		rhs_literal: &LiteralValue<'_>,
	) -> Result<bool, EvaluationError>
	where
		<T as std::str::FromStr>::Err: std::fmt::Debug,
	{
		let left = lhs_str.parse::<T>().map_err(|_| {
			EvaluationError::ParseError(format!(
				"Failed to parse numeric parameter value: {}",
				lhs_str
			))
		})?;

		let rhs_str = match rhs_literal {
			LiteralValue::Number(s) => s,
			_ => {
				return Err(EvaluationError::TypeMismatch(format!(
					"Expected number literal for {} comparison",
					std::any::type_name::<T>()
				)))
			}
		};

		let right = rhs_str.parse::<T>().map_err(|_| {
			EvaluationError::ParseError(format!(
				"Failed to parse comparison value '{}' as {}",
				rhs_str,
				std::any::type_name::<T>()
			))
		})?;

		compare_ordered_values(&left, operator, &right)
	}

	/// Compares two large integers (u256/i256) as strings.
	fn compare_large_int_as_string(
		&self,
		lhs_str: &str,
		operator: &ComparisonOperator,
		rhs_literal: &LiteralValue<'_>,
	) -> Result<bool, EvaluationError> {
		let right = match rhs_literal {
			LiteralValue::Number(s) => s,
			LiteralValue::Str(s) => s,
			_ => {
				return Err(EvaluationError::TypeMismatch(format!(
					"Expected number or string literal for i256 comparison, found: {:?}",
					rhs_literal
				)))
			}
		};

		tracing::debug!(
			"Comparing large integer strings: left: {}, right: {}",
			lhs_str,
			right
		);

		match operator {
			ComparisonOperator::Eq => Ok(lhs_str == *right),
			ComparisonOperator::Ne => Ok(lhs_str != *right),
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
		lhs_kind: &str, // "string", "address", "symbol", "bytes"
		lhs_str: &str,
		operator: &ComparisonOperator,
		rhs_literal: &LiteralValue<'_>,
	) -> Result<bool, EvaluationError> {
		let right_str = match rhs_literal {
			LiteralValue::Str(s) => *s,
			_ => {
				return Err(EvaluationError::TypeMismatch(format!(
					"Expected string literal for {} comparison, found: {:?}",
					lhs_kind, rhs_literal
				)))
			}
		};

		// Normalize based on kind
		let left_normalized;
		let right_normalized;

		let is_address_kind = lhs_kind == "address";
		let is_strict_eq_operator =
			operator == &ComparisonOperator::Eq || operator == &ComparisonOperator::Ne;

		if is_address_kind && is_strict_eq_operator {
			left_normalized = helpers::normalize_address(lhs_str);
			right_normalized = helpers::normalize_address(right_str);
		} else {
			left_normalized = lhs_str.to_lowercase();
			right_normalized = right_str.to_lowercase();
		}

		tracing::debug!(
			"Comparing strings: kind: {}, left: {}, operator: {:?}, right: {}",
			lhs_kind,
			left_normalized,
			operator,
			right_normalized,
		);

		match operator {
			ComparisonOperator::Eq => Ok(left_normalized == right_normalized),
			ComparisonOperator::Ne => Ok(left_normalized != right_normalized),
			ComparisonOperator::StartsWith => Ok(left_normalized.starts_with(&right_normalized)),
			ComparisonOperator::EndsWith => Ok(left_normalized.ends_with(&right_normalized)),
			ComparisonOperator::Contains => Ok(left_normalized.contains(&right_normalized)),
			_ => Err(EvaluationError::UnsupportedOperator {
				op: format!(
					"Operator {:?} not supported for type {}",
					operator, lhs_kind
				),
			}),
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
		lhs_kind: &str,
		lhs_str: &str,
		operator: &ComparisonOperator,
		rhs_literal: &LiteralValue<'_>,
	) -> Result<bool, EvaluationError> {
		match lhs_kind.to_lowercase().as_str() {
			"bool" => self.compare_bool(lhs_str, operator, rhs_literal),
			"u32" => self.compare_numeric::<u32>(lhs_str, operator, rhs_literal),
			"u64" | "timepoint" | "duration" => {
				self.compare_numeric::<u64>(lhs_str, operator, rhs_literal)
			}
			"i32" => self.compare_numeric::<i32>(lhs_str, operator, rhs_literal),
			"i64" => self.compare_numeric::<i64>(lhs_str, operator, rhs_literal),
			"u128" => self.compare_numeric::<u128>(lhs_str, operator, rhs_literal),
			"i128" => self.compare_numeric::<i128>(lhs_str, operator, rhs_literal),
			"u256" | "i256" => self.compare_large_int_as_string(lhs_str, operator, rhs_literal),
			"string" | "symbol" | "address" | "bytes" => self.compare_string(
				lhs_kind.to_ascii_lowercase().as_str(),
				lhs_str,
				operator,
				rhs_literal,
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

	#[test]
	fn test_compare_numeric() {
		let args: StellarArgs = vec![];
		let evaluator = StellarConditionEvaluator::new(&args);

		assert!(evaluator
			.compare_numeric::<u64>("100", &ComparisonOperator::Gt, &LiteralValue::Number("50"))
			.unwrap());
		// Type Mismatch
		assert!(matches!(
			evaluator.compare_numeric::<u64>(
				"100",
				&ComparisonOperator::Gt,
				&LiteralValue::Bool(true)
			),
			Err(EvaluationError::TypeMismatch(_))
		));
		// Parse Error LHS
		assert!(matches!(
			evaluator.compare_numeric::<u64>(
				"abc",
				&ComparisonOperator::Gt,
				&LiteralValue::Number("50")
			),
			Err(EvaluationError::ParseError(_))
		));
		// Parse Error RHS
		assert!(matches!(
			evaluator.compare_numeric::<u64>(
				"100",
				&ComparisonOperator::Gt,
				&LiteralValue::Number("xyz")
			),
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
}
