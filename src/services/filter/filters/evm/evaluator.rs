//! This module provides an implementation of the `ConditionEvaluator` trait
//! for evaluating conditions in EVM-based chains.

use super::helpers::{are_same_address, string_to_i256, string_to_u256};
use crate::{
	models::EVMMatchParamEntry,
	services::filter::expression::{
		compare_ordered_values, ComparisonOperator, ConditionEvaluator, EvaluationError,
		LiteralValue,
	},
};
use rust_decimal::Decimal;
use std::str::FromStr;

type EVMArgs = Vec<EVMMatchParamEntry>;

const UNSIGNED_INTEGER_KINDS: &[&str] = &[
	"uint8", "uint16", "uint32", "uint64", "uint128", "uint256", "number",
];

const SIGNED_INTEGER_KINDS: &[&str] = &["int8", "int16", "int32", "int64", "int128", "int256"];

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

		tracing::debug!(
			"Comparing U256: left: {}, op: {:?}, right: {}",
			left,
			operator,
			right
		);

		compare_ordered_values(&left, operator, &right)
	}

	/// Compares potential I256 LHS value with the RHS literal value
	fn compare_i256(
		&self,
		left_str: &str,
		operator: &ComparisonOperator,
		right_literal: &LiteralValue<'_>,
	) -> Result<bool, EvaluationError> {
		let left = string_to_i256(left_str).map_err(|error| {
			EvaluationError::ParseError(format!(
				"Failed to parse LHS value '{}' as I256: {}",
				left_str, error,
			))
		})?;

		let right_str = match right_literal {
			LiteralValue::Number(s) => s, // e.g., "-10", "10", "0x0A" (if string_to_i256 handles hex)
			LiteralValue::Str(s) => s,    // e.g., "'-10'", "'0x0A'"
			_ => {
				return Err(EvaluationError::TypeMismatch(format!(
					"Expected number or string literal for I256 comparison, found: {:?}",
					right_literal
				)));
			}
		};

		let right = string_to_i256(right_str).map_err(|error| {
			EvaluationError::ParseError(format!(
				"Failed to parse RHS value '{}' as I256: {}",
				right_str, error,
			))
		})?;

		tracing::debug!(
			"Comparing I256: left: {}, op: {:?}, right: {}",
			left,
			operator,
			right
		);

		compare_ordered_values(&left, operator, &right)
	}

	/// Compares an EVM address (string) with a literal value based on the operator.
	/// Only supports Eq and Ne operators.
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

	/// Compares a string value with a literal value based on the operator.
	/// Supports Eq, Ne, StartsWith, EndsWith, and Contains operators.
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

	/// Compares an EVM array (represented as a comma-separated string)
	/// with a literal value based on the operator.
	/// Supports Eq, Ne (for the whole string) and Contains (for elements).
	fn compare_array(
		&self,
		lhs_kind: &str, // "uint8[]" or "string[]"
		lhs_str: &str,
		operator: &ComparisonOperator,
		rhs_literal: &LiteralValue<'_>,
	) -> Result<bool, EvaluationError> {
		tracing::debug!(
			"Comparing array: lhs_kind='{}', lhs_str='{}', operator={:?}, rhs_literal={:?}",
			lhs_kind,
			lhs_str,
			operator,
			rhs_literal
		);

		let rhs_str = match rhs_literal {
			LiteralValue::Str(s) => *s,
			LiteralValue::Number(n) => *n,
			LiteralValue::Bool(b) => {
				if *b {
					"true"
				} else {
					"false"
				}
			}
		};

		match operator {
			ComparisonOperator::Eq => Ok(lhs_str == rhs_str),
			ComparisonOperator::Ne => Ok(lhs_str != rhs_str),
			ComparisonOperator::Contains => {
				// Split the LHS comma-separated string into elements
				// Trim whitespace from each element for both LHS and RHS
				let lhs_elements: Vec<&str> = lhs_str.split(',').map(str::trim).collect();
				let trimmed_rhs_str = rhs_str.trim();
				Ok(lhs_elements.iter().any(|&el| el == trimmed_rhs_str))
			}
			_ => Err(EvaluationError::UnsupportedOperator {
				op: format!(
					"Operator {:?} not supported for EVM array (kind: {})",
					operator, lhs_kind
				),
			}),
		}
	}

	/// Compares a fixed-point number (Decimal) with a literal value.
	fn compare_fixed_point(
		&self,
		lhs_str: &str, // LHS value as string (needs parsing)
		operator: &ComparisonOperator,
		rhs_literal: &LiteralValue<'_>,
	) -> Result<bool, EvaluationError> {
		let left_decimal = Decimal::from_str(lhs_str).map_err(|e| {
			EvaluationError::ParseError(format!(
				"Failed to parse LHS value '{}' as Decimal: {}",
				lhs_str, e
			))
		})?;

		// RHS must now be parsed from Number(&str) or Str(&str)
		let rhs_str = match rhs_literal {
			LiteralValue::Number(s) => *s,
			LiteralValue::Str(s) => *s, // If user quoted a numeric string e.g., '123.45'
			_ => {
				return Err(EvaluationError::TypeMismatch(format!(
					"Expected number or string literal for Decimal comparison, found: {:?}",
					rhs_literal
				)));
			}
		};

		let right_decimal = Decimal::from_str(rhs_str).map_err(|e| {
			EvaluationError::ParseError(format!(
				"Failed to parse RHS value '{}' as Decimal: {}",
				rhs_str, e
			))
		})?;

		tracing::debug!(
			"Comparing Decimal: left={}, op={:?}, right={}",
			left_decimal,
			operator,
			right_decimal
		);

		compare_ordered_values(&left_decimal, operator, &right_decimal)
	}

	/// Compares a boolean value (true/false) with a literal value.
	/// Only supports Eq and Ne operators.
	fn compare_boolean(
		&self,
		lhs_value_str: &str,
		operator: &ComparisonOperator,
		rhs_literal: &LiteralValue<'_>,
	) -> Result<bool, EvaluationError> {
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
		let lhs_kind = lhs_kind_str.to_lowercase();

		tracing::debug!(
			"EVM Comparing: lhs_val='{}', lhs_kind='{}', op='{:?}', rhs_lit='{:?}'",
			lhs_value_str,
			lhs_kind_str,
			operator,
			rhs_literal
		);

		if SIGNED_INTEGER_KINDS.contains(&lhs_kind.as_str()) {
			return self.compare_i256(lhs_value_str, operator, rhs_literal);
		}

		if UNSIGNED_INTEGER_KINDS.contains(&lhs_kind.as_str()) {
			return self.compare_u256(lhs_value_str, operator, rhs_literal);
		}

		// Check for EVM comma-separated array kind
		if lhs_kind.ends_with("[]") {
			return self.compare_array(&lhs_kind, lhs_value_str, operator, rhs_literal);
		}

		match lhs_kind.as_str() {
			"fixed" | "ufixed" => self.compare_fixed_point(lhs_value_str, operator, rhs_literal),
			"address" => self.compare_address(lhs_value_str, operator, rhs_literal),
			"string" | "bytes" | "bytes32" => {
				self.compare_string(lhs_value_str, operator, rhs_literal)
			}
			"bool" => self.compare_boolean(lhs_value_str, operator, rhs_literal),
			_ => Err(EvaluationError::TypeMismatch(format!(
				"Unsupported EVM parameter kind for comparison: {}",
				lhs_kind_str
			))),
		}
	}

	fn get_kind_from_json_value(&self, value: &serde_json::Value) -> String {
		match value {
			serde_json::Value::String(s) => {
				let s_lower = s.to_lowercase();
				if s_lower.starts_with("0x")
					&& s.len() == 42
					&& s.chars().skip(2).all(|c| c.is_ascii_hexdigit())
				{
					"address".to_string()
				} else if s_lower.starts_with("0x")
					&& s.chars().skip(2).all(|c| c.is_ascii_hexdigit())
				{
					if s.len() == 66 {
						// 0x + 32 bytes (64 hex chars)
						"bytes32".to_string()
					} else {
						"bytes".to_string()
					}
				// Check if it's a string representation of a decimal
				} else if Decimal::from_str(s).is_ok() && s.contains('.') {
					"fixed".to_string()
				} else {
					"string".to_string()
				}
			}
			serde_json::Value::Number(n) => {
				if n.is_f64() || n.to_string().contains('.') {
					"fixed".to_string()
				} else if n.is_i64() {
					"int64".to_string()
				} else {
					"number".to_string()
				}
			}
			serde_json::Value::Bool(_) => "bool".to_string(),
			serde_json::Value::Array(_) => "vec".to_string(),
			serde_json::Value::Object(_) => "map".to_string(),
			serde_json::Value::Null => "null".to_string(),
		}
	}
}
