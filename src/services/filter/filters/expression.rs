//! Shared logic for evaluating logical expressions

// TODO: move here, not used anywhere else
use crate::utils::split_expression;
use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
	// Matches "OR" with case-insensitivity and flexible whitespace
	static ref RE_OR: Regex = Regex::new(r"(?i)\s+OR\s+").unwrap();
	// Matches "AND" with case-insensitivity and flexible whitespace
	static ref RE_AND: Regex = Regex::new(r"(?i)\s+AND\s+").unwrap();
}

// TODO: add documentation
// TODO: consider returning a Result instead of a bool
pub fn evaluate_expression_core<F>(expression: &str, mut eval_single_condition: F) -> bool
where
	F: FnMut(&str, &str, &str) -> bool,
{
	// Trim whitespace from the overall expression
	let expression = expression.trim();
	if expression.is_empty() {
		tracing::debug!("evaluate_expression_core called with empty expression.");
		return false;
	}

	// Split by OR to get highest level conditions
	let or_conditions: Vec<&str> = RE_OR.split(expression).collect();

	// Check for invalid structure at OR level (e.g., "A OR OR B", " OR B", "A OR ")
	if or_conditions.len() > 1 && // Only check if OR was actually found
		(or_conditions.first().map_or(false, |s| s.trim().is_empty()) ||
		 or_conditions.last().map_or(false, |s| s.trim().is_empty()))
	{
		tracing::warn!("evaluate_expression_core: Invalid expression structure due to leading/trailing OR. Expression: '{}'", expression);
		return false;
	}
	// Check for completely empty intermediate parts (e.g. "A OR OR B")
	if or_conditions
		.iter()
		.skip(1)
		.rev()
		.skip(1)
		.any(|s| s.trim().is_empty())
	{
		tracing::warn!("evaluate_expression_core: Invalid expression structure due to double OR or empty OR part. Expression: '{}'", expression);
		return false;
	}

	// For OR logic, any condition being true makes the whole expression true
	for or_condition in or_conditions {
		let trimmed_or_condition = or_condition.trim();

		// Split each OR condition by AND
		let and_conditions: Vec<&str> = RE_AND.split(trimmed_or_condition).collect();

		tracing::trace!(
			"evaluate_expression_core: RAND conditions for OR part '{}': {:?}",
			trimmed_or_condition,
			and_conditions
		);

		// Check for invalid structure at AND level (e.g., "A AND AND B", " AND B", "A AND ")
		if and_conditions.len() > 1 && // Only check if AND was found
            (and_conditions.first().map_or(false, |s| s.trim().is_empty()) ||
             and_conditions.last().map_or(false, |s| s.trim().is_empty()))
		{
			tracing::warn!("evaluate_expression_core: Invalid AND group structure due to leading/trailing AND. Group: '{}'. Treating this OR branch as false.", trimmed_or_condition);
			continue; // This OR branch is invalid, try the next one.
		}
		// Check for completely empty intermediate parts (e.g. "A AND AND B")
		if and_conditions
			.iter()
			.skip(1)
			.rev()
			.skip(1)
			.any(|s| s.trim().is_empty())
		{
			tracing::warn!("evaluate_expression_core: Invalid expression structure due to double AND or empty AND part. Group: '{}'. Treating this OR branch as false.", trimmed_or_condition);
			continue; // This OR branch is invalid
		}

		// Evaluate all valid AND conditions within this OR branch
		let mut current_and_group_result = true; // Assume true unless a condition fails

		for condition_str in and_conditions {
			// Remove any surrounding parentheses and trim
			let clean_condition = condition_str.trim().trim_matches(|c| c == '(' || c == ')');

			if clean_condition.is_empty() {
				tracing::warn!("Empty condition found after splitting: {}", condition_str);
				current_and_group_result = false;
				break;
			}

			// Split into parts
			let parts = if let Some((left, operator, right)) = split_expression(clean_condition) {
				vec![left, operator, right]
			} else {
				tracing::warn!("Invalid expression format: {}", clean_condition);
				current_and_group_result = false;
				break;
			};

			let param_expr = parts[0];
			let operator = parts[1];
			let value = parts[2];

			// Evaluate the condition using the provided closure
			let condition_result = eval_single_condition(param_expr, operator, value);

			tracing::trace!(
				"Condition '{}' {} {} evaluated to: {}",
				param_expr,
				operator,
				value,
				condition_result
			);

			if !condition_result {
				current_and_group_result = false; // If any condition is false, the AND group is false
				break;
			}
		}

		tracing::trace!(
			"evaluate_expression_core: Result for AND group '{}': {}",
			trimmed_or_condition,
			current_and_group_result
		);

		if current_and_group_result {
			tracing::debug!(
				"evaluate_expression_core: Expression evaluated to true. OR branch succeeded: '{}'",
				trimmed_or_condition
			);
			return true;
		}
	}

	// No conditions were true
	false
}

#[cfg(test)]
mod tests {
	use super::*;
	use std::collections::HashMap;

	// Helper mock closure generator for tests
	fn eval_simple_conditions<'a>(
		conditions: &'a HashMap<&'a str, bool>,
	) -> impl FnMut(&str, &str, &str) -> bool + 'a {
		move |param, op, val| {
			// Reconstruct the simple condition string for lookup
			let key = format!("{} {} {}", param, op, val);
			let result = *conditions.get(key.as_str()).unwrap_or(&false);
			result
		}
	}

	#[test]
	fn test_core_empty_expression() {
		let expression = "";
		let result = evaluate_expression_core(expression, |_field, _operator, _value| true);
		assert_eq!(result, false);
	}

	#[test]
	fn test_core_single_condition() {
		let conditions = HashMap::from([("a == 1", true)]);
		assert!(evaluate_expression_core(
			"a == 1",
			eval_simple_conditions(&conditions)
		));
		assert!(!evaluate_expression_core(
			"b == 2",
			eval_simple_conditions(&conditions)
		)); // Condition not in map -> false
	}

	#[test]
	fn test_core_simple_and() {
		// T AND T => T
		let conditions1 = HashMap::from([("a == 1", true), ("b > 2", true)]);
		assert!(evaluate_expression_core(
			"a == 1 AND b > 2",
			eval_simple_conditions(&conditions1)
		));

		// T AND F => F
		let conditions2 = HashMap::from([("a == 1", true), ("b > 2", false)]);
		assert!(!evaluate_expression_core(
			"a == 1 AND b > 2",
			eval_simple_conditions(&conditions2)
		));

		// F AND T => F
		let conditions3 = HashMap::from([("a == 1", false), ("b > 2", true)]);
		assert!(!evaluate_expression_core(
			"a == 1 AND b > 2",
			eval_simple_conditions(&conditions3)
		));

		// F AND F => F
		let conditions4 = HashMap::from([("a == 1", false), ("b > 2", false)]);
		assert!(!evaluate_expression_core(
			"a == 1 AND b > 2",
			eval_simple_conditions(&conditions4)
		));
	}

	#[test]
	fn test_core_simple_or() {
		// T OR T => T
		let conditions1 = HashMap::from([("a == 1", true), ("b > 2", true)]);
		assert!(evaluate_expression_core(
			"a == 1 OR b > 2",
			eval_simple_conditions(&conditions1)
		));

		// T OR F => T
		let conditions2 = HashMap::from([("a == 1", true), ("b > 2", false)]);
		assert!(evaluate_expression_core(
			"a == 1 OR b > 2",
			eval_simple_conditions(&conditions2)
		));

		// F OR T => T
		let conditions3 = HashMap::from([("a == 1", false), ("b > 2", true)]);
		assert!(evaluate_expression_core(
			"a == 1 OR b > 2",
			eval_simple_conditions(&conditions3)
		));

		// F OR F => F
		let conditions4 = HashMap::from([("a == 1", false), ("b > 2", false)]);
		assert!(!evaluate_expression_core(
			"a == 1 OR b > 2",
			eval_simple_conditions(&conditions4)
		));
	}

	#[test]
	fn test_core_mixed_precedence() {
		// Case 1: (T AND F) OR (T AND T) => F OR T => T
		let conditions1 = HashMap::from([
			("a == 1", true),
			("b > 2", false),
			("c < 3", true),
			("d != 4", true),
		]);
		assert!(evaluate_expression_core(
			"a == 1 AND b > 2 OR c < 3 AND d != 4",
			eval_simple_conditions(&conditions1)
		));

		// Case 2: (T AND T) OR (F AND T) => T OR F => T
		let conditions2 = HashMap::from([
			("a == 1", true),
			("b > 2", true),
			("c < 3", false),
			("d != 4", true),
		]);
		assert!(evaluate_expression_core(
			"a == 1 AND b > 2 OR c < 3 AND d != 4",
			eval_simple_conditions(&conditions2)
		));

		// Case 3: (T AND F) OR (F AND T) => F OR F => F
		let conditions3 = HashMap::from([
			("a == 1", true),
			("b > 2", false),
			("c < 3", false),
			("d != 4", true),
		]);
		assert!(!evaluate_expression_core(
			"a == 1 AND b > 2 OR c < 3 AND d != 4",
			eval_simple_conditions(&conditions3)
		));
	}

	#[test]
	fn test_core_leading_trailing_whitespace() {
		// Test leading/trailing whitespace on expression
		let conditions = HashMap::from([("a == 1", true)]);
		assert!(evaluate_expression_core(
			"  a == 1  ",
			eval_simple_conditions(&conditions)
		));
	}

	#[test]
	fn test_core_invalid_syntax_structural() {
		// Conditions are irrelevant here, the structure itself should cause failure
		let conditions = HashMap::new();

		// Incomplete operators
		assert!(
			!evaluate_expression_core("a == 1 AND ", eval_simple_conditions(&conditions)),
			"Trailing AND"
		);
		assert!(
			!evaluate_expression_core(" AND a == 1", eval_simple_conditions(&conditions)),
			"Leading AND"
		);
		assert!(
			!evaluate_expression_core(" a == 1 OR ", eval_simple_conditions(&conditions)),
			"Trailing OR"
		);
		assert!(
			!evaluate_expression_core("OR a == 1", eval_simple_conditions(&conditions)),
			"Leading OR"
		);

		// Double operators
		assert!(
			!evaluate_expression_core("a == 1 AND AND b == 2", eval_simple_conditions(&conditions)),
			"Double AND"
		);
		assert!(
			!evaluate_expression_core("a == 1 OR OR b == 2", eval_simple_conditions(&conditions)),
			"Double OR"
		);

		// Only operators
		assert!(
			!evaluate_expression_core(" AND ", eval_simple_conditions(&conditions)),
			"Only AND"
		);
		assert!(
			!evaluate_expression_core(" OR ", eval_simple_conditions(&conditions)),
			"Only OR"
		);

		// Empty condition from parentheses
		assert!(
			!evaluate_expression_core("( )", eval_simple_conditions(&conditions)),
			"Empty parentheses"
		);
		assert!(
			!evaluate_expression_core("a == 1 AND ( )", eval_simple_conditions(&conditions)),
			"AND empty parentheses"
		);
	}

	#[test]
	fn test_core_invalid_condition_format() {
		// Test conditions where split_expression should fail
		let conditions = HashMap::new(); // Conditions irrelevant

		assert!(
			!evaluate_expression_core("a >", eval_simple_conditions(&conditions)),
			"Missing RHS"
		);
		assert!(
			!evaluate_expression_core("a > 1 AND b <", eval_simple_conditions(&conditions)),
			"AND with missing RHS"
		);
		assert!(
			!evaluate_expression_core("a > 1 OR > b", eval_simple_conditions(&conditions)),
			"OR with missing LHS"
		);
		assert!(
			!evaluate_expression_core("invalid condition", eval_simple_conditions(&conditions)),
			"No operator"
		);
		assert!(
			!evaluate_expression_core("a = = 1", eval_simple_conditions(&conditions)),
			"Double operator internal"
		); // Assumes split_expression handles this
	}

	#[test]
	fn test_core_whitespace_handling() {
		let conditions = HashMap::from([("a == 1", true), ("b > 2", true)]);

		// Test leading/trailing whitespace on expression
		assert!(evaluate_expression_core(
			"  a == 1  ",
			eval_simple_conditions(&conditions)
		));

		// Test whitespace around operators (should now pass)
		assert!(
			evaluate_expression_core("a == 1 AND b > 2", eval_simple_conditions(&conditions)),
			"Single space AND"
		);
		assert!(
			evaluate_expression_core("a == 1  AND b > 2", eval_simple_conditions(&conditions)),
			"Double space AND"
		);
		assert!(
			evaluate_expression_core("a == 1 AND  b > 2", eval_simple_conditions(&conditions)),
			"AND double space"
		);
		assert!(
			evaluate_expression_core("a == 1\tAND\tb > 2", eval_simple_conditions(&conditions)),
			"Tab AND"
		); // Tabs are whitespace
		assert!(
			evaluate_expression_core("a == 1 OR b > 2", eval_simple_conditions(&conditions)),
			"Single space OR"
		);
		assert!(
			evaluate_expression_core("a == 1   OR   b > 2", eval_simple_conditions(&conditions)),
			"Multi space OR"
		);
		assert!(
			evaluate_expression_core("a == 1\nOR\nb > 2", eval_simple_conditions(&conditions)),
			"Newline OR"
		); // Newlines are whitespace
	}

	#[test]
	fn test_core_case_insensitivity() {
		let conditions = HashMap::from([("a == 1", true), ("b > 2", true)]);

		assert!(
			evaluate_expression_core("a == 1 and b > 2", eval_simple_conditions(&conditions)),
			"Lowercase and"
		);
		assert!(
			evaluate_expression_core("a == 1 And b > 2", eval_simple_conditions(&conditions)),
			"Mixed case And"
		);
		assert!(
			evaluate_expression_core("a == 1 OR b > 2", eval_simple_conditions(&conditions)),
			"Uppercase OR"
		);
		assert!(
			evaluate_expression_core("a == 1 or b > 2", eval_simple_conditions(&conditions)),
			"Lowercase or"
		);
		assert!(
			evaluate_expression_core("a == 1 oR b > 2", eval_simple_conditions(&conditions)),
			"Mixed case oR"
		);
	}

	// TODO: Uncomment and fix the following tests once the parser is updated to handle parentheses correctly.
	// --- Tests for Parentheses Grouping ---
	// #[test]
	// fn test_evaluate_expression_fail_parentheses_grouping_1() {
	// 	// Expression: val1 > 50 AND (val2 == true OR val3 < 10)
	// 	// Failure Reason: Current parser splits by 'OR' first, breaking inside parentheses.
	// 	let filter = create_test_filter();
	// 	let args = Some(vec![
	// 		create_evm_param("val1", "100", "uint256"), // T
	// 		create_evm_param("val2", "true", "bool"),   // T
	// 		create_evm_param("val3", "50", "uint256"),  // F
	// 	]);
	// 	// Expected Correct Logic: T AND (T OR F) => T AND T => TRUE
	// 	let expression = "val1 > 50 AND (val2 == true OR val3 < 10)";
	// 	assert!(
	// 		filter.evaluate_expression(expression, &args),
	// 		"Grouping 'A AND (B OR C)' failed. Expression: '{}'",
	// 		expression
	// 	);
	// }

	// #[test]
	// fn test_evaluate_expression_fail_parentheses_grouping_2() {
	// 	// Expression: (val1 > 150 OR val2 == false) AND val3 == 5
	// 	// Failure Reason: Current parser splits by 'OR' first, breaking inside parentheses.
	// 	let filter = create_test_filter();
	// 	let args = Some(vec![
	// 		create_evm_param("val1", "100", "uint256"), // F
	// 		create_evm_param("val2", "false", "bool"),  // T
	// 		create_evm_param("val3", "5", "uint256"),   // T
	// 	]);
	// 	// Expected Correct Logic: (F OR T) AND T => T AND T => TRUE
	// 	let expression = "(val1 > 150 OR val2 == false) AND val3 == 5";
	// 	assert!(
	// 		filter.evaluate_expression(expression, &args),
	// 		"Grouping '(A OR B) AND C' failed. Expression: '{}'",
	// 		expression
	// 	);
	// }

	// #[test]
	// fn test_evaluate_expression_fail_nested_parentheses() {
	// 	// Expression: val1 > 50 AND (val2 == true OR (val3 < 10 AND val4 == 'a'))
	// 	// Failure Reason: Current parser cannot handle nested parentheses structure.
	// 	let filter = create_test_filter();
	// 	let args = Some(vec![
	// 		create_evm_param("val1", "100", "uint256"), // T
	// 		create_evm_param("val2", "false", "bool"),  // F
	// 		create_evm_param("val3", "5", "uint256"),   // T
	// 		create_evm_param("val4", "a", "string"),    // T
	// 	]);
	// 	// Expected Correct Logic: T AND (F OR (T AND T)) => T AND (F OR T) => T AND T => TRUE
	// 	// Current Parser likely yields: false
	// 	let expression = "val1 > 50 AND (val2 == false OR (val3 < 10 AND val4 == 'a'))";
	// 	assert!(
	// 		filter.evaluate_expression(expression, &args),
	// 		"FAILURE EXPECTED: Nested Parentheses failed. Expression: '{}'",
	// 		expression
	// 	);
	// }
}
