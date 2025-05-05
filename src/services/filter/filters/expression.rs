//! Shared logic for evaluating logical expressions

// TODO: move here, not used anywhere else
use crate::utils::split_expression;

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
	let or_conditions: Vec<&str> = expression.split(" OR ").collect();

	// For OR logic, any condition being true makes the whole expression true
	for or_condition in or_conditions {
		// Split each OR condition by AND
		let and_conditions: Vec<&str> = or_condition.trim().split(" AND ").collect();

		// All AND conditions must be true
		let and_result = and_conditions.iter().all(|condition| {
			// Remove any surrounding parentheses and trim
			let clean_condition = condition.trim().trim_matches(|c| c == '(' || c == ')');

			if clean_condition.is_empty() {
				tracing::warn!("Empty condition found after splitting: {}", condition);
				return false;
			}

			// Split into parts
			let parts = if let Some((left, operator, right)) = split_expression(clean_condition) {
				vec![left, operator, right]
			} else {
				tracing::warn!("Invalid expression format: {}", clean_condition);
				return false;
			};

			let param_expr = parts[0];
			let operator = parts[1];
			let value = parts[2];

			let condition_result = eval_single_condition(param_expr, operator, value);

			tracing::trace!(
				"Condition '{} {} {}' evaluated to: {}",
				param_expr,
				operator,
				value,
				condition_result
			);

			condition_result
		});

		// If any OR condition is true, return true
		if and_result {
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
		let conditions_true = HashMap::from([("a == 1", true)]);
		assert!(evaluate_expression_core(
			"a == 1",
			eval_simple_conditions(&conditions_true)
		));

		let conditions_false = HashMap::from([("a == 1", false)]);
		assert!(!evaluate_expression_core(
			"a == 1",
			eval_simple_conditions(&conditions_false)
		));
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
	fn test_core_invalid_expressions() {
		// Conditions are irrelevant for invalid expressions, because they should fail anyway
		let conditions = HashMap::new();

		// Missing expression parts
		assert!(!evaluate_expression_core(
			"amount > ",
			eval_simple_conditions(&conditions)
		));

		assert!(!evaluate_expression_core(
			"amount",
			eval_simple_conditions(&conditions)
		));
		
		assert!(!evaluate_expression_core(
			"> 1000",
			eval_simple_conditions(&conditions)
		));

		assert!(!evaluate_expression_core(
			"value 100",
			eval_simple_conditions(&conditions)
		));

		// Invalid condition format
		assert!(!evaluate_expression_core(
			"value == 100 invalid",
			eval_simple_conditions(&conditions)
		));

		// Invalid operator
		assert!(!evaluate_expression_core(
			"value invalid 100",
			eval_simple_conditions(&conditions)
		));
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

	// // --- Tests for Syntax Variations ---
	// #[test]
	// fn test_evaluate_expression_fail_whitespace_sensitivity() {
	// 	// Failure Reason: split(" OR ") and split(" AND ") require exactly one space.
	// 	let filter = create_test_filter();
	// 	let args = Some(vec![
	// 		create_evm_param("val1", "100", "uint256"), // T
	// 		create_evm_param("val2", "true", "bool"),   // T
	// 	]);

	// 	// Test OR with extra spaces
	// 	let expr_or = "val1 > 50  OR  val2 == false"; // Expected T OR F => TRUE
	// 	assert!(
	// 		filter.evaluate_expression(expr_or, &args),
	// 		"Whitespace sensitivity on 'OR'. Expression: '{}'",
	// 		expr_or
	// 	);

	// 	// Test AND with extra spaces
	// 	let expr_and = "val1 > 50 AND  val2 == true"; // Expected T AND T => TRUE
	// 	assert!(
	// 		filter.evaluate_expression(expr_and, &args),
	// 		"Whitespace sensitivity on 'AND'. Expression: '{}'",
	// 		expr_and
	// 	);
	// }

	// #[test]
	// fn test_evaluate_expression_fail_case_sensitivity() {
	// 	// Failure Reason: split(" OR ") and split(" AND ") require uppercase operators.
	// 	let filter = create_test_filter();
	// 	let args = Some(vec![
	// 		create_evm_param("val1", "100", "uint256"), // T
	// 		create_evm_param("val2", "true", "bool"),   // T
	// 	]);

	// 	// Test lowercase 'or'
	// 	let expr_or = "val1 > 50 or val2 == false"; // Expected T or F => TRUE
	// 	assert!(
	// 		filter.evaluate_expression(expr_or, &args),
	// 		"Case sensitivity on 'or'. Expression: '{}'",
	// 		expr_or
	// 	);

	// 	// Test lowercase 'and'
	// 	let expr_and = "val1 > 50 and val2 == true"; // Expected T and T => TRUE
	// 	assert!(
	// 		filter.evaluate_expression(expr_and, &args),
	// 		"Case sensitivity on 'and'. Expression: '{}'",
	// 		expr_and
	// 	);
	// }
}
