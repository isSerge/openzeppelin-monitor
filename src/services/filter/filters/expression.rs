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
				// Skip empty conditions resulting from splitting (e.g., "A AND ")
				return false; // Treat empty part as non-falsifying
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

			eval_single_condition(param_expr, operator, value)
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

	// TODO: move relevant tests to this module
	#[test]
	fn test_evaluate_expression_returns_false_if_empty_expression() {
		let expression = "";
		let result = evaluate_expression_core(expression, |_field, _operator, _value| true);
		assert_eq!(result, false);
	}
}
