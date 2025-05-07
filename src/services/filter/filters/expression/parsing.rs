use thiserror::Error;
use winnow::{
	ascii::{digit1, space0, Caseless},
	combinator::{alt, delimited, eof, opt, repeat},
	error::{ContextError, StrContext},
	prelude::*,
	token::{literal, one_of, take_while},
};

use crate::services::filter::filters::expression::ast::{
	ComparisonOperator, Condition, Expression, LogicalOperator, Value,
};

/// --- Error definitions ---
#[derive(Debug, PartialEq, Eq, Error)]
pub enum ExpressionParseError {
	#[error("Winnow parsing error")]
	Parser(String),
	// TODO: add more specific error types
}

/// --- Helper aliases ---
type Input<'a> = &'a str;
type ParserResult<T> = winnow::Result<T>;

/// --- Parser functions ---
/// Parses boolean literals into `Value::Bool`
fn parse_boolean<'a>(input: &mut Input<'a>) -> ParserResult<Value<'a>> {
	alt((
		literal("true").map(|_| Value::Bool(true)),
		literal("false").map(|_| Value::Bool(false)),
	))
	.parse_next(input)
}

/// Parser integer literals into `Value::Number`
fn parse_number<'a>(input: &mut Input<'a>) -> ParserResult<Value<'a>> {
	let start_input = *input;
	let _ = (opt(one_of(['+', '-'])), digit1).parse_next(input)?;
	let consumed_len = start_input.len() - input.len();
	let number_str = &start_input[..consumed_len];
	Ok(Value::Number(number_str))
}

// TODO: handle escaped quotes
/// Parses string literals enclosed in single quotes into `Value::Str`
fn parse_string<'a>(input: &mut Input<'a>) -> ParserResult<Value<'a>> {
	// Match opening quote, content (non-quote characters), closing quote
	delimited('\'', take_while(0.., |c| c != '\''), '\'')
		.map(Value::Str)
		.parse_next(input)
}

/// Parses a variable name into `Value::Variable`
fn parse_variable<'a>(input: &mut Input<'a>) -> ParserResult<Value<'a>> {
	let start_input = *input;
	let first_char = one_of(|c: char| c.is_alphabetic() || c == '_').parse_next(input)?;
	let rest_chars: &str =
		take_while(0.., |c: char| c.is_alphanumeric() || c == '_').parse_next(input)?;
	let consumed_len = (first_char.len_utf8() + rest_chars.len()) as usize;
	let ident = &start_input[..consumed_len];

	// Check if the identifier is a reserved keyword
	if ident == "true" || ident == "false" {
		let mut context = ContextError::new();
		context.push(StrContext::Label("keyword used as identifier"));
		Err(context)
	} else {
		Ok(Value::Variable(ident))
	}
}

/// Parses any valid Value (boolean, number, string, or variable)
/// Handles optional whitespace around the value
fn parse_value<'a>(input: &mut Input<'a>) -> ParserResult<Value<'a>> {
	delimited(
		space0,
		alt((parse_boolean, parse_number, parse_string, parse_variable)),
		space0,
	)
	.parse_next(input)
}

/// Parses a comparison operator (e.g., ==, !=, >, >=, <, <=)
/// Handles optional whitespace around the operator
fn parse_comparison_operator<'a>(input: &mut Input<'a>) -> ParserResult<ComparisonOperator> {
	delimited(
		space0,
		alt((
			literal("==").map(|_| ComparisonOperator::Eq),
			literal("!=").map(|_| ComparisonOperator::Ne),
			literal(">").map(|_| ComparisonOperator::Gt),
			literal(">=").map(|_| ComparisonOperator::Gte),
			literal("<").map(|_| ComparisonOperator::Lt),
			literal("<=").map(|_| ComparisonOperator::Lte),
		)),
		space0,
	)
	.parse_next(input)
}

/// Parses a condition expression (e.g., "a == 1") into an `Expression::Condition`
fn parse_condition<'a>(input: &mut Input<'a>) -> ParserResult<Expression<'a>> {
	let (left, op, right) =
		(parse_variable, parse_comparison_operator, parse_value).parse_next(input)?;

	// Ensure the left side is a variable name
	let variable_name = match left {
		Value::Variable(name) => name,
		_ => {
			let mut context = ContextError::new();
			context.push(StrContext::Label("left side must be a variable name"));
			return Err(context);
		}
	};

	let condition = Condition {
		left: variable_name,
		operator: op,
		right,
	};

	Ok(Expression::Condition(condition))
}

/// Parses the highest precedence components: conditions and parenthesized expressions
fn parse_term<'a>(input: &mut Input<'a>) -> ParserResult<Expression<'a>> {
	delimited(
		space0,
		alt((
			// Parse a condition
			parse_condition,
			// Parse a parenthesized expression
			delimited(
				(literal("("), space0),
				parse_expression,
				(space0, literal(")")),
			),
		)),
		space0,
	)
	.parse_next(input)
}

/// Parses the AND operator and its components
fn parse_and_expression<'a>(input: &mut Input<'a>) -> ParserResult<Expression<'a>> {
	let left = parse_term.parse_next(input)?;

	let and_operator_parser = delimited(
		space0,
		literal(Caseless("AND")).value(LogicalOperator::And),
		space0,
	);

	let trailing_parser = (and_operator_parser, parse_term);

	let mut folded_and_parser = repeat(0.., trailing_parser).fold(
		move || left.clone(), // Clone the left side for initial value
		|acc, (op, right)| Expression::Logical {
			left: Box::new(acc),
			operator: op,
			right: Box::new(right),
		},
	);

	folded_and_parser.parse_next(input)
}

/// Parses the OR operator and its components
fn parse_or_expression<'a>(input: &mut Input<'a>) -> ParserResult<Expression<'a>> {
	let left = parse_and_expression.parse_next(input)?;

	let or_operator_parser = delimited(
		space0,
		literal(Caseless("OR")).value(LogicalOperator::Or),
		space0,
	);

	let trailing_parser = (or_operator_parser, parse_and_expression);

	let mut folded_or_parser = repeat(0.., trailing_parser).fold(
		move || left.clone(), // Clone the left side for initial value
		|acc, (op, right)| Expression::Logical {
			left: Box::new(acc),
			operator: op,
			right: Box::new(right),
		},
	);

	folded_or_parser.parse_next(input)
}

/// Parses the entire expression, starting from the highest precedence
fn parse_expression<'a>(input: &mut Input<'a>) -> ParserResult<Expression<'a>> {
	delimited(space0, parse_or_expression, space0).parse_next(input)
}

/// Public method, which parses a string expression into an `Expression` AST
pub fn parse<'a>(expression_str: &'a str) -> Result<Expression<'a>, ExpressionParseError> {
	// Define the parser for the entire expression
	// This parser will parse the expression and ensure it ends with EOF
	let mut full_expression_parser = (parse_expression, eof).map(|(expr, _)| expr);

	match full_expression_parser.parse(expression_str) {
		Ok(expr) => Ok(expr),
		Err(err) => Err(ExpressionParseError::Parser(err.to_string())),
	}
}
