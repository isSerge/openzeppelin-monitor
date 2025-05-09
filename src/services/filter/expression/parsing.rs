use super::ast::{ComparisonOperator, Condition, Expression, LiteralValue, LogicalOperator};
use winnow::{
	ascii::{digit1, space0, space1, Caseless},
	combinator::{alt, delimited, eof, opt, peek, repeat},
	error::{ContextError, ErrMode, ParseError, StrContext, StrContextValue},
	prelude::*,
	token::{literal, one_of, take_while},
};

/// --- Helper aliases ---
type Input<'a> = &'a str;
/// Result for internal parser functions
type ParserResult<T> = winnow::Result<T, ErrMode<ContextError>>;

// Helper to check for keywords
fn is_keyword(ident: &str) -> bool {
	matches!(
		ident.to_ascii_lowercase().as_str(),
		"true" | "false" | "and" | "or" | "contains" | "starts_with" | "ends_with"
	)
}

// Helper to check if a char is a hex digit (a-f, A-F, 0-9)
fn is_hex_digit(c: char) -> bool {
	c.is_ascii_hexdigit()
}

/// --- Parser functions ---
/// Parses boolean literals into `LiteralValue::Bool`
fn parse_boolean<'a>(input: &mut Input<'a>) -> ParserResult<LiteralValue<'a>> {
	alt((
		literal("true").map(|_| LiteralValue::Bool(true)),
		literal("false").map(|_| LiteralValue::Bool(false)),
	))
	.context(StrContext::Expected(StrContextValue::Description(
		"boolean literal 'true' or 'false'",
	)))
	.parse_next(input)
}

/// Parser integer literals into `LiteralValue::Number`
fn parse_number<'a>(input: &mut Input<'a>) -> ParserResult<LiteralValue<'a>> {
	let start_input = *input;
	// Parse optional sign and digits
	// Define the parser sequence including the peek for the delimiter
	let mut parser_sequence = (
		opt(one_of(['+', '-'])),
		digit1,
		peek(alt((
			space1.value(()),
			one_of([')', '(']).value(()),
			one_of(['=', '!', '>', '<']).value(()), // TODO: Only first char of operator needed? Check this.
			eof.value(()),
		))),
	);

	let parse_result = parser_sequence.parse_next(input);
	match parse_result {
		Ok(_) => {
			let consumed_len = start_input.len() - input.len();
			let number_str = &start_input[..consumed_len];
			Ok(LiteralValue::Number(number_str))
		}
		Err(e) => Err(ErrMode::Backtrack(e)),
	}
}

// Parses an unquoted "0x..." or "0X..." sequence as a string.
fn parse_hex_string<'a>(input: &mut Input<'a>) -> ParserResult<LiteralValue<'a>> {
	// Checkpoint the input before attempting this specific format
	let initial_input = *input;

	let mut zero_x_parser = alt((
		literal::<_, _, ContextError>("0x"),
		literal::<_, _, ContextError>("0X"),
	));

	// Attempt to parse "0x" or "0X"
	if zero_x_parser.parse_next(input).is_err() {
		let context = ContextError::new();
		return Err(ErrMode::Backtrack(context));
	}

	// We need to ensure hex digits are present and consume them, but we don't need their value separately
	// if we're slicing the original input.
	let _parsed_hex_digits: &str = take_while(1.., is_hex_digit)
		.context(StrContext::Expected(StrContextValue::Description(
			"hexadecimal digits after '0x' or '0X'",
		)))
		.parse_next(input)?;
	let total_consumed_len = initial_input.len() - input.len();
	let full_hex_literal_str = &initial_input[..total_consumed_len];

	Ok(LiteralValue::Str(full_hex_literal_str))
}

// TODO: handle escaped quotes
/// Parses string literals enclosed in single quotes into `LiteralValue::Str`
fn parse_quoted_string<'a>(input: &mut Input<'a>) -> ParserResult<LiteralValue<'a>> {
	// Match opening quote, content (non-quote characters), closing quote
	delimited('\'', take_while(0.., |c| c != '\''), '\'')
		.map(LiteralValue::Str)
		.context(StrContext::Expected(StrContextValue::Description(
			"single-quoted string literal",
		)))
		.parse_next(input)
}

/// Fallback parser for unquoted strings (applied last)
fn parse_unquoted_string<'a>(input: &mut Input<'a>) -> ParserResult<LiteralValue<'a>> {
	// Take anything that's alphanumeric/underscore
	// ASSUMPTION: If we got here, it's not quoted, not bool, not hex, not pure number.
	let word: &str = take_while(1.., |c: char| c.is_alphanumeric() || c == '_')
		// Avoid matching keywords or simple numbers again
		.verify(|s: &str| {
			!is_keyword(s)
				&& !s
					.chars()
					.all(|c| c.is_ascii_digit() || c == '+' || c == '-')
		})
		.context(StrContext::Expected(StrContextValue::Description(
			"unquoted string literal",
		)))
		.parse_next(input)?;

	// NO checks for keywords or numbers here - rely purely on alt order.
	Ok(LiteralValue::Str(word))
}

/// Parses a variable name into string slice
fn parse_variable_name<'a>(input: &mut Input<'a>) -> ParserResult<&'a str> {
	let start_input = *input;
	let first_char = one_of(|c: char| c.is_alphabetic() || c == '_').parse_next(input)?;
	let rest_chars: &str =
		take_while(0.., |c: char| c.is_alphanumeric() || c == '_').parse_next(input)?;
	let consumed_len = (first_char.len_utf8() + rest_chars.len()) as usize;
	let ident = &start_input[..consumed_len];

	// Check if the identifier is a reserved keyword
	if is_keyword(ident) {
		let mut context = ContextError::new();
		context.push(StrContext::Label("keyword used as identifier"));
		return Err(ErrMode::Backtrack(context));
	}
	Ok(ident)
}

/// Parses any valid LiteralValue (boolean, number, string, or variable)
/// Handles optional whitespace around the value
fn parse_value<'a>(input: &mut Input<'a>) -> ParserResult<LiteralValue<'a>> {
	delimited(
		space0,
		alt((
			parse_quoted_string,
			parse_boolean,
			parse_hex_string,
			parse_number,
			parse_unquoted_string, // Fallback for unquoted strings
		)),
		space0,
	)
	.context(StrContext::Expected(StrContextValue::Description(
		"boolean, number, or string",
	)))
	.parse_next(input)
}

/// Parses a comparison operator (e.g., ==, !=, >, >=, <, <=)
/// Handles optional whitespace around the operator
fn parse_comparison_operator<'a>(input: &mut Input<'a>) -> ParserResult<ComparisonOperator> {
	delimited(
		space0,
		alt((
			literal(Caseless("contains")).map(|_| ComparisonOperator::Contains),
			literal(Caseless("starts_with")).map(|_| ComparisonOperator::StartsWith),
			literal(Caseless("ends_with")).map(|_| ComparisonOperator::EndsWith),
			literal(">=").map(|_| ComparisonOperator::Gte),
			literal("<=").map(|_| ComparisonOperator::Lte),
			literal("==").map(|_| ComparisonOperator::Eq),
			literal("!=").map(|_| ComparisonOperator::Ne),
			literal(">").map(|_| ComparisonOperator::Gt),
			literal("<").map(|_| ComparisonOperator::Lt),
		)),
		space0,
	)
	.context(StrContext::Expected(StrContextValue::Description(
		"comparison operator (e.g., ==, >, starts_with)",
	)))
	.parse_next(input)
}

/// Parses a condition expression (e.g., "a == 1") into an `Expression::Condition`
fn parse_condition<'a>(input: &mut Input<'a>) -> ParserResult<Expression<'a>> {
	let (left, operator, right) = (parse_variable_name, parse_comparison_operator, parse_value)
		.context(StrContext::Expected(StrContextValue::Description(
			"condition expression (e.g., variable == value)",
		)))
		.parse_next(input)?;

	let condition = Condition {
		left,
		operator,
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
				(space0, literal(")")).context(StrContext::Expected(StrContextValue::Description(
					"closing parenthesis ')'",
				))),
			),
		)),
		space0,
	)
	.context(StrContext::Expected(StrContextValue::Description(
		"condition or parenthesized expression",
	)))
	.parse_next(input)
}

/// Parses the AND operator and its components
fn parse_and_expression<'a>(input: &mut Input<'a>) -> ParserResult<Expression<'a>> {
	let left = parse_term.parse_next(input)?;

	let and_operator_parser = delimited(
		space0,
		literal(Caseless("AND")).value(LogicalOperator::And),
		space0,
	)
	.context(StrContext::Expected(StrContextValue::Description(
		"logical operator AND",
	)));

	let trailing_parser = (and_operator_parser, parse_term);

	let folded_and_parser = repeat(0.., trailing_parser).fold(
		move || left.clone(), // Clone the left side for initial value
		|acc, (op, right)| Expression::Logical {
			left: Box::new(acc),
			operator: op,
			right: Box::new(right),
		},
	);

	folded_and_parser
		.context(StrContext::Expected(StrContextValue::Description(
			"AND expression",
		)))
		.parse_next(input)
}

/// Parses the OR operator and its components
fn parse_or_expression<'a>(input: &mut Input<'a>) -> ParserResult<Expression<'a>> {
	let left = parse_and_expression.parse_next(input)?;

	let or_operator_parser = delimited(
		space0,
		literal(Caseless("OR")).value(LogicalOperator::Or),
		space0,
	)
	.context(StrContext::Expected(StrContextValue::Description(
		"logical operator OR",
	)));

	let trailing_parser = (or_operator_parser, parse_and_expression);

	let folded_or_parser = repeat(0.., trailing_parser).fold(
		move || left.clone(), // Clone the left side for initial value
		|acc, (op, right)| Expression::Logical {
			left: Box::new(acc),
			operator: op,
			right: Box::new(right),
		},
	);

	folded_or_parser
		.context(StrContext::Expected(StrContextValue::Description(
			"OR expression",
		)))
		.parse_next(input)
}

/// Parses the entire expression, starting from the highest precedence
fn parse_expression<'a>(input: &mut Input<'a>) -> ParserResult<Expression<'a>> {
	delimited(space0, parse_or_expression, space0)
		.context(StrContext::Expected(StrContextValue::Description(
			"a full expression",
		)))
		.parse_next(input)
}

/// Public method, which parses a string expression into an `Expression` AST
pub fn parse<'a>(
	expression_str: &'a str,
) -> Result<Expression<'a>, ParseError<Input<'a>, ContextError>> {
	// Define the parser for the entire expression
	// This parser will parse the expression and ensure it ends with EOF
	let mut full_expression_parser = (parse_expression, eof).map(|(expr, _)| expr);

	full_expression_parser.parse(expression_str)
}
