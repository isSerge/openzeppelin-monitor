use super::ast::{
	Accessor, ComparisonOperator, Condition, ConditionLeft, Expression, LiteralValue,
	LogicalOperator, VariablePath,
};
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
	(
		opt(one_of(['+', '-'])),
		digit1,
		peek(alt((
			space1.value(()),
			eof.value(()),
			one_of([')', '(', ',', '=', '!', '>', '<']).value(()),
		))),
	)
		.take()
		.map(|s: &str| LiteralValue::Number(s))
		.context(StrContext::Expected(StrContextValue::Description(
			"numeric literal",
		)))
		.parse_next(input)
}

// Parses an unquoted "0x..." or "0X..." sequence as a string.
fn parse_hex_string<'a>(input: &mut Input<'a>) -> ParserResult<LiteralValue<'a>> {
	(
		alt((literal("0x"), literal("0X"))),
		take_while(1.., |c: char| c.is_ascii_hexdigit()), // Ensure at least one hex digit
		peek(alt((
			space1.value(()),
			eof.value(()),
			one_of([')', '(', ',', '=', '!', '>', '<']).value(()),
		))),
	)
		.take()
		.map(|s: &str| LiteralValue::Str(s))
		.context(StrContext::Expected(StrContextValue::Description(
			"hexadecimal string literal",
		)))
		.parse_next(input)
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
	take_while(1.., |c: char| c.is_alphanum() || c == '_' || c == '-')
		.take()
		.verify(|s: &&str| {
			let word = *s;
			!is_keyword(word)
				&& !(word.contains(|c: char| c.is_ascii_digit() || c == '+' || c == '-')
					&& word
						.chars()
						.all(|c| c.is_ascii_digit() || c == '+' || c == '-'))
				&& !((word.starts_with("0x") || word.starts_with("0X"))
					&& word.chars().skip(2).all(|c| c.is_ascii_hexdigit()))
		})
		.map(|s: &str| LiteralValue::Str(s))
		.context(StrContext::Expected(StrContextValue::Description(
			"unquoted string literal",
		)))
		.parse_next(input)
}

/// Parses an accessor (either an index or a key) from the input
fn parse_accessor<'a>(input: &mut Input<'a>) -> ParserResult<Accessor> {
	let index_parser = delimited(
		literal("["),
		// digit1 itself returns &str, try_map converts it
		digit1.try_map(|s: &str| s.parse::<usize>()),
		literal("]"),
	)
	.map(Accessor::Index)
	.context(StrContext::Expected(StrContextValue::Description(
		"array index accessor like '[0]'",
	)));

	let key_parser = (
		literal("."),
		(
			one_of(|c: char| c.is_alpha() || c == '_'),
			take_while(0.., |c: char| c.is_alphanum() || c == '_'),
		)
			.take(),
	)
		.map(|(_, key_slice): (_, &str)| Accessor::Key(key_slice.to_string()))
		.context(StrContext::Expected(StrContextValue::Description(
			"object key accessor like '.key'",
		)));

	alt((index_parser, key_parser)).parse_next(input)
}

fn parse_base_variable_name<'a>(input: &mut Input<'a>) -> ParserResult<&'a str> {
	alt((
		// Standard identifier
		(
				one_of(|c: char| c.is_alpha() || c == '_'),
				take_while(0.., |c: char| c.is_alphanum() || c == '_')
		).take(), // Use .take()

		// Purely numeric identifier
		(
				digit1,
				peek(alt(( // Peek ensures it's properly delimited for an LHS base
						literal('['), literal('.'), space1, eof,
						literal("=="), literal("!="), literal(">="), literal("<="), literal(">"), literal("<"),
						// one_of([')', '(']),
				)))
		).take() // Use .take()
))
.verify(|ident_slice: &&str| !is_keyword(*ident_slice)) // Verify the taken slice
.map(|s:&str|s) // if verify needs &&str
.context(StrContext::Expected(StrContextValue::Description("variable base name (e.g., 'request', '0')")))
.parse_next(input)
}

fn parse_condition_lhs<'a>(input: &mut Input<'a>) -> ParserResult<ConditionLeft<'a>> {
	// Parse the base variable name
	let base = parse_base_variable_name.parse_next(input)?;

	// Parse any accessors (e.g., .key or [0])
	let accessors: Vec<Accessor> = repeat(0.., parse_accessor).parse_next(input)?;

	if accessors.is_empty() {
		Ok(ConditionLeft::Simple(base))
	} else {
		Ok(ConditionLeft::Path(VariablePath { base, accessors }))
	}
}

/// Parses any valid LiteralValue (boolean, number, string, or variable)
/// Handles optional whitespace around the value
fn parse_value<'a>(input: &mut Input<'a>) -> ParserResult<LiteralValue<'a>> {
	delimited(
		space0,
		alt((
			parse_quoted_string,   // "'string'"
			parse_boolean,         // "true" / "false"
			parse_hex_string,      // "0x..."
			parse_number,          // "123" / "-123"
			parse_unquoted_string, // "unquoted_string"
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
	let (left, operator, right) = (parse_condition_lhs, parse_comparison_operator, parse_value)
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
