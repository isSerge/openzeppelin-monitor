///! This module defines the abstract syntax tree (AST) for the filter expressions

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value<'a> {
	Bool(bool),
	Str(&'a str),
	// Store as str, conversion to specific type is done within chain context
	Number(&'a str),
	Variable(&'a str),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ComparisonOperator {
	Eq,
	Ne,
	Gt,
	Gte,
	Lt,
	Lte,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LogicalOperator {
	And,
	Or,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Condition<'a> {
	pub left: &'a str, // variable name
	pub operator: ComparisonOperator,
	pub right: Value<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression<'a> {
	Condition(Condition<'a>),
	Logical {
		left: Box<Expression<'a>>,
		operator: LogicalOperator,
		right: Box<Expression<'a>>,
	},
}
