use super::*;
use super::types::*;

use nom::{
	IResult,
	branch::alt,
	bytes::complete::tag,
	character::complete::{char, multispace0},
	combinator::{map, opt, value},
	multi::many0,
	sequence::{delimited, preceded}
};

// Internal type used during parsing
#[derive(Clone)]
enum Operator {
	Add,
	Subtract,
	Multiply,
	Divide,
	Modulus,
	Negate,
	Not,
	And,
	Or,
	BitAnd,
	BitOr,
	BitXor,
	BitShiftLeft,
	BitShiftRight,
	BitNot,
	Equal,
	NotEqual,
	LessThan,
	LessThanEqual,
	GreaterThan,
	GreaterThanEqual
}

// TODO: fix precedence
// https://en.cppreference.com/w/c/language/operator_precedence

fn parse_function_call_arguments(input: &str) -> IResult<&str, Vec<Expr>> {
	let (i, _) = multispace0(input)?;

	let (i, first_arg) = parse_expression(i)?;

	let mut result = Vec::new();
	result.push(first_arg);

	let (i, mut other_args) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, _) = char(',')(i)?;

		let (i, _) = multispace0(i)?;
		let (i, arg) = parse_expression(i)?;

		Ok((i, arg))
	})(i)?;

	result.append(&mut other_args);

	Ok((i, result))
}

fn parse_function_call(input: &str) -> IResult<&str, Expr> {
	let (i, func_name) = parse_identifier(input)?;

	let (i, _) = multispace0(i)?;
	let (i, arguments) = delimited(char('('), opt(parse_function_call_arguments), preceded(multispace0, char(')')))(i)?;

	let final_args = if let Some(args_inner) = arguments {
		args_inner
	} else {
		Vec::new()
	};

	Ok((i, Expr::FunctionCall(func_name, final_args)))
}

fn parse_expression_lowest(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, term) = alt((
		parse_function_call,
		map(parse_literal, |l| Expr::Literal(l)),
		delimited(char('('), parse_expression, preceded(multispace0, char(')')))
	))(i)?;

	let (i, array_access) = opt(
		delimited(preceded(multispace0, char('[')), parse_expression, preceded(multispace0, char(']')))
	)(i)?;

	if let Some(array) = array_access {
		Ok((i, Expr::ArrayAccess(Box::new(term), Box::new(array))))
	} else {
		Ok((i, term))
	}
}

fn parse_expression_member_access(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, term) = parse_expression_lowest(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, _) = char('.')(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_lowest(i)?;

		Ok((i, inner))
	})(i)?;

	let mut output_value = term;
	for inner in operator_chain {
		output_value = Expr::MemberAccess(Box::new(output_value), Box::new(inner));
	}

	Ok((i, output_value))
}

fn parse_expression_unary(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, leading_op) = opt(alt((
		value(Operator::Negate, char('-')),
		value(Operator::Not, char('!')),
		value(Operator::Not, char('~'))
		)))(i)?;

	let (i, term) = parse_expression_member_access(i)?;

	if let Some(op) = leading_op {
		let op_expr = match op {
			Operator::Negate => Expr::Negate(Box::new(term)),
			Operator::Not => Expr::Not(Box::new(term)),
			Operator::BitNot => Expr::BitNot(Box::new(term)),
			_ => panic!("unhandled operator in parse_expression_unary")
		};

		Ok((i, op_expr))
	} else {
		Ok((i, term))
	}
}

fn parse_expression_multiply(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_unary(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, op) = alt((
			value(Operator::Multiply, char('*')),
			value(Operator::Divide, char('/')),
			value(Operator::Modulus, char('%'))
		))(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_unary(i)?;

		Ok((i, (op, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (op, inner) in operator_chain {
		output_value = match op {
			Operator::Multiply => Expr::Multiply(Box::new(output_value), Box::new(inner)),
			Operator::Divide => Expr::Divide(Box::new(output_value), Box::new(inner)),
			Operator::Modulus => Expr::Modulus(Box::new(output_value), Box::new(inner)),
			_ => panic!("unhandled operator in parse_expression_multiply")
		};
	}

	Ok((i, output_value))
}

fn parse_expression_add(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_multiply(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, op) = alt((
			value(Operator::Add, char('+')),
			value(Operator::Subtract, char('-'))
		))(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_multiply(i)?;

		Ok((i, (op, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (op, inner) in operator_chain {
		output_value = match op {
			Operator::Add => Expr::Add(Box::new(output_value), Box::new(inner)),
			Operator::Subtract => Expr::Subtract(Box::new(output_value), Box::new(inner)),
			_ => panic!("unhandled operator in parse_expression_add")
		};
	}

	Ok((i, output_value))
}

fn parse_expression_bitshift(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_add(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, op) = alt((
			value(Operator::BitShiftLeft, tag("<<")),
			value(Operator::BitShiftRight, tag(">>"))
		))(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_add(i)?;

		Ok((i, (op, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (op, inner) in operator_chain {
		output_value = match op {
			Operator::BitShiftLeft => Expr::BitShiftLeft(Box::new(output_value), Box::new(inner)),
			Operator::BitShiftRight => Expr::BitShiftRight(Box::new(output_value), Box::new(inner)),
			_ => panic!("unhandled operator in parse_expression_bitshift")
		};
	}

	Ok((i, output_value))
}

fn parse_expression_compare(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_bitshift(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, op) = alt((
			value(Operator::LessThanEqual, tag("<=")),
			value(Operator::LessThan, char('<')),
			value(Operator::GreaterThanEqual, tag(">=")),
			value(Operator::GreaterThan, char('>')),
		))(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_bitshift(i)?;

		Ok((i, (op, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (op, inner) in operator_chain {
		output_value = match op {
			Operator::LessThanEqual => Expr::LessThanEqual(Box::new(output_value), Box::new(inner)),
			Operator::LessThan => Expr::LessThan(Box::new(output_value), Box::new(inner)),
			Operator::GreaterThanEqual => Expr::GreaterThanEqual(Box::new(output_value), Box::new(inner)),
			Operator::GreaterThan => Expr::GreaterThan(Box::new(output_value), Box::new(inner)),
			_ => panic!("unhandled operator in parse_expression_compare")
		};
	}

	Ok((i, output_value))
}

fn parse_expression_equality(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_compare(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, op) = alt((
			value(Operator::NotEqual, tag("!=")),
			value(Operator::Equal, tag("=="))
		))(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_compare(i)?;

		Ok((i, (op, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (op, inner) in operator_chain {
		output_value = match op {
			Operator::NotEqual => Expr::NotEqual(Box::new(output_value), Box::new(inner)),
			Operator::Equal => Expr::Equal(Box::new(output_value), Box::new(inner)),
			_ => panic!("unhandled operator in parse_expression_equality")
		};
	}

	Ok((i, output_value))
}

fn parse_expression_bitwise_and(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_equality(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, _) = char('&')(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_equality(i)?;

		Ok((i, inner))
	})(i)?;

	let mut output_value = first_term;
	for inner in operator_chain {
		output_value = Expr::BitAnd(Box::new(output_value), Box::new(inner));
	}

	Ok((i, output_value))
}

fn parse_expression_bitwise_xor(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_bitwise_and(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, _) = char('^')(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_bitwise_and(i)?;

		Ok((i, inner))
	})(i)?;

	let mut output_value = first_term;
	for inner in operator_chain {
		output_value = Expr::BitXor(Box::new(output_value), Box::new(inner));
	}

	Ok((i, output_value))
}

fn parse_expression_bitwise_or(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_bitwise_xor(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, _) = char('|')(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_bitwise_xor(i)?;

		Ok((i, inner))
	})(i)?;

	let mut output_value = first_term;
	for inner in operator_chain {
		output_value = Expr::BitOr(Box::new(output_value), Box::new(inner));
	}

	Ok((i, output_value))
}

fn parse_expression_logical_and(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_bitwise_or(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, _) = tag("&&")(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_bitwise_or(i)?;

		Ok((i, inner))
	})(i)?;

	let mut output_value = first_term;
	for inner in operator_chain {
		output_value = Expr::And(Box::new(output_value), Box::new(inner));
	}

	Ok((i, output_value))
}

fn parse_expression_logical_or(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_logical_and(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, _) = tag("||")(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_logical_and(i)?;

		Ok((i, inner))
	})(i)?;

	let mut output_value = first_term;
	for inner in operator_chain {
		output_value = Expr::Or(Box::new(output_value), Box::new(inner));
	}

	Ok((i, output_value))
}

fn parse_expression_ternary(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_logical_or(i)?;

	let (i, ternary_terms) = opt(|input: &str| {
		// Note: C/C++ parse the middle expression as though it is in parenthesis, so recurse from the top here
		let (i, _) = multispace0(input)?;
		let (i, _) = char('?')(i)?;
		let (i, if_term) = parse_expression(i)?;

		let (i, _) = multispace0(i)?;
		let (i, _) = char(':')(i)?;
		let (i, else_term) = parse_expression_logical_or(i)?;

		Ok((i, (if_term, else_term)))
	})(i)?;

	if let Some((if_term, else_term)) = ternary_terms {
		Ok((i, Expr::Ternary(Box::new(first_term), Box::new(if_term), Box::new(else_term))))
	} else {
		Ok((i, first_term))
	}
}

pub fn parse_expression(input: &str) -> IResult<&str, Expr> {
	parse_expression_ternary(input)
}
