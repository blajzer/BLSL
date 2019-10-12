use super::*;
use super::types::*;

use nom::{
	IResult,
	error::{ErrorKind, ParseError},
	Err::Error,
	branch::alt,
	bytes::complete::tag,
	character::complete::{char, hex_digit1, multispace0},
	combinator::{map, opt, value},
	multi::many0,
	sequence::{delimited, preceded}
};
use nom_locate::position;

// Precedence rules based on C:
// https://en.cppreference.com/w/c/language/operator_precedence


enum SomeInt {
	U64(u64),
	I64(i64)
}

fn parse_double(input: Span) -> IResult<Span, f64> {
	let (i, leading_digits) = digit1(input)?;
	let (i, _) = char('.')(i)?;
	let (i, trailing_digits) = digit1(i)?;
	let (i, scientific) = opt(|input: Span| {
		let (i, _) = char('e')(input)?;
		let (i, sign) = opt(alt((tag("-"), tag("+"))))(i)?;
		let (i, digits) = digit1(i)?;

		let mut combined = String::new();
		combined += "e";
		if let Some(sign) = sign {
			combined += sign.fragment;
		}
		combined += digits.fragment;

		Ok((i, combined))
	})(i)?;

	let mut float_string = leading_digits.fragment.to_string();
	float_string += ".";
	float_string += trailing_digits.fragment;
	
	if let Some(scientific) = scientific {
		float_string += scientific.as_str();
	}

	if let Ok(num) = float_string.as_str().parse::<f64>() {
		Ok((i, num))
	} else {
		Err(Error(ParseError::from_error_kind(i, ErrorKind::Float)))
	}
}

fn parse_integer(input: Span) -> IResult<Span, SomeInt> {
	let (i, digits) = digit1(input)?;

	if let Ok(num) = digits.fragment.parse::<i64>() {
		Ok((i, SomeInt::I64(num)))
	} else if let Ok(num) = digits.fragment.parse::<u64>() {
		Ok((i, SomeInt::U64(num)))
	} else {
		Err(Error(ParseError::from_error_kind(i, ErrorKind::Digit)))
	}
}

fn parse_hex(input: Span) -> IResult<Span, u64> {
	let (i, _) = alt((tag("0x"), tag("0X")))(input)?;
	let (i, digits) = hex_digit1(i)?;

	if let Ok(num) = u64::from_str_radix(digits.fragment, 16) {
		Ok((i, num))
	} else {
		Err(Error(ParseError::from_error_kind(i, ErrorKind::HexDigit)))
	}
}

fn parse_literal(input: Span) -> IResult<Span, Literal> {
	// TODO: fix all of this. it doesn't greedily handle floats properly...
	// i.e. it thinks everything is a float (-_-)
	alt((
		map(parse_double, |f: f64| Literal::Float(f)),
		map(parse_hex, |u: u64| Literal::UInt(u)),
		map(parse_integer, |s: SomeInt| {
			match s {
				SomeInt::I64(i) => Literal::Int(i),
				SomeInt::U64(u) => Literal::UInt(u),
			}
		}),
		map(parse_identifier, |s: String| {
			match s.as_str() {
				"true" => Literal::Bool(true),
				"false" => Literal::Bool(false),
				_ => Literal::Identifier(s)
			}
		})
	))(input)
}

fn parse_function_call_arguments(input: Span) -> IResult<Span, Vec<Expr>> {
	let (i, _) = multispace0(input)?;

	let (i, first_arg) = parse_expression(i)?;

	let mut result = Vec::new();
	result.push(first_arg);

	let (i, mut other_args) = many0(|input: Span| {
		let (i, _) = multispace0(input)?;
		let (i, _) = char(',')(i)?;

		let (i, _) = multispace0(i)?;
		let (i, arg) = parse_expression(i)?;

		Ok((i, arg))
	})(i)?;

	result.append(&mut other_args);

	Ok((i, result))
}

fn parse_function_call(input: Span) -> IResult<Span, Expr> {
	let (i, pos) = position(input)?;
	let (i, func_name) = parse_identifier(i)?;

	let (i, _) = multispace0(i)?;
	let (i, arguments) = delimited(char('('), opt(parse_function_call_arguments), preceded(multispace0, char(')')))(i)?;

	let final_args = if let Some(args_inner) = arguments {
		args_inner
	} else {
		Vec::new()
	};

	Ok((i, Expr::FunctionCall {pos: pos, name: func_name, args: final_args}))
}

fn parse_expression_lowest(input: Span) -> IResult<Span, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, pos) = position(i)?;
	let (i, term) = alt((
		parse_function_call,
		map(parse_literal, |l| Expr::Literal{pos: pos, value: l}),
		delimited(char('('), parse_expression, preceded(multispace0, char(')')))
	))(i)?;

	let (i, pos) = position(i)?;
	let (i, array_access) = opt(
		delimited(preceded(multispace0, char('[')), parse_expression, preceded(multispace0, char(']')))
	)(i)?;

	if let Some(array) = array_access {
		Ok((i, Expr::BinaryExpr {pos: pos, op: BinaryOperator::ArrayAccess, lhs: Box::new(term), rhs: Box::new(array)}))
	} else {
		Ok((i, term))
	}
}

fn parse_expression_member_access(input: Span) -> IResult<Span, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, term) = parse_expression_lowest(i)?;

	let (i, operator_chain) = many0(|input: Span| {
		let (i, _) = multispace0(input)?;
		let (i, _) = char('.')(i)?;

		let (i, _) = multispace0(i)?;
		let (i, pos) = position(i)?;
		let (i, inner) = parse_expression_lowest(i)?;

		Ok((i, (pos, inner)))
	})(i)?;

	let mut output_value = term;
	for (pos, inner) in operator_chain {
		output_value = Expr::BinaryExpr {pos: pos, op: BinaryOperator::MemberAccess, lhs: Box::new(output_value), rhs: Box::new(inner)};
	}

	Ok((i, output_value))
}

fn parse_expression_unary(input: Span) -> IResult<Span, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, pos) = position(i)?;
	let (i, leading_op) = opt(alt((
		value(UnaryOperator::Negate, char('-')),
		value(UnaryOperator::Not, char('!')),
		value(UnaryOperator::BitNot, char('~'))
		)))(i)?;

	let (i, term) = parse_expression_member_access(i)?;

	if let Some(op) = leading_op {
		Ok((i, Expr::UnaryExpr {pos: pos, op: op, operand: Box::new(term)}))
	} else {
		Ok((i, term))
	}
}

fn parse_expression_multiply(input: Span) -> IResult<Span, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_unary(i)?;

	let (i, operator_chain) = many0(|input: Span| {
		let (i, _) = multispace0(input)?;
		let (i, pos) = position(i)?;
		let (i, op) = alt((
			value(BinaryOperator::Multiply, char('*')),
			value(BinaryOperator::Divide, char('/')),
			value(BinaryOperator::Modulus, char('%'))
		))(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_unary(i)?;

		Ok((i, (pos, op, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (pos, op, inner) in operator_chain {
		output_value =  Expr::BinaryExpr {pos: pos, op: op, lhs: Box::new(output_value), rhs: Box::new(inner)};
	}

	Ok((i, output_value))
}

fn parse_expression_add(input: Span) -> IResult<Span, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_multiply(i)?;

	let (i, operator_chain) = many0(|input: Span| {
		let (i, _) = multispace0(input)?;
		let (i, pos) = position(i)?;
		let (i, op) = alt((
			value(BinaryOperator::Add, char('+')),
			value(BinaryOperator::Subtract, char('-'))
		))(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_multiply(i)?;

		Ok((i, (pos, op, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (pos, op, inner) in operator_chain {
		output_value = Expr::BinaryExpr {pos: pos, op: op, lhs: Box::new(output_value), rhs: Box::new(inner)};
	}

	Ok((i, output_value))
}

fn parse_expression_bitshift(input: Span) -> IResult<Span, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_add(i)?;

	let (i, operator_chain) = many0(|input: Span| {
		let (i, _) = multispace0(input)?;
		let (i, pos) = position(i)?;
		let (i, op) = alt((
			value(BinaryOperator::BitShiftLeft, tag("<<")),
			value(BinaryOperator::BitShiftRight, tag(">>"))
		))(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_add(i)?;

		Ok((i, (pos, op, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (pos, op, inner) in operator_chain {
		output_value = Expr::BinaryExpr {pos: pos, op: op, lhs: Box::new(output_value), rhs: Box::new(inner)};
	}

	Ok((i, output_value))
}

fn parse_expression_compare(input: Span) -> IResult<Span, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_bitshift(i)?;

	let (i, operator_chain) = many0(|input: Span| {
		let (i, _) = multispace0(input)?;
		let (i, pos) = position(i)?;
		let (i, op) = alt((
			value(BinaryOperator::LessThanEqual, tag("<=")),
			value(BinaryOperator::LessThan, char('<')),
			value(BinaryOperator::GreaterThanEqual, tag(">=")),
			value(BinaryOperator::GreaterThan, char('>')),
		))(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_bitshift(i)?;

		Ok((i, (pos, op, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (pos, op, inner) in operator_chain {
		output_value = Expr::BinaryExpr {pos: pos, op: op, lhs: Box::new(output_value), rhs: Box::new(inner)};
	}

	Ok((i, output_value))
}

fn parse_expression_equality(input: Span) -> IResult<Span, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_compare(i)?;

	let (i, operator_chain) = many0(|input: Span| {
		let (i, _) = multispace0(input)?;
		let (i, pos) = position(i)?;
		let (i, op) = alt((
			value(BinaryOperator::NotEqual, tag("!=")),
			value(BinaryOperator::Equal, tag("=="))
		))(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_compare(i)?;

		Ok((i, (pos, op, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (pos, op, inner) in operator_chain {
		output_value = Expr::BinaryExpr {pos: pos, op: op, lhs: Box::new(output_value), rhs: Box::new(inner)};
	}

	Ok((i, output_value))
}

fn parse_expression_bitwise_and(input: Span) -> IResult<Span, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_equality(i)?;

	let (i, operator_chain) = many0(|input: Span| {
		let (i, _) = multispace0(input)?;
		let (i, pos) = position(i)?;
		let (i, _) = char('&')(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_equality(i)?;

		Ok((i, (pos, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (pos, inner) in operator_chain {
		output_value = Expr::BinaryExpr {pos: pos, op: BinaryOperator::BitAnd, lhs: Box::new(output_value), rhs: Box::new(inner)};
	}

	Ok((i, output_value))
}

fn parse_expression_bitwise_xor(input: Span) -> IResult<Span, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_bitwise_and(i)?;

	let (i, operator_chain) = many0(|input: Span| {
		let (i, _) = multispace0(input)?;
		let (i, pos) = position(i)?;
		let (i, _) = char('^')(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_bitwise_and(i)?;

		Ok((i, (pos, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (pos, inner) in operator_chain {
		output_value = Expr::BinaryExpr {pos: pos, op: BinaryOperator::BitXor, lhs: Box::new(output_value), rhs: Box::new(inner)};
	}

	Ok((i, output_value))
}

fn parse_expression_bitwise_or(input: Span) -> IResult<Span, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_bitwise_xor(i)?;

	let (i, operator_chain) = many0(|input: Span| {
		let (i, _) = multispace0(input)?;
		let (i, pos) = position(i)?;
		let (i, _) = char('|')(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_bitwise_xor(i)?;

		Ok((i, (pos, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (pos, inner) in operator_chain {
		output_value = Expr::BinaryExpr {pos: pos, op: BinaryOperator::BitOr, lhs: Box::new(output_value), rhs: Box::new(inner)};
	}

	Ok((i, output_value))
}

fn parse_expression_logical_and(input: Span) -> IResult<Span, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_bitwise_or(i)?;

	let (i, operator_chain) = many0(|input: Span| {
		let (i, _) = multispace0(input)?;
		let (i, pos) = position(i)?;
		let (i, _) = tag("&&")(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_bitwise_or(i)?;

		Ok((i, (pos, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (pos, inner) in operator_chain {
		output_value = Expr::BinaryExpr {pos: pos, op: BinaryOperator::And, lhs: Box::new(output_value), rhs: Box::new(inner)};
	}

	Ok((i, output_value))
}

fn parse_expression_logical_or(input: Span) -> IResult<Span, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_logical_and(i)?;

	let (i, operator_chain) = many0(|input: Span| {
		let (i, _) = multispace0(input)?;
		let (i, pos) = position(i)?;
		let (i, _) = tag("||")(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_logical_and(i)?;

		Ok((i, (pos, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (pos, inner) in operator_chain {
		output_value = Expr::BinaryExpr {pos: pos, op: BinaryOperator::Or, lhs: Box::new(output_value), rhs: Box::new(inner)};
	}

	Ok((i, output_value))
}

fn parse_expression_ternary(input: Span) -> IResult<Span, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_logical_or(i)?;

	let (i, ternary_terms) = opt(|input: Span| {
		// Note: C/C++ parse the middle expression as though it is in parenthesis, so recurse from the top here
		let (i, _) = multispace0(input)?;
		let (i, pos) = position(i)?;
		let (i, _) = char('?')(i)?;
		let (i, if_term) = parse_expression(i)?;

		let (i, _) = multispace0(i)?;
		let (i, _) = char(':')(i)?;
		let (i, else_term) = parse_expression_logical_or(i)?;

		Ok((i, (pos, if_term, else_term)))
	})(i)?;

	if let Some((pos, if_term, else_term)) = ternary_terms {
		Ok((i, Expr::Ternary {pos: pos, cond: Box::new(first_term), success: Box::new(if_term), failure: Box::new(else_term)}))
	} else {
		Ok((i, first_term))
	}
}

pub fn parse_expression(input: Span) -> IResult<Span, Expr> {
	parse_expression_ternary(input)
}
