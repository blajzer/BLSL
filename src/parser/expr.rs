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

// Precedence rules based on C:
// https://en.cppreference.com/w/c/language/operator_precedence

fn parse_literal(input: &str) -> IResult<&str, Literal> {
	// TODO: fix all of this. it doesn't greedily handle floats properly...
	// i.e. it thinks everything is a float (-_-)
	alt((
		map(double, |f: f64| Literal::Float(f)),
		map(digit1, |s: &str| {
			if let Ok(i) = s.parse::<i64>() {
				Literal::Int(i)
			} else if let Ok(i) = s.parse::<u64>() {
				Literal::UInt(i)
			} else {
				Literal::UInt(0) // This could be better...
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
		Ok((i, Expr::BinaryExpr(BinaryOperator::ArrayAccess, Box::new(term), Box::new(array))))
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
		output_value = Expr::BinaryExpr(BinaryOperator::MemberAccess, Box::new(output_value), Box::new(inner));
	}

	Ok((i, output_value))
}

fn parse_expression_unary(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, leading_op) = opt(alt((
		value(UnaryOperator::Negate, char('-')),
		value(UnaryOperator::Not, char('!')),
		value(UnaryOperator::BitNot, char('~'))
		)))(i)?;

	let (i, term) = parse_expression_member_access(i)?;

	if let Some(op) = leading_op {
		Ok((i, Expr::UnaryExpr(op, Box::new(term))))
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
			value(BinaryOperator::Multiply, char('*')),
			value(BinaryOperator::Divide, char('/')),
			value(BinaryOperator::Modulus, char('%'))
		))(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_unary(i)?;

		Ok((i, (op, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (op, inner) in operator_chain {
		output_value =  Expr::BinaryExpr(op, Box::new(output_value), Box::new(inner));
	}

	Ok((i, output_value))
}

fn parse_expression_add(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_multiply(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, op) = alt((
			value(BinaryOperator::Add, char('+')),
			value(BinaryOperator::Subtract, char('-'))
		))(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_multiply(i)?;

		Ok((i, (op, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (op, inner) in operator_chain {
		output_value = Expr::BinaryExpr(op, Box::new(output_value), Box::new(inner));
	}

	Ok((i, output_value))
}

fn parse_expression_bitshift(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_add(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, op) = alt((
			value(BinaryOperator::BitShiftLeft, tag("<<")),
			value(BinaryOperator::BitShiftRight, tag(">>"))
		))(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_add(i)?;

		Ok((i, (op, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (op, inner) in operator_chain {
		output_value = Expr::BinaryExpr(op, Box::new(output_value), Box::new(inner));
	}

	Ok((i, output_value))
}

fn parse_expression_compare(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_bitshift(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, op) = alt((
			value(BinaryOperator::LessThanEqual, tag("<=")),
			value(BinaryOperator::LessThan, char('<')),
			value(BinaryOperator::GreaterThanEqual, tag(">=")),
			value(BinaryOperator::GreaterThan, char('>')),
		))(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_bitshift(i)?;

		Ok((i, (op, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (op, inner) in operator_chain {
		output_value = Expr::BinaryExpr(op, Box::new(output_value), Box::new(inner));
	}

	Ok((i, output_value))
}

fn parse_expression_equality(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_compare(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, op) = alt((
			value(BinaryOperator::NotEqual, tag("!=")),
			value(BinaryOperator::Equal, tag("=="))
		))(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_compare(i)?;

		Ok((i, (op, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (op, inner) in operator_chain {
		output_value = Expr::BinaryExpr(op, Box::new(output_value), Box::new(inner));
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
		output_value = Expr::BinaryExpr(BinaryOperator::BitAnd, Box::new(output_value), Box::new(inner));
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
		output_value = Expr::BinaryExpr(BinaryOperator::BitXor, Box::new(output_value), Box::new(inner));
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
		output_value = Expr::BinaryExpr(BinaryOperator::BitOr, Box::new(output_value), Box::new(inner));
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
		output_value = Expr::BinaryExpr(BinaryOperator::And, Box::new(output_value), Box::new(inner));
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
		output_value = Expr::BinaryExpr(BinaryOperator::Or, Box::new(output_value), Box::new(inner));
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
