// Copyright 2019-2020 Brett Lajzer
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::*;
use super::types::*;

use nom::{
	IResult,
	branch::alt,
	bytes::complete::tag,
	character::complete::{char, multispace0},
	combinator::{cut, map, opt, value},
	multi::many0,
	sequence::{delimited, preceded}
};
use nom_locate::position;

// Precedence rules based on C:
// https://en.cppreference.com/w/c/language/operator_precedence

macro_rules! parse_single_operator {
	($name:ident, $recurse:ident, $op_str:expr, $op_type:expr $(, $visibility:ident)?) => {
		$($visibility)? fn $name(input: Span) -> IResult<Span, Expr> {
			let (i, first_term) = preceded(multispace0, $recurse)(input)?;

			let (i, operator_chain) = many0(|input: Span| {
				let (i, pos) = preceded(multispace0, position)(input)?;
				let (i, _) = tag($op_str)(i)?;

				let (i, inner) = preceded(multispace0, cut($recurse))(i)?;

				Ok((i, (pos, inner)))
			})(i)?;

			let mut output_value = first_term;
			for (pos, inner) in operator_chain {
				output_value = Expr::BinaryExpr {pos: pos, op: $op_type, lhs: Box::new(output_value), rhs: Box::new(inner)};
			}

			Ok((i, output_value))
		}
	};
}

macro_rules! parse_multi_operator {
	($name:ident, $recurse:ident, $(($op_str:expr, $op_type:expr)),+ $(, $visibility:ident)?) => {
		$($visibility)? fn $name(input: Span) -> IResult<Span, Expr> {
			let (i, first_term) = preceded(multispace0, $recurse)(input)?;

			let (i, operator_chain) = many0(|input: Span| {
				let (i, pos) = preceded(multispace0, position)(input)?;
				let (i, op) = alt((
					$(
						value($op_type, tag($op_str)),
					)+
				))(i)?;

				let (i, inner) = preceded(multispace0, cut($recurse))(i)?;

				Ok((i, (pos, op, inner)))
			})(i)?;

			let mut output_value = first_term;
			for (pos, op, inner) in operator_chain {
				output_value =  Expr::BinaryExpr {pos: pos, op: op, lhs: Box::new(output_value), rhs: Box::new(inner)};
			}

			Ok((i, output_value))
		}
	};
}

fn parse_literal(input: Span) -> IResult<Span, Literal> {
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
	let (i, first_arg) = preceded(multispace0, parse_expression)(input)?;

	let mut result = Vec::new();
	result.push(first_arg);

	let (i, mut other_args) = many0(|input: Span| {
		let (i, _) = preceded(multispace0, char(','))(input)?;
		let (i, arg) = preceded(multispace0, cut(parse_expression))(i)?;

		Ok((i, arg))
	})(i)?;

	result.append(&mut other_args);

	Ok((i, result))
}

fn parse_function_call(input: Span) -> IResult<Span, Expr> {
	let (i, pos) = position(input)?;
	let (i, func_name) = parse_identifier(i)?;

	let (i, arguments) = preceded(multispace0, delimited(char('('), opt(parse_function_call_arguments), preceded(multispace0, cut(char(')')))))(i)?;

	let final_args = if let Some(args_inner) = arguments {
		args_inner
	} else {
		Vec::new()
	};

	Ok((i, Expr::FunctionCall {pos: pos, name: func_name, args: final_args}))
}

fn parse_expression_lowest(input: Span) -> IResult<Span, Expr> {
	let (i, pos) = preceded(multispace0, position)(input)?;
	let (i, term) = alt((
		parse_function_call,
		map(parse_literal, |l| Expr::Literal{pos: pos, value: l}),
		delimited(char('('), parse_expression, preceded(multispace0, cut(char(')'))))
	))(i)?;

	Ok((i, term))
}

pub fn parse_expression_member_access<'a>(input: Span<'a>) -> IResult<Span<'a>, Expr> {
	let (i, first_term) = preceded(multispace0, parse_expression_lowest)(input)?;

	let (i, operator_chain) = many0(|input: Span<'a>| {
		let (i, pos) = preceded(multispace0, position)(input)?;
		let (i, (op, inner)) = alt((
			|input: Span<'a>| {
				let (i, _) = tag(".")(input)?;
				let (i, inner) = preceded(multispace0, cut(parse_expression_lowest))(i)?;

				Ok((i, (BinaryOperator::MemberAccess, inner)))
			},
			|input: Span<'a>| {
				let (i, inner) = delimited(char('['), parse_expression, preceded(multispace0, cut(char(']'))))(input)?;
				Ok((i, (BinaryOperator::ArrayAccess, inner)))
			},
		))(i)?;

		Ok((i, (pos, op, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (pos, op, inner) in operator_chain {
		output_value =  Expr::BinaryExpr {pos: pos, op: op, lhs: Box::new(output_value), rhs: Box::new(inner)};
	}

	Ok((i, output_value))
}

fn parse_expression_unary(input: Span) -> IResult<Span, Expr> {
	let (i, pos) = preceded(multispace0, position)(input)?;
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

parse_multi_operator!(
	parse_expression_multiply,
	parse_expression_unary,
	("*", BinaryOperator::Multiply),
	("/", BinaryOperator::Divide),
	("%", BinaryOperator::Modulus)
);

parse_multi_operator!(
	parse_expression_add,
	parse_expression_multiply,
	("+", BinaryOperator::Add),
	("-", BinaryOperator::Subtract)
);

parse_multi_operator!(
	parse_expression_bitshift,
	parse_expression_add,
	("<<", BinaryOperator::BitShiftLeft),
	(">>", BinaryOperator::BitShiftRight)
);

parse_multi_operator!(
	parse_expression_compare,
	parse_expression_bitshift,
	("<=", BinaryOperator::LessThanEqual),
	("<", BinaryOperator::LessThan),
	(">=", BinaryOperator::GreaterThanEqual),
	(">", BinaryOperator::GreaterThan)
);

parse_multi_operator!(
	parse_expression_equality,
	parse_expression_compare,
	("!=", BinaryOperator::NotEqual),
	("==", BinaryOperator::Equal)
);

parse_single_operator!(parse_expression_bitwise_and, parse_expression_equality, "&", BinaryOperator::BitAnd);
parse_single_operator!(parse_expression_bitwise_xor, parse_expression_bitwise_and, "^", BinaryOperator::BitXor);
parse_single_operator!(parse_expression_bitwise_or, parse_expression_bitwise_xor, "|", BinaryOperator::BitOr);
parse_single_operator!(parse_expression_logical_and, parse_expression_bitwise_or, "&&", BinaryOperator::And);
parse_single_operator!(parse_expression_logical_or, parse_expression_logical_and, "||", BinaryOperator::Or);

fn parse_expression_ternary(input: Span) -> IResult<Span, Expr> {
	let (i, first_term) = preceded(multispace0, parse_expression_logical_or)(input)?;

	let (i, ternary_terms) = opt(|input: Span| {
		// Note: C/C++ parse the middle expression as though it is in parenthesis, so recurse from the top here
		let (i, pos) = preceded(multispace0, position)(input)?;
		let (i, _) = char('?')(i)?;
		let (i, if_term) = cut(parse_expression)(i)?;

		let (i, _) = preceded(multispace0, cut(char(':')))(i)?;
		let (i, else_term) = cut(parse_expression_logical_or)(i)?;

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
