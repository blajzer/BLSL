// Copyright 2019-2020 Brett Lajzer
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

pub mod definition;
pub mod expr;
pub mod statement;
pub mod types;

#[cfg(test)]
pub mod tests;

use types::*;

use nom::{
	IResult,
	branch::alt,
	bytes::complete::tag,
	character::complete::{
		alpha1,
		alphanumeric1,
		anychar,
		char,
		digit1,
		hex_digit1,
		line_ending,
		multispace0,
		multispace1},
	combinator::{cut, opt},
	error::{ErrorKind, ParseError},
	Err::Error,
	multi::{fold_many0, fold_many1, many_till},
	sequence::{preceded, terminated}
};
use nom_locate::position;

fn parse_identifier(input: Span) -> IResult<Span, String> {
	let (i, first) = fold_many1(alt((alpha1, tag("_"))), String::new(), |mut acc, s: Span| { acc.push_str(s.fragment); acc })(input)?;
	let (i, second) = fold_many0(alt((alphanumeric1, tag("_"))), first, |mut acc, s: Span| { acc.push_str(s.fragment); acc })(i)?;

	Ok((i, second))
}

fn consume_comment(input: Span) -> IResult<Span, ()> {
	let single_line_comment = preceded(tag("//"), many_till(anychar, line_ending));
	let multi_line_comment = preceded(tag("/*"), many_till(anychar, tag("*/")));
	
	let (i, _) = opt(alt((single_line_comment, multi_line_comment)))(input)?;
	Ok((i, ()))
}

fn whitespace0(input: Span) -> IResult<Span, ()> {
	let mut old_i = input;
	loop {
		let (i, _) = multispace0(old_i)?;
		let (i, _) = consume_comment(i)?;
		if i == old_i {
			return Ok((i, ()));
		} else {
			old_i = i;
		}
	}
}

enum SomeInt {
	U64(u64),
	I64(i64)
}

fn parse_double(input: Span) -> IResult<Span, f64> {
	let (i, leading_digits) = digit1(input)?;
	let (i, _) = char('.')(i)?;
	let (i, trailing_digits) = cut(digit1)(i)?;
	let (i, scientific) = opt(|input: Span| {
		let (i, _) = char('e')(input)?;
		let (i, sign) = opt(alt((tag("-"), tag("+"))))(i)?;
		let (i, digits) = cut(digit1)(i)?;

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
	let (i, digits) = cut(hex_digit1)(i)?;

	if let Ok(num) = u64::from_str_radix(digits.fragment, 16) {
		Ok((i, num))
	} else {
		Err(Error(ParseError::from_error_kind(i, ErrorKind::HexDigit)))
	}
}

fn parse_type_decl(input: Span) -> IResult<Span, TypeDecl> {
	let (i, _) = multispace0(input)?;
	let (i, pos) = position(i)?;
	
	let (i, maybe_const) = opt(terminated(tag("const"), multispace1))(i)?;
	let (i, maybe_ref) = opt(terminated(tag("ref"), multispace1))(i)?;

	let (i, name) = parse_identifier(i)?;

	let decl = TypeDecl {
		pos: pos,
		name: name.to_string(),
		path: vec!(),
		is_ref: maybe_ref.is_some(),
		is_const: maybe_const.is_some()
	};

	Ok((i, decl))
}

pub fn expression_to_string(e: &Expr) -> String {
	let mut output = String::new();

	match e {
		Expr::BinaryExpr {pos:_, op, lhs, rhs} => {
			output.push_str("(");
			output.push_str(expression_to_string(&lhs).as_str());
			let mut array_access = false;
			match op {
				BinaryOperator::Add => output.push_str("+"),
				BinaryOperator::Subtract => output.push_str("-"),
				BinaryOperator::Multiply => output.push_str("*"),
				BinaryOperator::Divide => output.push_str("/"),
				BinaryOperator::Modulus => output.push_str("%"),
				BinaryOperator::And => output.push_str("&&"),
				BinaryOperator::Or => output.push_str("||"),
				BinaryOperator::BitAnd => output.push_str("&"),
				BinaryOperator::BitOr => output.push_str("|"),
				BinaryOperator::BitXor => output.push_str("^"),
				BinaryOperator::BitShiftLeft => output.push_str("<<"),
				BinaryOperator::BitShiftRight => output.push_str(">>"),
				BinaryOperator::Equal => output.push_str("=="),
				BinaryOperator::NotEqual => output.push_str("!="),
				BinaryOperator::LessThan => output.push_str("<"),
				BinaryOperator::LessThanEqual => output.push_str("<="),
				BinaryOperator::GreaterThan => output.push_str(">"),
				BinaryOperator::GreaterThanEqual => output.push_str(">="),
				BinaryOperator::MemberAccess => output.push_str("."),
				BinaryOperator::ArrayAccess => {
					output.push_str("[");
					output.push_str(expression_to_string(&rhs).as_str());
					output.push_str("]");
					array_access = true;
				}
			}
			if !array_access {
				output.push_str(expression_to_string(&rhs).as_str());
			}
			output.push_str(")");
		},
		Expr::UnaryExpr {pos:_, op, operand} => {
			output.push_str("(");
			match op {
				UnaryOperator::Negate => output.push_str("-"),
				UnaryOperator::Not => output.push_str("!"),
				UnaryOperator::BitNot => output.push_str("~")
			}
			output.push_str(expression_to_string(&operand).as_str());
			output.push_str(")");
		},
		Expr::FunctionCall {pos:_, name, args} => {
			output.push_str(name.as_str());
			output.push_str("(");
			for (i, e) in args.iter().enumerate() {
				output.push_str(expression_to_string(e).as_str());
				if i != args.len() - 1 {
					output.push_str(",");
				}
			}
			output.push_str(")");
		},
		Expr::Ternary {pos:_, cond, success, failure} => {
			output.push_str("(");
			output.push_str(expression_to_string(&cond).as_str());
			output.push_str("?");
			output.push_str(expression_to_string(&success).as_str());
			output.push_str(":");
			output.push_str(expression_to_string(&failure).as_str());
			output.push_str(")");
		},
		Expr::Literal {pos:_, value} => {
			match value {
				Literal::Int(i) => output.push_str(i.to_string().as_str()),
				Literal::UInt(u) => output.push_str(u.to_string().as_str()),
				Literal::Float(f) => output.push_str(f.to_string().as_str()),
				Literal::Identifier(i) => output.push_str(i.as_str()),
				Literal::Bool(b) => output.push_str(b.to_string().as_str())
			}
		}
	}

	output
}

