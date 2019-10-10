pub mod expr;
pub mod types;

use types::*;

use nom::{
	IResult,
	branch::alt,
	character::complete::{alpha1, alphanumeric0, digit1},
	combinator::{map, opt},
	number::complete::double,
};


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

// todo: underscores
fn parse_identifier(input: &str) -> IResult<&str, String> {
	let (i, first) = alpha1(input)?;
	let (i, second) = opt(alphanumeric0)(i)?;

	let mut result = first.to_string();
	if let Some(second_inner) = second {
		result.push_str(second_inner);
	}

	Ok((i, result))
}