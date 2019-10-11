pub mod expr;
pub mod types;

use nom::{
	IResult,
	character::complete::{alpha1, alphanumeric0, digit1},
	combinator::{opt},
	number::complete::double,
};

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