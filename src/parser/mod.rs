pub mod expr;
pub mod types;

use types::*;

use nom::{
	IResult,
	character::complete::{alpha1, alphanumeric0, digit1},
	combinator::{opt},
};

// todo: underscores
fn parse_identifier(input: Span) -> IResult<Span, String> {
	let (i, first) = alpha1(input)?;
	let (i, second) = opt(alphanumeric0)(i)?;

	let mut result = first.to_string();
	if let Some(second_inner) = second {
		result.push_str(second_inner.fragment);
	}

	Ok((i, result))
}