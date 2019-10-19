use super::*;
use super::statement::*;
use super::types::*;

use nom::{
	IResult,
	branch::alt,
	bytes::complete::tag,
	character::complete::{char, multispace0},
	combinator::{cut, map, opt, value},
	multi::{many0, separated_list},
	sequence::{delimited, preceded}
};
use nom_locate::position;

fn parse_function_parameter(input: Span) -> IResult<Span, FunctionParam> {
	let (i, pos) = preceded(multispace0, position)(input)?;
	let (i, type_decl) = preceded(multispace0, parse_type_decl)(i)?;
	let (i, name) = preceded(multispace0, parse_identifier)(i)?;

	Ok((i, FunctionParam {pos: pos, name: name, type_name: type_decl}))
}

fn parse_function_definition(input: Span) -> IResult<Span, Definition> {
	let (i, pos) = preceded(multispace0, position)(input)?;
	let (i, _) = tag("func")(i)?;
	let (i, name) = preceded(multispace0, cut(parse_identifier))(i)?;
	let (i, params) = preceded(multispace0, delimited(
		tag("("), 
		separated_list(preceded(multispace0, tag(",")), parse_function_parameter),
		preceded(multispace0, cut(tag(")")))))(i)?;
	let (i, maybe_return) = opt(|input: Span| {
		let (i, _) = preceded(multispace0, tag("->"))(input)?;
		let (i, return_type) = cut(parse_type_decl)(i)?;
		Ok((i, return_type))
	})(i)?;
	let (i, body) = preceded(multispace0, parse_statement)(i)?;

	Ok((i, Definition::Function {
		pos: pos, 
		name: name,
		params: params,
		ret: maybe_return,
		body: body
		}))
}

pub fn parse_definition(input: Span) -> IResult<Span, Definition> {
	parse_function_definition(input)
}