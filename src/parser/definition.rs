// Copyright 2019-2020 Brett Lajzer
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

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

fn parse_variant_definition(input: Span) -> IResult<Span, Definition> {
	let (i, pos) = preceded(multispace0, position)(input)?;
	let (i, _) = tag("variant")(i)?;
	let (i, name) = preceded(multispace1, parse_identifier)(i)?;
	let (i, maybe_values) = opt(delimited(
		preceded(multispace0, tag("{")),
		separated_list(preceded(multispace0, tag(",")), parse_integer),
		preceded(multispace0, tag("}")),
	))(i)?;
	
	let values = maybe_values.unwrap_or(vec!()).iter().map(|v| {
		match v {
			SomeInt::I64(i) => *i,
			SomeInt::U64(u) => *u as i64
		}
	}).collect();

	Ok((i, Definition::Variant {
		pos: pos,
		name: name,
		values: values
	}))
}

pub fn parse_definition(input: Span) -> IResult<Span, Definition> {
	alt((
		parse_variant_definition,
		parse_function_definition
	))(input)
}
