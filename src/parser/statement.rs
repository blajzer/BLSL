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



fn parse_statement(input: &str) -> IResult<&str, Statement> {

}
