use super::expr::*;
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

fn parse_statement_expression(input: Span) -> IResult<Span, Statement> {
	let (i, _) = multispace0(input)?;
	let (i, pos) = position(i)?;
	let (i, expr) = parse_expression(i)?;
	let (i, _) = multispace0(i)?;
	let (i, _) = tag(";")(i)?;

	Ok((i, Statement::Expr {pos: pos, expr: expr}))
}

fn parse_statement_block(input: Span) -> IResult<Span, Statement> {
	let (i, _) = multispace0(input)?;
	let (i, pos) = position(i)?;
	let (i, body) = delimited(char('{'), many0(parse_statement), preceded(multispace0, cut(char('}'))))(i)?;

	Ok((i, Statement::Block {pos: pos, body: body}))
}

fn parse_statement_assignment(input: Span) -> IResult<Span, Statement> {
	let (i, _) = multispace0(input)?;
	let (i, pos) = position(i)?;
	let (i, lhs) = parse_expression_member_access(i)?;

	let (i, _) = multispace0(i)?;
	let (i, op) = alt((
		value(AssignmentOperator::AssignAdd, tag("+=")),
		value(AssignmentOperator::AssignSubtract, tag("-=")),
		value(AssignmentOperator::AssignMultiply, tag("*=")),
		value(AssignmentOperator::AssignDivide, tag("/=")),
		value(AssignmentOperator::AssignModulus, tag("%=")),
		value(AssignmentOperator::AssignBitAnd, tag("&=")),
		value(AssignmentOperator::AssignBitOr, tag("|=")),
		value(AssignmentOperator::AssignBitXor, tag("^=")),
		value(AssignmentOperator::Assign, tag("="))
	))(i)?;

	let (i, _) = multispace0(i)?;
	let (i, rhs) = parse_expression(i)?;

	let (i, _) = multispace0(i)?;
	let (i, _) = tag(";")(i)?;

	Ok((i, Statement::Assignment {pos: pos, lhs: lhs, rhs: rhs, op: op}))
}

fn parse_statement_if(input: Span) -> IResult<Span, Statement> {
	let (i, _) = multispace0(input)?;
	let (i, pos) = position(i)?;
	let (i, _) = tag("if")(i)?;

	let (i, _) = multispace0(i)?;
	let (i, cond) = delimited(char('('), parse_expression, preceded(multispace0, cut(char(')'))))(i)?;

	let (i, _) = multispace0(i)?;
	let (i, if_body) = parse_statement(i)?;

	let (i, else_body) = opt(|input: Span| {
		let (i, _) = multispace0(input)?;
		let (i, _) = tag("else")(i)?;

		let (i, _) = multispace0(i)?;
		let (i, else_body) = parse_statement(i)?;
		Ok((i, Box::new(else_body)))
	})(i)?;

	Ok((i, Statement::If {pos: pos, cond: cond, if_body: Box::new(if_body), else_body: else_body}))
}

pub fn parse_statement(input: Span) -> IResult<Span, Statement> {
	alt((
		parse_statement_if,
		parse_statement_block,
		parse_statement_assignment,
		parse_statement_expression
	))(input)
}
