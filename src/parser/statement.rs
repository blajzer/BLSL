// Copyright 2019-2020 Brett Lajzer
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::*;
use super::expr::*;
use super::types::*;

use nom::{
	IResult,
	branch::alt,
	bytes::complete::tag,
	character::complete::{char},
	combinator::{cut, map, opt, value},
	multi::many0,
	sequence::{delimited, preceded}
};

use nom_locate::position;

fn parse_statement_expression(input: Span) -> IResult<Span, Statement> {
	let (i, pos) = preceded(whitespace0, position)(input)?;
	let (i, expr) = parse_expression(i)?;
	let (i, _) = preceded(whitespace0, tag(";"))(i)?;

	Ok((i, Statement::Expr {pos: pos, expr: expr}))
}

fn parse_statement_block(input: Span) -> IResult<Span, Statement> {
	let (i, _) = whitespace0(input)?;
	let (i, pos) = position(i)?;
	let (i, body) = delimited(char('{'), many0(parse_statement), preceded(whitespace0, cut(char('}'))))(i)?;

	Ok((i, Statement::Block {pos: pos, body: body}))
}

fn parse_statement_assignment(input: Span) -> IResult<Span, Statement> {
	let (i, pos) = preceded(whitespace0, position)(input)?;
	let (i, lhs) = parse_expression_member_access(i)?;

	let (i, op) = preceded(whitespace0, alt((
		value(AssignmentOperator::AssignAdd, tag("+=")),
		value(AssignmentOperator::AssignSubtract, tag("-=")),
		value(AssignmentOperator::AssignMultiply, tag("*=")),
		value(AssignmentOperator::AssignDivide, tag("/=")),
		value(AssignmentOperator::AssignModulus, tag("%=")),
		value(AssignmentOperator::AssignBitAnd, tag("&=")),
		value(AssignmentOperator::AssignBitOr, tag("|=")),
		value(AssignmentOperator::AssignBitXor, tag("^=")),
		value(AssignmentOperator::Assign, tag("="))
	)))(i)?;

	let (i, rhs) = preceded(whitespace0, parse_expression)(i)?;
	let (i, _) = preceded(whitespace0, tag(";"))(i)?;

	Ok((i, Statement::Assignment {pos: pos, lhs: lhs, rhs: rhs, op: op}))
}

fn parse_statement_variable_decl(input: Span) -> IResult<Span, Statement> {
	let (i, pos) = preceded(whitespace0, position)(input)?;
	let (i, decl) = preceded(whitespace0, parse_type_decl)(i)?;
	let (i, name) = preceded(whitespace1, parse_identifier)(i)?;

	let (i, maybe_init) = opt(|input: Span| {
		let (i, _) = preceded(whitespace0, tag("="))(input)?;
		let (i, expr) = parse_expression(i)?;
		Ok((i, expr))
	})(i)?;

	let (i, _) = preceded(whitespace0, tag(";"))(i)?;

	let decl = Statement::VariableDeclaration {
		pos: pos,
		name: name,
		var_type: decl,
		initialization: maybe_init
	};

	Ok((i, decl))
}

fn parse_statement_if(input: Span) -> IResult<Span, Statement> {
	let (i, pos) = preceded(whitespace0, position)(input)?;
	let (i, _) = tag("if")(i)?;

	let (i, cond) = preceded(whitespace0, delimited(char('('), parse_expression, preceded(whitespace0, cut(char(')')))))(i)?;
	let (i, if_body) = preceded(whitespace0, parse_statement)(i)?;

	let (i, else_body) = opt(|input: Span| {
		let (i, _) = preceded(whitespace0, tag("else"))(input)?;
		let (i, else_body) = preceded(whitespace0, parse_statement)(i)?;
		Ok((i, Box::new(else_body)))
	})(i)?;

	Ok((i, Statement::If {pos: pos, cond: cond, if_body: Box::new(if_body), else_body: else_body}))
}

fn parse_statement_return(input: Span) -> IResult<Span, Statement> {
	let (i, pos) = preceded(whitespace0, position)(input)?;
	let (i, _) = preceded(whitespace0, tag("return"))(i)?;

	let (i, maybe_expr) = opt(preceded(whitespace1, parse_expression))(i)?;

	let (i, _) = preceded(whitespace0, tag(";"))(i)?;

	Ok((i, Statement::Return {pos: pos, expr: maybe_expr}))
}

fn parse_statement_break(input: Span) -> IResult<Span, Statement> {
	let (i, pos) = preceded(whitespace0, position)(input)?;
	let (i, _) = preceded(whitespace0, tag("break"))(i)?;
	let (i, _) = preceded(whitespace0, tag(";"))(i)?;

	Ok((i, Statement::Break {pos: pos}))
}

fn parse_statement_continue(input: Span) -> IResult<Span, Statement> {
	let (i, pos) = preceded(whitespace0, position)(input)?;
	let (i, _) = preceded(whitespace0, tag("continue"))(i)?;
	let (i, _) = preceded(whitespace0, tag(";"))(i)?;

	Ok((i, Statement::Continue {pos: pos}))
}

pub fn parse_statement(input: Span) -> IResult<Span, Statement> {
	alt((
		parse_statement_if,
		parse_statement_block,
		parse_statement_break,
		parse_statement_continue,
		parse_statement_return,
		parse_statement_variable_decl,
		parse_statement_assignment,
		parse_statement_expression
	))(input)
}
