extern crate nom;

use nom::{
	IResult,
	branch::alt,
	bytes::complete::tag,
	character::complete::{alpha1, alphanumeric0, char, digit1, multispace0},
	combinator::{map, opt, value},
	multi::many0,
	number::complete::double,
	sequence::{delimited, preceded}
};

#[derive(Debug)]
enum Expr {
	Add(Box<Expr>, Box<Expr>),
	Subtract(Box<Expr>, Box<Expr>),
	Multiply(Box<Expr>, Box<Expr>),
	Divide(Box<Expr>, Box<Expr>),
	Equal(Box<Expr>, Box<Expr>),
	NotEqual(Box<Expr>, Box<Expr>),
	LessThan(Box<Expr>, Box<Expr>),
	LessThanEqual(Box<Expr>, Box<Expr>),
	GreaterThan(Box<Expr>, Box<Expr>),
	GreaterThanEqual(Box<Expr>, Box<Expr>),
	Negate(Box<Expr>),
	Not(Box<Expr>),
	FunctionCall(String, Vec<Expr>),
	Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
	Literal(Literal)
}

#[derive(Debug)]
enum Literal {
	Int(i64),
	UInt(u64),
	Float(f64),
	Identifier(String)
}

// Internal type used during parsing
#[derive(Clone)]
enum Operator {
	Add,
	Subtract,
	Multiply,
	Divide,
	Negate,
	Not,
	Equal,
	NotEqual,
	LessThan,
	LessThanEqual,
	GreaterThan,
	GreaterThanEqual
}

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
		map(parse_identifier, |s: String| Literal::Identifier(s))
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

fn parse_function_call_arguments(input: &str) -> IResult<&str, Vec<Expr>> {
	let (i, _) = multispace0(input)?;

	let (i, first_arg) = parse_expression(i)?;

	let mut result = Vec::new();
	result.push(first_arg);

	let (i, mut other_args) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, _) = char(',')(i)?;

		let (i, _) = multispace0(i)?;
		let (i, arg) = parse_expression(i)?;

		Ok((i, arg))
	})(i)?;

	result.append(&mut other_args);

	Ok((i, result))
}

fn parse_function_call(input: &str) -> IResult<&str, Expr> {
	let (i, func_name) = parse_identifier(input)?;

	let (i, _) = multispace0(i)?;
	let (i, arguments) = delimited(tag("("), opt(parse_function_call_arguments), preceded(multispace0, tag(")")))(i)?;

	let final_args = if let Some(args_inner) = arguments {
		args_inner
	} else {
		Vec::new()
	};

	Ok((i, Expr::FunctionCall(func_name, final_args)))
}

fn parse_expression_unary(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, leading_op) = opt(alt((
		value(Operator::Negate, tag("-")),
		value(Operator::Not, tag("!"))
		)))(i)?;

	let (i, _) = multispace0(i)?;
	let (i, term) = alt((
		parse_function_call,
		map(parse_literal, |l| Expr::Literal(l)),
		delimited(tag("("), parse_expression, preceded(multispace0, tag(")")))
	))(i)?;

	if let Some(op) = leading_op {
		let op_expr = match op {
			Operator::Negate => Expr::Negate(Box::new(term)),
			Operator::Not => Expr::Not(Box::new(term)),
			_ => panic!("unhandled operator in parse_expression_unary")
		};

		Ok((i, op_expr))
	} else {
		Ok((i, term))
	}
}

fn parse_expression_multiply(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_unary(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, op) = alt((
			value(Operator::Multiply, tag("*")),
			value(Operator::Divide, tag("/"))
		))(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_unary(i)?;

		Ok((i, (op, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (op, inner) in operator_chain {
		output_value = match op {
			Operator::Multiply => Expr::Multiply(Box::new(output_value), Box::new(inner)),
			Operator::Divide => Expr::Divide(Box::new(output_value), Box::new(inner)),
			_ => panic!("unhandled operator in parse_expression_multiply")
		};
	}

	Ok((i, output_value))
}

fn parse_expression_add(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_multiply(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, op) = alt((
			value(Operator::Add, tag("+")),
			value(Operator::Subtract, tag("-"))
		))(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_multiply(i)?;

		Ok((i, (op, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (op, inner) in operator_chain {
		output_value = match op {
			Operator::Add => Expr::Add(Box::new(output_value), Box::new(inner)),
			Operator::Subtract => Expr::Subtract(Box::new(output_value), Box::new(inner)),
			_ => panic!("unhandled operator in parse_expression_add")
		};
	}

	Ok((i, output_value))
}

fn parse_expression_compare(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_add(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, op) = alt((
			value(Operator::NotEqual, tag("!=")),
			value(Operator::Equal, tag("==")),
			value(Operator::LessThanEqual, tag("<=")),
			value(Operator::LessThan, tag("<")),
			value(Operator::GreaterThanEqual, tag(">=")),
			value(Operator::GreaterThan, tag(">"))
		))(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_add(i)?;

		Ok((i, (op, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (op, inner) in operator_chain {
		output_value = match op {
			Operator::NotEqual => Expr::NotEqual(Box::new(output_value), Box::new(inner)),
			Operator::Equal => Expr::Equal(Box::new(output_value), Box::new(inner)),
			Operator::LessThanEqual => Expr::LessThanEqual(Box::new(output_value), Box::new(inner)),
			Operator::LessThan => Expr::LessThan(Box::new(output_value), Box::new(inner)),
			Operator::GreaterThanEqual => Expr::GreaterThanEqual(Box::new(output_value), Box::new(inner)),
			Operator::GreaterThan => Expr::GreaterThan(Box::new(output_value), Box::new(inner)),
			_ => panic!("unhandled operator in parse_expression_compare")
		};
	}

	Ok((i, output_value))
}

fn parse_expression_ternary(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_compare(i)?;

	let (i, ternary_terms) = opt(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, _) = char('?')(i)?;
		let (i, if_term) = parse_expression_compare(i)?;

		let (i, _) = multispace0(i)?;
		let (i, _) = char(':')(i)?;
		let (i, else_term) = parse_expression_compare(i)?;

		Ok((i, (if_term, else_term)))
	})(i)?;

	if let Some((if_term, else_term)) = ternary_terms {
		Ok((i, Expr::Ternary(Box::new(first_term), Box::new(if_term), Box::new(else_term))))
	} else {
		Ok((i, first_term))
	}
}

fn parse_expression(input: &str) -> IResult<&str, Expr> {
	parse_expression_ternary(input)
}

fn main() {
	//println!("parsed {:?}", parse_expression("  myFun((- a  ), 5) * ( ! 5 / 6)*( 4 + 3 )  "));
	println!("parsed {:?}", parse_expression("  myFun(a > b ? 4 + x : 5)"));
}
