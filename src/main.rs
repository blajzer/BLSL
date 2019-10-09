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
enum Statement {
	Block(Vec<Statement>),
	Expr(Box<Expr>)
}

#[derive(Debug)]
enum Expr {
	Add(Box<Expr>, Box<Expr>),
	Subtract(Box<Expr>, Box<Expr>),
	Multiply(Box<Expr>, Box<Expr>),
	Divide(Box<Expr>, Box<Expr>),
	Modulus(Box<Expr>, Box<Expr>),
	Equal(Box<Expr>, Box<Expr>),
	NotEqual(Box<Expr>, Box<Expr>),
	LessThan(Box<Expr>, Box<Expr>),
	LessThanEqual(Box<Expr>, Box<Expr>),
	GreaterThan(Box<Expr>, Box<Expr>),
	GreaterThanEqual(Box<Expr>, Box<Expr>),
	Negate(Box<Expr>),
	Not(Box<Expr>),
	And(Box<Expr>, Box<Expr>),
	Or(Box<Expr>, Box<Expr>),
	BitAnd(Box<Expr>, Box<Expr>),
	BitOr(Box<Expr>, Box<Expr>),
	BitXor(Box<Expr>, Box<Expr>),
	BitShiftLeft(Box<Expr>, Box<Expr>),
	BitShiftRight(Box<Expr>, Box<Expr>),
	BitNot(Box<Expr>),
	FunctionCall(String, Vec<Expr>),
	Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
	MemberAccess(Box<Expr>, Box<Expr>),
	ArrayAccess(Box<Expr>, Box<Expr>),
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
	Modulus,
	Negate,
	Not,
	And,
	Or,
	BitAnd,
	BitOr,
	BitXor,
	BitShiftLeft,
	BitShiftRight,
	BitNot,
	Equal,
	NotEqual,
	LessThan,
	LessThanEqual,
	GreaterThan,
	GreaterThanEqual
}

// TODO: fix precedence
// https://en.cppreference.com/w/c/language/operator_precedence

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
	let (i, arguments) = delimited(char('('), opt(parse_function_call_arguments), preceded(multispace0, char(')')))(i)?;

	let final_args = if let Some(args_inner) = arguments {
		args_inner
	} else {
		Vec::new()
	};

	Ok((i, Expr::FunctionCall(func_name, final_args)))
}

fn parse_expression_lowest(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, term) = alt((
		parse_function_call,
		map(parse_literal, |l| Expr::Literal(l)),
		delimited(char('('), parse_expression, preceded(multispace0, char(')')))
	))(i)?;

	let (i, array_access) = opt(
		delimited(preceded(multispace0, char('[')), parse_expression, preceded(multispace0, char(']')))
	)(i)?;

	if let Some(array) = array_access {
		Ok((i, Expr::ArrayAccess(Box::new(term), Box::new(array))))
	} else {
		Ok((i, term))
	}
}

fn parse_expression_member_access(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, term) = parse_expression_lowest(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, _) = char('.')(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_lowest(i)?;

		Ok((i, inner))
	})(i)?;

	let mut output_value = term;
	for inner in operator_chain {
		output_value = Expr::MemberAccess(Box::new(output_value), Box::new(inner));
	}

	Ok((i, output_value))
}

fn parse_expression_unary(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, leading_op) = opt(alt((
		value(Operator::Negate, char('-')),
		value(Operator::Not, char('!'))
		)))(i)?;

	let (i, term) = parse_expression_member_access(i)?;

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
			value(Operator::Multiply, char('*')),
			value(Operator::Divide, char('/')),
			value(Operator::Modulus, char('%'))
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
			Operator::Modulus => Expr::Modulus(Box::new(output_value), Box::new(inner)),
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
			value(Operator::Add, char('+')),
			value(Operator::Subtract, char('-'))
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

fn parse_expression_bitshift(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_add(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, op) = alt((
			value(Operator::BitShiftLeft, tag("<<")),
			value(Operator::BitShiftRight, tag(">>"))
		))(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_add(i)?;

		Ok((i, (op, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (op, inner) in operator_chain {
		output_value = match op {
			Operator::BitShiftLeft => Expr::BitShiftLeft(Box::new(output_value), Box::new(inner)),
			Operator::BitShiftRight => Expr::BitShiftRight(Box::new(output_value), Box::new(inner)),
			_ => panic!("unhandled operator in parse_expression_bitshift")
		};
	}

	Ok((i, output_value))
}

fn parse_expression_compare(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_bitshift(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, op) = alt((
			value(Operator::LessThanEqual, tag("<=")),
			value(Operator::LessThan, char('<')),
			value(Operator::GreaterThanEqual, tag(">=")),
			value(Operator::GreaterThan, char('>')),
		))(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_bitshift(i)?;

		Ok((i, (op, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (op, inner) in operator_chain {
		output_value = match op {
			Operator::LessThanEqual => Expr::LessThanEqual(Box::new(output_value), Box::new(inner)),
			Operator::LessThan => Expr::LessThan(Box::new(output_value), Box::new(inner)),
			Operator::GreaterThanEqual => Expr::GreaterThanEqual(Box::new(output_value), Box::new(inner)),
			Operator::GreaterThan => Expr::GreaterThan(Box::new(output_value), Box::new(inner)),
			_ => panic!("unhandled operator in parse_expression_compare")
		};
	}

	Ok((i, output_value))
}

fn parse_expression_equality(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_compare(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, op) = alt((
			value(Operator::NotEqual, tag("!=")),
			value(Operator::Equal, tag("=="))
		))(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_compare(i)?;

		Ok((i, (op, inner)))
	})(i)?;

	let mut output_value = first_term;
	for (op, inner) in operator_chain {
		output_value = match op {
			Operator::NotEqual => Expr::NotEqual(Box::new(output_value), Box::new(inner)),
			Operator::Equal => Expr::Equal(Box::new(output_value), Box::new(inner)),
			_ => panic!("unhandled operator in parse_expression_equality")
		};
	}

	Ok((i, output_value))
}

fn parse_expression_bitwise_and(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_equality(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, _) = char('&')(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_equality(i)?;

		Ok((i, inner))
	})(i)?;

	let mut output_value = first_term;
	for inner in operator_chain {
		output_value = Expr::BitAnd(Box::new(output_value), Box::new(inner));
	}

	Ok((i, output_value))
}

fn parse_expression_bitwise_xor(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_bitwise_and(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, _) = char('^')(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_bitwise_and(i)?;

		Ok((i, inner))
	})(i)?;

	let mut output_value = first_term;
	for inner in operator_chain {
		output_value = Expr::BitXor(Box::new(output_value), Box::new(inner));
	}

	Ok((i, output_value))
}

fn parse_expression_bitwise_or(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_bitwise_xor(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, _) = char('|')(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_bitwise_xor(i)?;

		Ok((i, inner))
	})(i)?;

	let mut output_value = first_term;
	for inner in operator_chain {
		output_value = Expr::BitOr(Box::new(output_value), Box::new(inner));
	}

	Ok((i, output_value))
}

fn parse_expression_logical_and(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_bitwise_or(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, _) = tag("&&")(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_bitwise_or(i)?;

		Ok((i, inner))
	})(i)?;

	let mut output_value = first_term;
	for inner in operator_chain {
		output_value = Expr::And(Box::new(output_value), Box::new(inner));
	}

	Ok((i, output_value))
}

fn parse_expression_logical_or(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_logical_and(i)?;

	let (i, operator_chain) = many0(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, _) = tag("||")(i)?;

		let (i, _) = multispace0(i)?;
		let (i, inner) = parse_expression_logical_and(i)?;

		Ok((i, inner))
	})(i)?;

	let mut output_value = first_term;
	for inner in operator_chain {
		output_value = Expr::Or(Box::new(output_value), Box::new(inner));
	}

	Ok((i, output_value))
}

fn parse_expression_ternary(input: &str) -> IResult<&str, Expr> {
	let (i, _) = multispace0(input)?;
	let (i, first_term) = parse_expression_logical_or(i)?;

	let (i, ternary_terms) = opt(|input: &str| {
		let (i, _) = multispace0(input)?;
		let (i, _) = char('?')(i)?;
		let (i, if_term) = parse_expression_logical_or(i)?;

		let (i, _) = multispace0(i)?;
		let (i, _) = char(':')(i)?;
		let (i, else_term) = parse_expression_logical_or(i)?;

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
	//println!("parsed {:?}", parse_expression("  myFun(a > b ? 4 + x : 5)"));

	//println!("parsed {:?}", parse_expression("  myFun(a.y.z > b.x ? 4 + x : 5).a"));
	println!("parsed {:?}", parse_expression(" b[5 + 4].z >> 5 | 4"));

	/*if let Ok((i,e)) = parse_expression(" myFun((- a  ), 5) * ( ! 5 / 6)*( 4 + 3 ) ") {
		pretty_print_expr(&e);
		println!("");
	}*/
}



// TODO: precendence rules so we know where to put parens
/*fn pretty_print_expr(e: &Expr) {
	match e {
		Expr::Add(lhs, rhs) => {
			pretty_print_expr(lhs);
			print!("+");
			pretty_print_expr(rhs);
		},
		Expr::Subtract(lhs, rhs) => {
			pretty_print_expr(lhs);
			print!("-");
			pretty_print_expr(rhs);
		},
		Expr::Multiply(lhs, rhs) => {
			pretty_print_expr(lhs);
			print!("*");
			pretty_print_expr(rhs);
		},
		Expr::Divide(lhs, rhs) => {
			pretty_print_expr(lhs);
			print!("/");
			pretty_print_expr(rhs);
		},
		Expr::Equal(lhs, rhs) => {
			pretty_print_expr(lhs);
			print!("==");
			pretty_print_expr(rhs);
		},
		Expr::NotEqual(lhs, rhs) => {
			pretty_print_expr(lhs);
			print!("!=");
			pretty_print_expr(rhs);
		},
		Expr::LessThan(lhs, rhs) => {
			pretty_print_expr(lhs);
			print!("<");
			pretty_print_expr(rhs);
		},
		Expr::LessThanEqual(lhs, rhs) => {
			pretty_print_expr(lhs);
			print!("<=");
			pretty_print_expr(rhs);
		},
		Expr::GreaterThan(lhs, rhs) => {
			pretty_print_expr(lhs);
			print!(">");
			pretty_print_expr(rhs);
		},
		Expr::GreaterThanEqual(lhs, rhs) => {
			pretty_print_expr(lhs);
			print!(">=");
			pretty_print_expr(rhs);
		},
		Expr::Negate(arg) => {
			print!("-");
			pretty_print_expr(arg);
		},
		Expr::Not(arg) => {
			print!("!");
			pretty_print_expr(arg);
		},
		Expr::FunctionCall(name, arguments) => {
			print!("{}(", name);
			let mut arg_iter = arguments.iter();
			if let Some(arg) = arg_iter.next() {
				pretty_print_expr(arg);
				
				for a in arg_iter {
					print!(",");
					pretty_print_expr(a);
				}
			}
			print!(")");
		},
		Expr::Ternary(cond, if_exp, else_exp) => {
			pretty_print_expr(cond);
			print!("?");
			pretty_print_expr(if_exp);
			print!(":");
			pretty_print_expr(else_exp);
		},
		Expr::MemberAccess(lhs, rhs) => {
			pretty_print_expr(lhs);
			print!(".");
			pretty_print_expr(rhs);
		},
		Expr::ArrayAccess(lhs, rhs) => {
			pretty_print_expr(lhs);
			print!("[");
			pretty_print_expr(rhs);
			print!("]");
		},
		Expr::Literal(l) => {
			match l {
				Literal::Float(f) => print!("{}", f),
				Literal::Identifier(i) => print!("{}", i),
				Literal::Int(i) => print!("{}", i),
				Literal::UInt(u) => print!("{}", u)
			}
		}
	}
}
*/