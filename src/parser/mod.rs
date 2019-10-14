pub mod expr;
pub mod statement;
pub mod types;

#[cfg(test)]
pub mod tests;

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

pub fn expression_to_string(e: &Expr) -> String {
	let mut output = String::new();

	match e {
		Expr::BinaryExpr {pos:_, op, lhs, rhs} => {
			output.push_str("(");
			output.push_str(expression_to_string(&lhs).as_str());
			let mut array_access = false;
			match op {
				BinaryOperator::Add => output.push_str("+"),
				BinaryOperator::Subtract => output.push_str("-"),
				BinaryOperator::Multiply => output.push_str("*"),
				BinaryOperator::Divide => output.push_str("/"),
				BinaryOperator::Modulus => output.push_str("%"),
				BinaryOperator::And => output.push_str("&&"),
				BinaryOperator::Or => output.push_str("||"),
				BinaryOperator::BitAnd => output.push_str("&"),
				BinaryOperator::BitOr => output.push_str("|"),
				BinaryOperator::BitXor => output.push_str("^"),
				BinaryOperator::BitShiftLeft => output.push_str("<<"),
				BinaryOperator::BitShiftRight => output.push_str(">>"),
				BinaryOperator::Equal => output.push_str("=="),
				BinaryOperator::NotEqual => output.push_str("!="),
				BinaryOperator::LessThan => output.push_str("<"),
				BinaryOperator::LessThanEqual => output.push_str("<="),
				BinaryOperator::GreaterThan => output.push_str(">"),
				BinaryOperator::GreaterThanEqual => output.push_str(">="),
				BinaryOperator::MemberAccess => output.push_str("."),
				BinaryOperator::ArrayAccess => {
					output.push_str("[");
					output.push_str(expression_to_string(&rhs).as_str());
					output.push_str("]");
					array_access = true;
				}
			}
			if !array_access {
				output.push_str(expression_to_string(&rhs).as_str());
			}
			output.push_str(")");
		},
		Expr::UnaryExpr {pos:_, op, operand} => {
			output.push_str("(");
			match op {
				UnaryOperator::Negate => output.push_str("-"),
				UnaryOperator::Not => output.push_str("!"),
				UnaryOperator::BitNot => output.push_str("~")
			}
			output.push_str(expression_to_string(&operand).as_str());
			output.push_str(")");
		},
		Expr::FunctionCall {pos:_, name, args} => {
			output.push_str(name.as_str());
			output.push_str("(");
			for e in args {
				output.push_str(expression_to_string(e).as_str());
			}
			output.push_str(")");
		},
		Expr::Ternary {pos:_, cond, success, failure} => {
			output.push_str("(");
			output.push_str(expression_to_string(&cond).as_str());
			output.push_str("?");
			output.push_str(expression_to_string(&success).as_str());
			output.push_str(":");
			output.push_str(expression_to_string(&failure).as_str());
			output.push_str(")");
		},
		Expr::Literal {pos:_, value} => {
			match value {
				Literal::Int(i) => output.push_str(i.to_string().as_str()),
				Literal::UInt(u) => output.push_str(u.to_string().as_str()),
				Literal::Float(f) => output.push_str(f.to_string().as_str()),
				Literal::Identifier(i) => output.push_str(i.as_str()),
				Literal::Bool(b) => output.push_str(b.to_string().as_str())
			}
		}
	}

	output
}

