extern crate nom;
extern crate nom_locate;

mod builtin;

mod parser;
use parser::definition::parse_definition;
use parser::expression_to_string;
use parser::expr::parse_expression;
use parser::statement::parse_statement;
use parser::types::Span;

mod validator;

use nom::{
	IResult,
	error::VerboseErrorKind
};

fn main() {
	println!("parsed {:#?}", parse_definition(Span::new("
	func testFunc(int param1, int param2, vec4f param3) -> vec3f {
		return vec3f(param1 * param2 + param3.xyz);
	}
	")));
}
