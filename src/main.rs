extern crate nom;
extern crate nom_locate;

mod parser;
use parser::expression_to_string;
use parser::expr::parse_expression;
use parser::statement::parse_statement;
use parser::types::Span;

use nom::{
	IResult,
	error::VerboseErrorKind
};

fn main() {
	/*println!("parsed {:?}", parse_statement(Span::new("	if( a - b > 6) {
		g();
	} else if (x)
	{
		y += 4;
		r.t.w[4] = 5;

	}")));
*/

	if let Ok((_, expr)) = parse_expression(Span::new("a.b.c.d.e.w[5]")) {
		println!("{}", expression_to_string(&expr));
	}
}
