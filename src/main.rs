extern crate nom;

mod parser;
use parser::expr::parse_expression;

fn main() {
	//println!("parsed {:?}", parse_expression("  myFun((- a  ), 5) * ( ! 5 / 6)*( 4 + 3 )  "));
	//println!("parsed {:?}", parse_expression("  myFun(a > b ? 4 + x : 5)"));

	//println!("parsed {:?}", parse_expression("  myFun(a.y.z > b.x ? 4 + x : 5).a"));
	//println!("parsed {:?}", parse_expression(" b[5 + 4].z >> 5 | 4"));

	println!("parsed {:?}", parse_expression("a ? b + 5 ? x : z : c"));

	/*if let Ok((i,e)) = parse_expression(" myFun((- a  ), 5) * ( ! 5 / 6)*( 4 + 3 ) ") {
		pretty_print_expr(&e);
		println!("");
	}*/
}
