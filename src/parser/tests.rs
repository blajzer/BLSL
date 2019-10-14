use super::*;
use super::expr::*;
use super::statement::*;
use super::types::*;


fn expression_parse_test(input: &str, expected: &str) {
	let parsed = parse_expression(Span::new(input));
	if let Ok((_, expr)) = parsed {
		let output = expression_to_string(&expr);
		assert_eq!(expected, output, "Parsed expression \"{}\" doesn't match expected \"{}\"", output, expected);
	} else {
		panic!("Failed to parse expression: \"{}\"\nDebug output: {:?}", input, parsed);
	}
}

#[test]
fn test_expression() {
	// test literals
	expression_parse_test("4", "4");
	//expression_parse_test("1.0", "1.0"); // TODO: prints wrong
	expression_parse_test("a", "a");
	expression_parse_test("true", "true");
	expression_parse_test("false", "false");

	// precedence and associativity
	expression_parse_test("a?b?c:d:e", "(a?(b?c:d):e)");
	expression_parse_test("a.b.c.d.e.w[5]", "((((((a.b).c).d).e).w)[5])");
}