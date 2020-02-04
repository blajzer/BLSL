// Copyright 2019-2020 Brett Lajzer
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::*;
use super::expr::*;
use super::types::*;


fn expression_parse_test(input: &str, expected: &str) {
	let parsed = parse_expression(Span::new(input));
	if let Ok((_, expr)) = parsed {
		let output = expression_to_string(&expr);
		assert_eq!(expected, output, "Parsed expression \"{}\" doesn't match expected \"{}\"", output, expected);
	} else {
		panic!("Failed to parse expression: \"{}\"\nDebug output: {:?}\n", input, parsed);
	}
}

#[test]
fn expr_literals() {
	// test literals
	expression_parse_test("4", "4");
	//expression_parse_test("1.0", "1.0"); // TODO: prints wrong
	expression_parse_test("a", "a");
	expression_parse_test("true", "true");
	expression_parse_test("false", "false");

	expression_parse_test("_a", "_a");
	expression_parse_test("_1", "_1");
	expression_parse_test("a_a", "a_a");
	expression_parse_test("_a1b2c34", "_a1b2c34");
	expression_parse_test("abc_a1b_2c34", "abc_a1b_2c34");
}

#[test]
fn expr_comments() {
	expression_parse_test("/**/foo//\n( /* bloop */ a,b,// single line comment\n5,2)// bleh\n", "foo(a,b,5,2)");
}

#[test]
fn expr_function_call() {
	expression_parse_test("foo(a,b,5,2)", "foo(a,b,5,2)");
	expression_parse_test("foo(a,b,g(x),2)", "foo(a,b,g(x),2)");
}


#[test]
fn expr_member_access() {
	expression_parse_test("a.b", "(a.b)");
	expression_parse_test("a.b.c", "((a.b).c)");
	expression_parse_test("a.b.c.d", "(((a.b).c).d)");
}

#[test]
fn expr_method_call() {
	expression_parse_test("bar.foo(a,b,5,2)", "foo(bar,a,b,5,2)");
	expression_parse_test("bar.foo(a,b,5,2).baz()", "baz(foo(bar,a,b,5,2))");
}

#[test]
fn expr_array_access() {
	expression_parse_test("a[b]", "(a[b])");
	expression_parse_test("a[b][c]", "((a[b])[c])");
}

#[test]
fn expr_not() {
	expression_parse_test("!true", "(!true)");
	expression_parse_test("!!true", "(!(!true))");
}

#[test]
fn expr_negate() {
	expression_parse_test("-5", "(-5)");
	expression_parse_test("--5", "(-(-5))");
}

#[test]
fn expr_bitwise_not() {
	expression_parse_test("~5", "(~5)");
	expression_parse_test("~~5", "(~(~5))");
}

#[test]
fn expr_multiply() {
	expression_parse_test("4*6", "(4*6)");
	expression_parse_test("1*2*3", "((1*2)*3)");
}


#[test]
fn expr_divide() {
	expression_parse_test("4/6", "(4/6)");
	expression_parse_test("1/2/3", "((1/2)/3)");
}


#[test]
fn expr_modulus() {
	expression_parse_test("4%6", "(4%6)");
	expression_parse_test("1%2%3", "((1%2)%3)");
}

#[test]
fn expr_add() {
	expression_parse_test("4+6", "(4+6)");
	expression_parse_test("1+2+3", "((1+2)+3)");
}

#[test]
fn expr_subtract() {
	expression_parse_test("4-6", "(4-6)");
	expression_parse_test("1-2-3", "((1-2)-3)");
}

#[test]
fn expr_bitshift_left() {
	expression_parse_test("40<<3", "(40<<3)");
}

#[test]
fn expr_bitshift_right() {
	expression_parse_test("40>>3", "(40>>3)");
}

#[test]
fn expr_less_than_or_equal() {
	expression_parse_test("40<=3", "(40<=3)");
}

#[test]
fn expr_greater_than_or_equal() {
	expression_parse_test("40>=3", "(40>=3)");
}

#[test]
fn expr_less_than() {
	expression_parse_test("40<3", "(40<3)");
}

#[test]
fn expr_greater_than() {
	expression_parse_test("40>3", "(40>3)");
}

#[test]
fn expr_not_equal() {
	expression_parse_test("40!=3", "(40!=3)");
}

#[test]
fn expr_equal() {
	expression_parse_test("40==3", "(40==3)");
}

#[test]
fn expr_bitwise_and() {
	expression_parse_test("358&2", "(358&2)");
	expression_parse_test("358&2&1", "((358&2)&1)");
}

#[test]
fn expr_bitwise_xor() {
	expression_parse_test("358^2", "(358^2)");
	expression_parse_test("358^2^1", "((358^2)^1)");
}

#[test]
fn expr_bitwise_or() {
	expression_parse_test("358|2", "(358|2)");
	expression_parse_test("358|2|1", "((358|2)|1)");
}

#[test]
fn expr_and() {
	expression_parse_test("a&&b", "(a&&b)");
	expression_parse_test("a&&b&&c", "((a&&b)&&c)");
}

#[test]
fn expr_or() {
	expression_parse_test("a||b", "(a||b)");
	expression_parse_test("a||b||c", "((a||b)||c)");
}

#[test]
fn expr_ternary() {
	expression_parse_test("a?b:c", "(a?b:c)");
	expression_parse_test("a?b?c:d:e", "(a?(b?c:d):e)");
}

#[test]
fn expr_precedence() {
	// precedence and associativity
	expression_parse_test("a.b.c.d.e.w[5]", "((((((a.b).c).d).e).w)[5])");
}