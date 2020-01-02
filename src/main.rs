// Copyright 2019-2020 Brett Lajzer
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

extern crate nom;
extern crate nom_locate;

pub mod builtin;

pub mod parser;
use parser::definition::parse_definition;
use parser::expression_to_string;
use parser::expr::parse_expression;
use parser::statement::parse_statement;
use parser::types::Span;

pub mod validator;

pub mod types;

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
