// Copyright 2019-2020 Brett Lajzer
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use super::super::parser::types::{
	AssignmentOperator,
	BinaryOperator,
	UnaryOperator,
	Literal,
	Span
};


#[derive(Debug, Clone)]
pub struct SourcePos<'a> {
	file: &'a str,
	line: usize,
	column: usize
}

impl<'a> SourcePos<'a> {
	pub fn new(filename: &'a str, span: Span) -> SourcePos<'a> {
		SourcePos {
			file: filename,
			line: span.line as usize,
			column: span.get_utf8_column() as usize
		}
	}
}

#[derive(Debug)]
pub struct ValidateError<'a> {
	pos: SourcePos<'a>,
	message: String
}

impl<'a> ValidateError<'a> {
	pub fn new(pos: SourcePos<'a>, message: String) -> ValidateError<'a> {
		ValidateError {
			pos: pos,
			message: message
		}
	}
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BasicType {
	Bool,
	I32,
	U32,
	F16,
	F32,
	F64
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructField {
	pub name: String,
	pub field_type_index: usize
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct EnumValue {
	pub name: String,
	pub value: i32
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
	Void,
	BasicType(BasicType),
	Vector {
		dim: usize,
		base_type_index: usize
	},
	Matrix {
		rows: usize,
		cols: usize,
		base_type_index: usize
	},
	Array {
		dim: usize,
		base_type_index: usize
	},
	Struct {
		name: String,
		fields: Vec<StructField>
	},
	Enum {
		name: String,
		values: Vec<EnumValue>
	},
	Function {
		name: String,
		argument_type_indices: Vec<usize>,
		return_type_index: usize
	},
	QualifiedType {
		is_const: bool,
		is_ref: bool,
		base_type_index: usize
	},
	TypeAlias {
		name: String,
		alias_index: usize
	},
	// TODO: resource types
	// TODO: ExternalType is definitely the wrong way to handle this. Reconsider.
	ExternalType {
		name: String,
		module_path: String,
		type_index: usize
	}
}

#[derive(Debug)]
pub enum Expr<'a> {
	BinaryExpr {
		pos: SourcePos<'a>,
		op: BinaryOperator,
		lhs: Box<Expr<'a>>,
		rhs: Box<Expr<'a>>,
		type_index: usize
	},
	UnaryExpr {
		pos: SourcePos<'a>,
		op: UnaryOperator,
		operand: Box<Expr<'a>>,
		type_index: usize
	},
	VectorSwizzleExpr {
		pos: SourcePos<'a>,
		operand: Box<Expr<'a>>,
		indices: Vec<u8>,
		type_index: usize
	},
	FunctionCall {
		pos: SourcePos<'a>,
		name: String,
		args: Vec<Expr<'a>>,
		function_type_index: usize,
		return_type_index: usize
	},
	Ternary {
		pos: SourcePos<'a>,
		cond: Box<Expr<'a>>,
		success: Box<Expr<'a>>, 
		failure: Box<Expr<'a>>,
		type_index: usize
	},
	Literal {
		pos: SourcePos<'a>,
		value: Literal,
		type_index: usize
	}
}

#[derive(Debug)]
pub enum Statement<'a> {
	Block {
		pos: SourcePos<'a>,
		body: Vec<Statement<'a>>
	},
	Expr {
		pos: SourcePos<'a>,
		expr: Expr<'a>
	},
	Assignment {
		pos: SourcePos<'a>,
		lhs: Expr<'a>,
		rhs: Expr<'a>,
		op: AssignmentOperator
	},
	If {
		pos: SourcePos<'a>,
		cond: Expr<'a>,
		if_body: Box<Statement<'a>>,
		else_body: Option<Box<Statement<'a>>>
	},
	VariableDeclaration {
		pos: SourcePos<'a>,
		name: String,
		type_index: usize,
		initialization: Option<Expr<'a>>
	},
	ForLoop {
		pos: SourcePos<'a>,
		initialization: Box<Statement<'a>>,
		cond: Expr<'a>,
		update: Box<Statement<'a>>,
		body: Box<Statement<'a>>
	},
	WhileLoop {
		pos: SourcePos<'a>,
		cond: Expr<'a>,
		body: Box<Statement<'a>>
	},
	Return {
		pos: SourcePos<'a>,
		expr: Option<Expr<'a>>
	},
	Break {
		pos: SourcePos<'a>
	},
	Continue {
		pos: SourcePos<'a>
	}
}

impl<'a> Expr<'a> {
	pub fn get_type_index(&self) -> usize {
		match self {
			Expr::BinaryExpr { type_index, .. } => *type_index,
			Expr::UnaryExpr { type_index, .. } => *type_index,
			Expr::VectorSwizzleExpr { type_index, .. } => *type_index,
			Expr::FunctionCall { return_type_index, .. } => *return_type_index,
			Expr::Ternary { type_index, .. } => *type_index,
			Expr::Literal { type_index, .. } => *type_index
		}
	}
}
