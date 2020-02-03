// Copyright 2019-2020 Brett Lajzer
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug)]
pub struct FunctionParam<'a> {
	pub pos: Span<'a>,
	pub name: String,
	pub type_name: TypeDecl<'a>,
	// TODO: semantic
}

#[derive(Debug)]
pub struct TypeDecl<'a> {
	pub pos: Span<'a>,
	pub name: String,
	pub path: Vec<String>,
	pub is_ref: bool,
	pub is_const: bool
}

#[derive(Debug)]
pub enum Definition<'a> {
	Function {
		pos: Span<'a>,
		name: String,
		params: Vec<FunctionParam<'a>>,
		ret: Option<TypeDecl<'a>>,
		body: Statement<'a>
	},
	Variant {
		pos: Span<'a>,
		name: String,
		values: Vec<i64>
	}
}

#[derive(Debug)]
pub enum Statement<'a> {
	Block {
		pos: Span<'a>,
		body: Vec<Statement<'a>>
	},
	Expr {
		pos: Span<'a>,
		expr: Expr<'a>
	},
	Assignment {
		pos: Span<'a>,
		lhs: Expr<'a>,
		rhs: Expr<'a>,
		op: AssignmentOperator
	},
	If {
		pos: Span<'a>,
		cond: Expr<'a>,
		if_body: Box<Statement<'a>>,
		else_body: Option<Box<Statement<'a>>>
	},
	VariableDeclaration {
		pos: Span<'a>,
		name: String,
		var_type: TypeDecl<'a>,
		initialization: Option<Expr<'a>>
	},
	ForLoop {
		pos: Span<'a>,
		initialization: Box<Statement<'a>>,
		cond: Expr<'a>,
		update: Box<Statement<'a>>,
		body: Box<Statement<'a>>
	},
	WhileLoop {
		pos: Span<'a>,
		cond: Expr<'a>,
		body: Box<Statement<'a>>
	},
	Return {
		pos: Span<'a>,
		expr: Option<Expr<'a>>
	},
	Break {
		pos: Span<'a>
	},
	Continue {
		pos: Span<'a>
	}
}

#[derive(Debug, Clone)]
pub enum AssignmentOperator {
	Assign,
	AssignAdd,
	AssignSubtract,
	AssignMultiply,
	AssignDivide,
	AssignModulus,
	AssignBitAnd,
	AssignBitOr,
	AssignBitXor
}

#[derive(Debug)]
pub enum Expr<'a> {
	BinaryExpr {
		pos: Span<'a>,
		op: BinaryOperator,
		lhs: Box<Expr<'a>>,
		rhs: Box<Expr<'a>>
	},
	UnaryExpr {
		pos: Span<'a>,
		op: UnaryOperator,
		operand: Box<Expr<'a>>
	},
	FunctionCall {
		pos: Span<'a>,
		name: String,
		args: Vec<Expr<'a>>
	},
	Ternary {
		pos: Span<'a>,
		cond: Box<Expr<'a>>,
		success: Box<Expr<'a>>, 
		failure: Box<Expr<'a>>
	},
	Literal {
		pos: Span<'a>,
		value: Literal
	}
}

impl<'a> Expr<'a> {
	pub fn get_pos(&self) -> &Span<'a> {
		match self {
			Expr::BinaryExpr { pos, .. } => pos,
			Expr::UnaryExpr { pos, .. } => pos,
			Expr::FunctionCall { pos, .. } => pos,
			Expr::Ternary { pos, .. } => pos,
			Expr::Literal { pos, .. } => pos
		}
	}
}

#[derive(Debug)]
pub enum Literal {
	Int(i64),
	UInt(u64),
	Float(f64),
	Identifier(String),
	Bool(bool)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BinaryOperator {
	Add,
	Subtract,
	Multiply,
	Divide,
	Modulus,
	And,
	Or,
	BitAnd,
	BitOr,
	BitXor,
	BitShiftLeft,
	BitShiftRight,
	Equal,
	NotEqual,
	LessThan,
	LessThanEqual,
	GreaterThan,
	GreaterThanEqual,
	MemberAccess,
	ArrayAccess
}

impl std::fmt::Display for BinaryOperator {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}",
			match self {
				Add => "add",
				Subtract => "subtract",
				Multiply => "multiply",
				Divide => "divide",
				Modulus => "modulus",
				And => "and",
				Or => "or",
				BitAnd => "bitwise and",
				BitOr => "bitwise or",
				BitXor => "bitwise exclusive-or",
				BitShiftLeft => "bit shift left",
				BitShiftRight => "bit shift right",
				Equal => "equal",
				NotEqual => "not equal",
				LessThan => "less than",
				LessThanEqual => "less than or equal",
				GreaterThan => "greater than",
				GreaterThanEqual => "greater than or equal",
				MemberAccess => "member access",
				ArrayAccess => "array access"
			}
		)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaryOperator {
	Negate,
	Not,
	BitNot
}
