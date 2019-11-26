use super::super::parser::types::{BinaryOperator, UnaryOperator, Literal};


#[derive(Debug)]
pub struct SourcePos<'a> {
	file: &'a str,
	line: usize,
	column: usize
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
	name: String,
	field_type_index: usize
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct EnumValue {
	name: String,
	value: i32
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
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
	FunctionCall {
		pos: SourcePos<'a>,
		name: String,
		args: Vec<Expr<'a>>,
		type_index: usize
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