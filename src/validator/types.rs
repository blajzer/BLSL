use super::super::parser::types::{BinaryOperator, UnaryOperator, Literal, Span};


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

impl<'a> Expr<'a> {
	pub fn get_type_index(&self) -> usize {
		match self {
			Expr::BinaryExpr { type_index, .. } => *type_index,
			Expr::UnaryExpr { type_index, .. } => *type_index,
			Expr::FunctionCall { type_index, .. } => *type_index,
			Expr::Ternary { type_index, .. } => *type_index,
			Expr::Literal { type_index, .. } => *type_index
		}
	}
}
