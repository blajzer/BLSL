use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug)]
pub struct FunctionParam<'a> {
	pub pos: Span<'a>
}

#[derive(Debug)]
pub struct TypeDecl<'a> {
	pub pos: Span<'a>,
	pub name: String,
	pub path: Vec<String>
}

#[derive(Debug)]
pub enum Definition<'a> {
	Function {
		pos: Span<'a>,
		name: String,
		params: Vec<FunctionParam<'a>>,
		ret: TypeDecl<'a>,
		body: Statement<'a>
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
		initalization: Option<Expr<'a>>
	},
	ForLoop {
		pos: Span<'a>,
		initalization: Box<Statement<'a>>,
		cond: Expr<'a>,
		update: Box<Statement<'a>>
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

#[derive(Debug)]
pub enum Literal {
	Int(i64),
	UInt(u64),
	Float(f64),
	Identifier(String),
	Bool(bool)
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum UnaryOperator {
	Negate,
	Not,
	BitNot
}
