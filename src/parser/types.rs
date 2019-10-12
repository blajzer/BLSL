use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug)]
pub enum Statement<'a> {
	Block {
		pos: Span<'a>,
		body: Vec<Statement<'a>>
	},
	Expr {
		pos: Span<'a>,
		expr: Expr<'a>
	}
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
