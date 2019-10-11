
#[derive(Debug)]
pub enum Expr {
	BinaryExpr(BinaryOperator, Box<Expr>, Box<Expr>),
	UnaryExpr(UnaryOperator, Box<Expr>),
	FunctionCall(String, Vec<Expr>),
	Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
	Literal(Literal)
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
