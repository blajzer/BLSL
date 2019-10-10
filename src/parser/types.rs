
#[derive(Debug)]
pub enum Expr {
	Add(Box<Expr>, Box<Expr>),
	Subtract(Box<Expr>, Box<Expr>),
	Multiply(Box<Expr>, Box<Expr>),
	Divide(Box<Expr>, Box<Expr>),
	Modulus(Box<Expr>, Box<Expr>),
	Equal(Box<Expr>, Box<Expr>),
	NotEqual(Box<Expr>, Box<Expr>),
	LessThan(Box<Expr>, Box<Expr>),
	LessThanEqual(Box<Expr>, Box<Expr>),
	GreaterThan(Box<Expr>, Box<Expr>),
	GreaterThanEqual(Box<Expr>, Box<Expr>),
	Negate(Box<Expr>),
	Not(Box<Expr>),
	And(Box<Expr>, Box<Expr>),
	Or(Box<Expr>, Box<Expr>),
	BitAnd(Box<Expr>, Box<Expr>),
	BitOr(Box<Expr>, Box<Expr>),
	BitXor(Box<Expr>, Box<Expr>),
	BitShiftLeft(Box<Expr>, Box<Expr>),
	BitShiftRight(Box<Expr>, Box<Expr>),
	BitNot(Box<Expr>),
	FunctionCall(String, Vec<Expr>),
	Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
	MemberAccess(Box<Expr>, Box<Expr>),
	ArrayAccess(Box<Expr>, Box<Expr>),
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

