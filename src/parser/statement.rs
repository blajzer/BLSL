use super::types::*;

#[derive(Debug)]
enum Statement {
	Block(Vec<Statement>),
	Expr(Box<Expr>)
}

