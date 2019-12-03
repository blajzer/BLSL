pub mod types;

use types::*;
use super::builtin::*;
use super::parser::types::{Expr, Literal};

pub struct Validator<'a> {
	modules: Vec<Module>,
	types: Vec<types::Type>,
	scopes: Vec<Scope<'a>>
}

struct Module {
	name: String,
	path: String,
	type_offset: usize
}

struct Scope<'a> {
	variables: Vec<Variable<'a>>
}

struct Variable<'a> {
	name: &'a str,
	type_index: usize
}

impl<'a> Validator<'a> {
	pub fn new() -> Self {
		let mut validator = Validator {
			modules: Vec::new(),
			types: Vec::new(),
			scopes: Vec::new()
		};

		validator.load_std_module();
		validator
	}

	fn load_std_module(&mut self) {
		self.modules.push(Module {name: "std".to_string(), path: String::new(), type_offset: self.types.len()});
		self.types.clone_from_slice(BULTIN_TYPES);
	}

	fn find_or_add_type(&mut self, t: types::Type) -> usize {
		if let Some(pos) = self.types.iter().position(|o| *o == t) {
			pos
		} else {
			self.types.push(t);
			self.types.len() - 1
		}
	}

	fn resolve_type(&self, t: types::Type) -> Option<usize> {
		None
	}

	fn get_variable_type<'b>(&self, name: &str, pos: &SourcePos<'b>) -> Result<usize, types::ValidateError<'b>> {
		for scope in self.scopes.iter() {
			if let Some(v) = scope.variables.iter().find(|v| v.name == name) {
				return Ok(v.type_index);
			}
		}

		let message = format!("Undeclared identifier {}", name);
		return Err(ValidateError::new(pos.clone(), message));
	}

	fn validate_expr<'b>(&mut self, file: &'b str, expr: super::parser::types::Expr<'b>) -> Result<types::Expr<'b>, types::ValidateError<'b>> {
		
		match expr {
			Expr::BinaryExpr { pos, op, lhs, rhs } => {
				let new_lhs = self.validate_expr(file, *lhs)?;
				let new_rhs = self.validate_expr(file, *rhs)?;

				// TODO: type checking here
				if new_lhs.get_type_index() != new_rhs.get_type_index() {
					// TODO: better errors
					let message = "Mismatched types between left and right sides of binary expression.".to_string();
					return Err(ValidateError::new(SourcePos::new(file, pos), message));
				}

				let type_index = new_lhs.get_type_index();

				Ok(types::Expr::BinaryExpr {
					pos: SourcePos::new(file, pos),
					op: op,
					lhs: Box::new(new_lhs),
					rhs: Box::new(new_rhs),
					type_index: type_index
				})
			},
			Expr::UnaryExpr { pos, op, operand } => {
				let new_operand = self.validate_expr(file, *operand)?;

				let type_index = new_operand.get_type_index();

				Ok(types::Expr::UnaryExpr {
					pos: SourcePos::new(file, pos),
					op: op,
					operand: Box::new(new_operand),
					type_index: type_index
				})
			},
			Expr::FunctionCall { pos, name, args } => {
				let new_args = Vec::new(); // TODO: this
				// TODO: function calls
				Ok(types::Expr::FunctionCall {
					pos: SourcePos::new(file, pos),
					name: name,
					args: new_args,
					type_index: 0 // TODO: how?!
				})
			},
			Expr::Ternary { pos, cond, success, failure } => {
				let new_cond = self.validate_expr(file, *cond)?;
				let new_success = self.validate_expr(file, *success)?;
				let new_failure = self.validate_expr(file, *failure)?;

				Ok(types::Expr::Ternary {
					pos: SourcePos::new(file, pos),
					cond: Box::new(new_cond),
					success: Box::new(new_success),
					failure: Box::new(new_failure),
					type_index: 0
					
				})
			},
			Expr::Literal { pos, value } => {
				let new_pos = SourcePos::new(file, pos);
				
				let type_index = match &value {
					Literal::Int(_) => I32_INDEX,
					Literal::UInt(_) => U32_INDEX,
					Literal::Float(_) => F32_INDEX,
					Literal::Identifier(s) => self.get_variable_type(s.as_str(), &new_pos)?,
					Literal::Bool(_) => BOOL_INDEX
				};

				Ok(types::Expr::Literal {
					pos: new_pos,
					value: value,
					type_index: type_index
				})
			}
		}
	}
}