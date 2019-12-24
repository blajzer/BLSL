pub mod types;

use types::*;
use super::builtin::*;
use super::parser::types::{BinaryOperator, Expr, Literal, UnaryOperator};

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

	fn get_base_type(&self, t: usize) -> usize {
		match self.types[t] {
			Type::QualifiedType {base_type_index, ..} => self.get_base_type(base_type_index),
			Type::TypeAlias {alias_index, ..} => self.get_base_type(alias_index),
			Type::ExternalType {type_index, ..} => self.get_base_type(type_index),
			_ => t
		}
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

				// The output type of the binary expression depends on the operator in question.
				// For most binary operators, it's the same type as the LHS and RHS.
				let type_index = match op {
					BinaryOperator::Add | BinaryOperator::Subtract | BinaryOperator::Multiply | BinaryOperator::Divide | BinaryOperator::Modulus 
					| BinaryOperator::BitAnd | BinaryOperator::BitOr | BinaryOperator::BitXor | BinaryOperator::BitShiftLeft | BinaryOperator::BitShiftRight => {
						if new_lhs.get_type_index() != new_rhs.get_type_index() {
							let message = "Mismatched types between left and right sides of binary expression.".to_string();
							return Err(ValidateError::new(SourcePos::new(file, pos), message));
						}

						new_lhs.get_type_index()
					},
					BinaryOperator::And | BinaryOperator::Or => {
						// TODO: boolean conversion?
						if new_lhs.get_type_index() != BOOL_INDEX {
							let message = "Left-hand side of boolean operation must be a boolean.".to_string();
							return Err(ValidateError::new(SourcePos::new(file, pos), message));
						}

						if new_rhs.get_type_index() != BOOL_INDEX {
							let message = "Right-hand side of boolean operation must be a boolean.".to_string();
							return Err(ValidateError::new(SourcePos::new(file, pos), message));
						}

						BOOL_INDEX
					},
					BinaryOperator::Equal 
					| BinaryOperator::NotEqual 
					| BinaryOperator::LessThan 
					| BinaryOperator::LessThanEqual 
					| BinaryOperator::GreaterThan 
					| BinaryOperator::GreaterThanEqual => {
						if new_lhs.get_type_index() != new_rhs.get_type_index() {
							let message = "Mismatched types between left and right sides of comparison.".to_string();
							return Err(ValidateError::new(SourcePos::new(file, pos), message));
						}

						BOOL_INDEX
					},
					BinaryOperator::MemberAccess => {
						// TODO: implement
						VOID_INDEX
					},
					BinaryOperator::ArrayAccess => {
						let lhs_type = &self.types[self.get_base_type(new_lhs.get_type_index())];

						if let Type::Array {base_type_index, .. } = lhs_type {
							*base_type_index
						} else {
							let message = "Can't index non-array type.".to_string();
							return Err(ValidateError::new(SourcePos::new(file, pos), message));
						}
					}
				};

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
				
				// Boolean conversion
				let type_index = if op == UnaryOperator::Not { BOOL_INDEX } else { new_operand.get_type_index() };

				Ok(types::Expr::UnaryExpr {
					pos: SourcePos::new(file, pos),
					op: op,
					operand: Box::new(new_operand),
					type_index: type_index
				})
			},
			Expr::FunctionCall { pos, name, args } => {
				// Convert arguments
				let mut new_args = Vec::new();
				for arg in args {
					let converted_arg = self.validate_expr(file, arg)?;
					new_args.push(converted_arg);
				}

				// TODO: lookup function call type

				// TODO: function calls
				Ok(types::Expr::FunctionCall {
					pos: SourcePos::new(file, pos),
					name: name,
					args: new_args,
					type_index: 0 // TODO: how?! should be return type?
				})
			},
			Expr::Ternary { pos, cond, success, failure } => {
				let new_cond = self.validate_expr(file, *cond)?;
				let new_success = self.validate_expr(file, *success)?;
				let new_failure = self.validate_expr(file, *failure)?;

				// TODO: bool-convertible?
				if new_cond.get_type_index() != BOOL_INDEX {
					let message = "Conditional must be a boolean expression in ternary.".to_string();
					return Err(ValidateError::new(SourcePos::new(file, pos), message));
				}

				if new_success.get_type_index() != new_failure.get_type_index() {
					// TODO: better errors
					let message = "Mismatched types between success and failure branches of ternary expression.".to_string();
					return Err(ValidateError::new(SourcePos::new(file, pos), message));
				}

				let type_index = new_success.get_type_index();

				Ok(types::Expr::Ternary {
					pos: SourcePos::new(file, pos),
					cond: Box::new(new_cond),
					success: Box::new(new_success),
					failure: Box::new(new_failure),
					type_index: type_index
					
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