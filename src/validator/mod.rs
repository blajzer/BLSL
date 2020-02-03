// Copyright 2019-2020 Brett Lajzer
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

pub mod types;

use types::*;
use super::builtin::*;
use super::parser::types::{AssignmentOperator, BinaryOperator, Expr, Literal, Statement, TypeDecl, UnaryOperator};

pub struct Validator {
	modules: Vec<Module>,
	types: Vec<types::Type>,
	scopes: Vec<Scope>
}

struct Module {
	name: String,
	path: String,
	type_offset: usize
}

struct Scope {
	variables: Vec<Variable>
}

struct Variable {
	name: String,
	type_index: usize
}

impl Validator {
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

	// TODO: does this need to exist? Should qualified types even be concrete?
	fn get_qualified_base_type(&self, t: usize) -> usize {
		match self.types[t] {
			Type::TypeAlias {alias_index, ..} => self.get_qualified_base_type(alias_index),
			Type::ExternalType {type_index, ..} => self.get_qualified_base_type(type_index),
			_ => t
		}
	}

	fn get_variable_type<'b>(&self, name: &str, pos: &SourcePos<'b>) -> Result<usize, types::ValidateError<'b>> {
		for scope in self.scopes.iter().rev() {
			if let Some(v) = scope.variables.iter().find(|v| v.name == name) {
				return Ok(v.type_index);
			}
		}

		let message = format!("Undeclared identifier {}", name);
		return Err(ValidateError::new(pos.clone(), message));
	}

	fn resolve_type_decl<'b>(&mut self, decl: &TypeDecl<'b>) -> Result<usize, types::ValidateError<'b>> {
		// TODO: implement this		
		Ok(0)
	}

	fn add_variable_to_current_scope<'b>(&mut self, name: &str, type_index: usize, pos: &SourcePos<'b>) -> Result<(), types::ValidateError<'b>> {
		if let Some(scope) = self.scopes.last_mut() {
			if let Some(v) = scope.variables.iter().find(|v| v.name == name) {
				let message = format!("Variable already exists in current scope {}", name);
				return Err(ValidateError::new(pos.clone(), message));
			} else {
				scope.variables.push(Variable { name: name.to_string(), type_index: type_index });
			}
		}

		Ok(())
	}

	/// Returns (function type, function return type) or an error
	fn find_function_type<'b>(&self, source_pos: SourcePos<'b>, func_name: &str, args: &Vec<types::Expr>) -> Result<(usize, usize), types::ValidateError<'b>> {
		let arg_types: Vec<(usize, usize)> = args.iter().map(|e| {
			let type_index = e.get_type_index();
			(type_index, self.get_qualified_base_type(type_index))
		}).collect();

		let candidate_types: Vec<(usize, &types::Type)> = self.types.iter().enumerate().filter(|(_, t)| {
			if let types::Type::Function {name, argument_type_indices, ..} = t {
				if name == func_name && argument_type_indices.len() == args.len() {
					argument_type_indices.iter().zip(arg_types.iter()).fold(true, |a, (t1, (_, t2))| a && (t1 == t2))
				} else {
					false
				}
			} else {
				false
			}
		}).collect();

		if candidate_types.len() > 0 {
			// TODO: compare candidates and choose the best one versus choosing the first one
			let (func_type_index, func_type) = candidate_types[0];
			if let types::Type::Function {return_type_index, ..} = func_type {
				Ok((func_type_index, *return_type_index))
			} else {
				unreachable!()
			}
		} else {
			let message = "No matching function declaration.".to_string();
			return Err(ValidateError::new(source_pos, message));
		}
	}

	fn validate_vector_swizzle<'b>(&mut self, expr: types::Expr<'b>, source_pos: SourcePos<'b>, dim: usize, base_type_index: usize, swizzle: &str) -> Result<types::Expr<'b>, types::ValidateError<'b>> {
		let mut indices: Vec<u8> = Vec::new();

		for (i, c) in swizzle.chars().enumerate() {
			if i >= 4 {
				let message = "Vector swizzle output is too wide".to_string();
				return Err(ValidateError::new(source_pos, message));
			}

			match c {
				'x' | 'r' => indices.push(0),
				'y' | 'g' => indices.push(1),
				'z' | 'b' => if dim >= 3 {
						indices.push(2)
					} else {
						let message = "Invalid vector swizzle index".to_string();
						return Err(ValidateError::new(source_pos, message));
					},
				'w' | 'a' => if dim >= 4 {
						indices.push(3)
					} else {
						let message = "Invalid vector swizzle index".to_string();
						return Err(ValidateError::new(source_pos, message));
					},
				_ => {
					let message = "Invalid vector swizzle index".to_string();
					return Err(ValidateError::new(source_pos, message));
				}
			}
		}

		let output_type = if indices.len() == 1 {
			base_type_index
		} else {
			self.find_or_add_type(types::Type::Vector { dim: indices.len(), base_type_index: base_type_index })
		};

		Ok(types::Expr::VectorSwizzleExpr {
			pos: source_pos,
			operand: Box::new(expr),
			indices: indices,
			type_index: output_type
		})
	}

	fn validate_expr<'b>(&mut self, file: &'b str, expr: super::parser::types::Expr<'b>) -> Result<types::Expr<'b>, types::ValidateError<'b>> {
		match expr {
			Expr::BinaryExpr { pos, op, lhs, rhs } => {
				let new_lhs = self.validate_expr(file, *lhs)?;
				
				// Handle vector swizzles. They're a special case of member access expression.
				if op == BinaryOperator::MemberAccess {
					if let &Type::Vector { dim, base_type_index } = &self.types[new_lhs.get_type_index()] {
						if let &Expr::Literal {value: Literal::Identifier(swizzle), ..} = &rhs.as_ref() {
							return self.validate_vector_swizzle(new_lhs, SourcePos::new(file, pos), dim, base_type_index, swizzle.as_str());
						} else {
							let message = "Invalid vector swizzle expression".to_string();
							return Err(ValidateError::new(SourcePos::new(file, pos), message));
						}
					}
				}
				
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
						if let &types::Expr::Literal {value: Literal::Identifier(ref identifier), ..} = &new_rhs {
							let lhs_type = &self.types[self.get_base_type(new_lhs.get_type_index())];
							if let &types::Type::Struct {ref fields, ..} = lhs_type {
								if let Some(field) = fields.iter().find(|f| f.name == *identifier) {
									field.field_type_index
								} else {
									let message = "Invalid member access, field not found in struct.".to_string();
									return Err(ValidateError::new(SourcePos::new(file, pos), message));
								}
							} else {
								let message = "Invalid member access, struct expected on left-hand-side.".to_string();
								return Err(ValidateError::new(SourcePos::new(file, pos), message));
							}
						} else {
							let message = "Invalid member access, identifier expected on right-hand-side.".to_string();
							return Err(ValidateError::new(SourcePos::new(file, pos), message));
						}
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

				let (function_type, return_type) = self.find_function_type(SourcePos::new(file, pos), &name, &new_args)?;

				Ok(types::Expr::FunctionCall {
					pos: SourcePos::new(file, pos),
					name: name,
					args: new_args,
					function_type_index: function_type,
					return_type_index: return_type
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

	fn validate_lvalue<'b>(&self, file: &'b str, lhs: &super::parser::types::Expr<'b>) -> Result<(), types::ValidateError<'b>> {
		match lhs {
			Expr::BinaryExpr { pos, op, lhs, rhs } => {
				match op {
					BinaryOperator::ArrayAccess | BinaryOperator::MemberAccess=> {
						return self.validate_lvalue(file, &lhs);
					},
					_ => {
						let message = format!("The {} operator cannot appear in an lvalue expression. Only member access and array access are valid.", op.to_string());
						return Err(ValidateError::new(SourcePos::new(file, pos.clone()), message));
					}
				};
			},
			Expr::Literal { pos, value } => {
				if let Literal::Identifier(_) = value {
				} else {
					let message = "Expected identifier.".to_string();
					return Err(ValidateError::new(SourcePos::new(file, pos.clone()), message));
				}
			},
			_ => {
				let message = "Invalid expression in lvalue.".to_string();
				return Err(ValidateError::new(SourcePos::new(file, lhs.get_pos().clone()), message));
			}
		};

		Ok(())
	}

	fn validate_statement<'b>(&mut self, file: &'b str, statement: super::parser::types::Statement<'b>, is_loop: bool, return_type: usize) -> Result<types::Statement<'b>, types::ValidateError<'b>> {
		match statement {
			Statement::Block { pos, body } => {
				self.scopes.push(Scope { variables: Vec::new() });

				let mut new_body = Vec::new();
				for s in body {
					let new_s = self.validate_statement(file, s, is_loop, return_type)?;
					new_body.push(new_s);
				}

				self.scopes.pop();

				Ok(types::Statement::Block {
					pos: SourcePos::new(file, pos),
					body: new_body
				})
			},
			Statement::Expr { pos, expr } => {
				let new_expr = self.validate_expr(file, expr)?;

				Ok(types::Statement::Expr {
					pos: SourcePos::new(file, pos),
					expr: new_expr
				})
			},
			Statement::Assignment { pos, lhs, rhs, op } => {
				self.validate_lvalue(file, &lhs)?;
				let new_lhs = self.validate_expr(file, lhs)?;
				let new_rhs = self.validate_expr(file, rhs)?;

				Ok(types::Statement::Assignment {
					pos: SourcePos::new(file, pos),
					lhs: new_lhs,
					rhs: new_rhs,
					op: op
				})
			},
			Statement::If { pos, cond, if_body, else_body } => {
				let new_cond = self.validate_expr(file, cond)?;
				let new_if_body = self.validate_statement(file, *if_body, is_loop, return_type)?;
				let new_else_body = if let Some(else_body) = else_body {
					let new_else_body = self.validate_statement(file, *else_body, is_loop, return_type)?;
					Some(Box::new(new_else_body))
				} else {
					None
				};

				Ok(types::Statement::If {
					pos: SourcePos::new(file, pos),
					cond: new_cond,
					if_body: Box::new(new_if_body),
					else_body: new_else_body
				})
			},
			Statement::VariableDeclaration { pos, name, var_type, initialization } => {
				let new_pos = SourcePos::new(file, pos);

				let var_type_index = self.resolve_type_decl(&var_type)?;
				self.add_variable_to_current_scope(name.as_str(), var_type_index, &new_pos)?;

				let new_initialization = if let Some(initialization) = initialization {
					let new_initialization = self.validate_expr(file, initialization)?;
					Some(new_initialization)
				} else {
					None
				};

				Ok(types::Statement::VariableDeclaration {
					pos: new_pos,
					name: name.clone(),
					type_index: var_type_index,
					initialization: new_initialization
				})
			},
			Statement::ForLoop { pos, initialization, cond, update, body } => {
				// TODO: validate that init is a declaration, that update is an assignment, and body is a block
				let new_initialization = self.validate_statement(file, *initialization, is_loop, return_type)?;
				let new_cond = self.validate_expr(file, cond)?;
				let new_update = self.validate_statement(file, *update, is_loop, return_type)?;
				let new_body = self.validate_statement(file, *body, true, return_type)?;
				
				Ok(types::Statement::ForLoop {
					pos: SourcePos::new(file, pos),
					initialization: Box::new(new_initialization),
					cond: new_cond,
					update: Box::new(new_update),
					body: Box::new(new_body)
				})
			},
			Statement::WhileLoop { pos, cond, body } => {
				// TODO: validate body is a block
				let new_cond = self.validate_expr(file, cond)?;
				let new_body = self.validate_statement(file, *body, true, return_type)?;
				
				Ok(types::Statement::WhileLoop {
					pos: SourcePos::new(file, pos),
					cond: new_cond,
					body: Box::new(new_body)
				})
			},
			Statement::Return { pos, expr } => {
				let new_expr = if let Some(expr) = expr {
					let new_expr = self.validate_expr(file, expr)?;
					if new_expr.get_type_index() != return_type {
						// TODO: better message here
						let message = "Mismatched return type".to_string();
						return Err(ValidateError::new(SourcePos::new(file, pos), message));
					}

					Some(new_expr)
				} else {
					if return_type != VOID_INDEX {
						let message = "Invalid return type, expected void.".to_string();
						return Err(ValidateError::new(SourcePos::new(file, pos), message));
					}

					None
				};

				Ok(types::Statement::Return {
					pos: SourcePos::new(file, pos),
					expr: new_expr
				})
			},
			Statement::Break { pos } => {
				if !is_loop {
					let message = "Break statements can only appear inside of a loop.".to_string();
					return Err(ValidateError::new(SourcePos::new(file, pos), message));
				}

				Ok(types::Statement::Break {
					pos: SourcePos::new(file, pos)
				})
			},
			Statement::Continue { pos } => {
				if !is_loop {
					let message = "Continue statements can only appear inside of a loop.".to_string();
					return Err(ValidateError::new(SourcePos::new(file, pos), message));
				}

				Ok(types::Statement::Continue {
					pos: SourcePos::new(file, pos)
				})
			}
		}
	}
}