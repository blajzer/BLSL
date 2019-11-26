pub mod types;

use super::builtin::*;

pub struct Validator {
	modules: Vec<Module>,
	types: Vec<types::Type>
}

struct Module {
	name: String,
	path: String,
	type_offset: usize,

}

impl Validator {
	pub fn new() -> Self {
		let mut validator = Validator {
			modules: Vec::new(),
			types: Vec::new()
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
}