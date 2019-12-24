use super::parser::types::*;
use super::validator::types::*;

pub const VOID_INDEX: usize = 0;
pub const BOOL_INDEX: usize = 1;
pub const I32_INDEX: usize = 2;
pub const U32_INDEX: usize = 3;
pub const F16_INDEX: usize = 4;
pub const F32_INDEX: usize = 5;
pub const F64_INDEX: usize = 6;

// TODO: generate with a macro?

pub const BULTIN_TYPES: &[Type] = &[
	// Void type
	Type::Void,

	// Basic types
	Type::BasicType(BasicType::Bool),
	Type::BasicType(BasicType::I32),
	Type::BasicType(BasicType::U32),
	Type::BasicType(BasicType::F16),
	Type::BasicType(BasicType::F32),
	Type::BasicType(BasicType::F64),

	// Vector Types
	Type::Vector { dim: 2, base_type_index: BOOL_INDEX },
	Type::Vector { dim: 3, base_type_index: BOOL_INDEX },
	Type::Vector { dim: 4, base_type_index: BOOL_INDEX },

	Type::Vector { dim: 2, base_type_index: I32_INDEX },
	Type::Vector { dim: 3, base_type_index: I32_INDEX },
	Type::Vector { dim: 4, base_type_index: I32_INDEX },

	Type::Vector { dim: 2, base_type_index: U32_INDEX },
	Type::Vector { dim: 3, base_type_index: U32_INDEX },
	Type::Vector { dim: 4, base_type_index: U32_INDEX },

	Type::Vector { dim: 2, base_type_index: F16_INDEX },
	Type::Vector { dim: 3, base_type_index: F16_INDEX },
	Type::Vector { dim: 4, base_type_index: F16_INDEX },

	Type::Vector { dim: 2, base_type_index: F32_INDEX },
	Type::Vector { dim: 3, base_type_index: F32_INDEX },
	Type::Vector { dim: 4, base_type_index: F32_INDEX },

	Type::Vector { dim: 2, base_type_index: F64_INDEX },
	Type::Vector { dim: 3, base_type_index: F64_INDEX },
	Type::Vector { dim: 4, base_type_index: F64_INDEX },

	// Matrix Types
	Type::Matrix { rows: 2, cols: 2, base_type_index: BOOL_INDEX },
	Type::Matrix { rows: 2, cols: 3, base_type_index: BOOL_INDEX },
	Type::Matrix { rows: 2, cols: 4, base_type_index: BOOL_INDEX },
	Type::Matrix { rows: 3, cols: 2, base_type_index: BOOL_INDEX },
	Type::Matrix { rows: 3, cols: 3, base_type_index: BOOL_INDEX },
	Type::Matrix { rows: 3, cols: 4, base_type_index: BOOL_INDEX },
	Type::Matrix { rows: 4, cols: 2, base_type_index: BOOL_INDEX },
	Type::Matrix { rows: 4, cols: 3, base_type_index: BOOL_INDEX },
	Type::Matrix { rows: 4, cols: 4, base_type_index: BOOL_INDEX },

	Type::Matrix { rows: 2, cols: 2, base_type_index: I32_INDEX },
	Type::Matrix { rows: 2, cols: 3, base_type_index: I32_INDEX },
	Type::Matrix { rows: 2, cols: 4, base_type_index: I32_INDEX },
	Type::Matrix { rows: 3, cols: 2, base_type_index: I32_INDEX },
	Type::Matrix { rows: 3, cols: 3, base_type_index: I32_INDEX },
	Type::Matrix { rows: 3, cols: 4, base_type_index: I32_INDEX },
	Type::Matrix { rows: 4, cols: 2, base_type_index: I32_INDEX },
	Type::Matrix { rows: 4, cols: 3, base_type_index: I32_INDEX },
	Type::Matrix { rows: 4, cols: 4, base_type_index: I32_INDEX },

	Type::Matrix { rows: 2, cols: 2, base_type_index: U32_INDEX },
	Type::Matrix { rows: 2, cols: 3, base_type_index: U32_INDEX },
	Type::Matrix { rows: 2, cols: 4, base_type_index: U32_INDEX },
	Type::Matrix { rows: 3, cols: 2, base_type_index: U32_INDEX },
	Type::Matrix { rows: 3, cols: 3, base_type_index: U32_INDEX },
	Type::Matrix { rows: 3, cols: 4, base_type_index: U32_INDEX },
	Type::Matrix { rows: 4, cols: 2, base_type_index: U32_INDEX },
	Type::Matrix { rows: 4, cols: 3, base_type_index: U32_INDEX },
	Type::Matrix { rows: 4, cols: 4, base_type_index: U32_INDEX },

	Type::Matrix { rows: 2, cols: 2, base_type_index: F16_INDEX },
	Type::Matrix { rows: 2, cols: 3, base_type_index: F16_INDEX },
	Type::Matrix { rows: 2, cols: 4, base_type_index: F16_INDEX },
	Type::Matrix { rows: 3, cols: 2, base_type_index: F16_INDEX },
	Type::Matrix { rows: 3, cols: 3, base_type_index: F16_INDEX },
	Type::Matrix { rows: 3, cols: 4, base_type_index: F16_INDEX },
	Type::Matrix { rows: 4, cols: 2, base_type_index: F16_INDEX },
	Type::Matrix { rows: 4, cols: 3, base_type_index: F16_INDEX },
	Type::Matrix { rows: 4, cols: 4, base_type_index: F16_INDEX },

	Type::Matrix { rows: 2, cols: 2, base_type_index: F32_INDEX },
	Type::Matrix { rows: 2, cols: 3, base_type_index: F32_INDEX },
	Type::Matrix { rows: 2, cols: 4, base_type_index: F32_INDEX },
	Type::Matrix { rows: 3, cols: 2, base_type_index: F32_INDEX },
	Type::Matrix { rows: 3, cols: 3, base_type_index: F32_INDEX },
	Type::Matrix { rows: 3, cols: 4, base_type_index: F32_INDEX },
	Type::Matrix { rows: 4, cols: 2, base_type_index: F32_INDEX },
	Type::Matrix { rows: 4, cols: 3, base_type_index: F32_INDEX },
	Type::Matrix { rows: 4, cols: 4, base_type_index: F32_INDEX },

	Type::Matrix { rows: 2, cols: 2, base_type_index: F64_INDEX },
	Type::Matrix { rows: 2, cols: 3, base_type_index: F64_INDEX },
	Type::Matrix { rows: 2, cols: 4, base_type_index: F64_INDEX },
	Type::Matrix { rows: 3, cols: 2, base_type_index: F64_INDEX },
	Type::Matrix { rows: 3, cols: 3, base_type_index: F64_INDEX },
	Type::Matrix { rows: 3, cols: 4, base_type_index: F64_INDEX },
	Type::Matrix { rows: 4, cols: 2, base_type_index: F64_INDEX },
	Type::Matrix { rows: 4, cols: 3, base_type_index: F64_INDEX },
	Type::Matrix { rows: 4, cols: 4, base_type_index: F64_INDEX },

	// TODO: texture types? should matrices and vectors be genericized better?
];
