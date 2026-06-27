use crate::ast::Type;

/// Map a primitive type keyword (`int`, `i32`, …) to its `Type`.
pub fn integer_type_from_keyword(name: &str) -> Option<Type> {
    match name {
        "int" => Some(Type::Int),
        "i8" => Some(Type::I8),
        "i16" => Some(Type::I16),
        "i32" => Some(Type::I32),
        "i64" => Some(Type::I64),
        "u8" => Some(Type::U8),
        "u16" => Some(Type::U16),
        "u32" => Some(Type::U32),
        "u64" => Some(Type::U64),
        "uint" => Some(Type::UInt),
        _ => None,
    }
}

/// `int::MIN` / `i32::MAX` style limits resolve to the corresponding integer type.
pub fn resolve_integer_limit(type_name: &str, member: &str) -> Option<Type> {
    if member != "MIN" && member != "MAX" {
        return None;
    }
    integer_type_from_keyword(type_name)
}

pub fn is_unsigned_integer(ty: &Type) -> bool {
    matches!(
        ty,
        Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::UInt
    )
}
