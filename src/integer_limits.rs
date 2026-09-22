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

pub fn is_builtin_hash_callee(name: &str) -> bool {
    matches!(
        name,
        "ion_hash_int"
            | "ion_hash_i8"
            | "ion_hash_i16"
            | "ion_hash_i32"
            | "ion_hash_i64"
            | "ion_hash_u8"
            | "ion_hash_u16"
            | "ion_hash_u32"
            | "ion_hash_u64"
            | "ion_hash_uint"
            | "ion_hash_string"
    )
}

/// Compiler `Hash` for integer primitives and `String`.
/// The symbol is a runtime function that takes `&T` (a C pointer).
pub fn builtin_hash_symbol(type_name: &str) -> Option<&'static str> {
    match type_name {
        "int" => Some("ion_hash_int"),
        "i8" => Some("ion_hash_i8"),
        "i16" => Some("ion_hash_i16"),
        "i32" => Some("ion_hash_i32"),
        "i64" => Some("ion_hash_i64"),
        "u8" => Some("ion_hash_u8"),
        "u16" => Some("ion_hash_u16"),
        "u32" => Some("ion_hash_u32"),
        "u64" => Some("ion_hash_u64"),
        "uint" => Some("ion_hash_uint"),
        "String" => Some("ion_hash_string"),
        _ => None,
    }
}

pub fn builtin_hash_type_name(ty: &Type) -> Option<&'static str> {
    match ty {
        Type::Ref { inner, .. } => builtin_hash_type_name(inner),
        Type::Int => Some("int"),
        Type::I8 => Some("i8"),
        Type::I16 => Some("i16"),
        Type::I32 => Some("i32"),
        Type::I64 => Some("i64"),
        Type::U8 => Some("u8"),
        Type::U16 => Some("u16"),
        Type::U32 => Some("u32"),
        Type::U64 => Some("u64"),
        Type::UInt => Some("uint"),
        Type::String => Some("String"),
        _ => None,
    }
}

pub fn is_unsigned_integer(ty: &Type) -> bool {
    matches!(
        ty,
        Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::UInt
    )
}
