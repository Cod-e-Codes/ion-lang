use crate::ast::Type;

/// One integer primitive: Ion name, type, C spelling, width, signedness, limit text, hash symbol.
/// `width` is the fixed C width. It is `None` for `int` and `uint` because shifts use `sizeof(int)`.
/// `promote_bits` is the mixed-arithmetic width. It is 32 for `int` and `uint`.
pub struct IntegerRow {
    pub name: &'static str,
    pub ty: Type,
    pub c_type: &'static str,
    pub c_unsigned: &'static str,
    pub width: Option<u32>,
    pub promote_bits: u32,
    pub signed: bool,
    pub c_min: &'static str,
    pub c_max: &'static str,
    pub hash_symbol: &'static str,
}

pub const INTEGERS: &[IntegerRow] = &[
    IntegerRow {
        name: "int",
        ty: Type::Int,
        c_type: "int",
        c_unsigned: "unsigned int",
        width: None,
        promote_bits: 32,
        signed: true,
        c_min: "(0 - 2147483647 - 1)",
        c_max: "2147483647",
        hash_symbol: "ion_hash_int",
    },
    IntegerRow {
        name: "i8",
        ty: Type::I8,
        c_type: "int8_t",
        c_unsigned: "uint8_t",
        width: Some(8),
        promote_bits: 8,
        signed: true,
        c_min: "(-128)",
        c_max: "127",
        hash_symbol: "ion_hash_i8",
    },
    IntegerRow {
        name: "i16",
        ty: Type::I16,
        c_type: "int16_t",
        c_unsigned: "uint16_t",
        width: Some(16),
        promote_bits: 16,
        signed: true,
        c_min: "(-32768)",
        c_max: "32767",
        hash_symbol: "ion_hash_i16",
    },
    IntegerRow {
        name: "i32",
        ty: Type::I32,
        c_type: "int32_t",
        c_unsigned: "uint32_t",
        width: Some(32),
        promote_bits: 32,
        signed: true,
        c_min: "(0 - 2147483647 - 1)",
        c_max: "2147483647",
        hash_symbol: "ion_hash_i32",
    },
    IntegerRow {
        name: "i64",
        ty: Type::I64,
        c_type: "int64_t",
        c_unsigned: "uint64_t",
        width: Some(64),
        promote_bits: 64,
        signed: true,
        c_min: "((int64_t)0 - (int64_t)9223372036854775807LL - (int64_t)1)",
        c_max: "9223372036854775807LL",
        hash_symbol: "ion_hash_i64",
    },
    IntegerRow {
        name: "u8",
        ty: Type::U8,
        c_type: "uint8_t",
        c_unsigned: "uint8_t",
        width: Some(8),
        promote_bits: 8,
        signed: false,
        c_min: "0",
        c_max: "255",
        hash_symbol: "ion_hash_u8",
    },
    IntegerRow {
        name: "u16",
        ty: Type::U16,
        c_type: "uint16_t",
        c_unsigned: "uint16_t",
        width: Some(16),
        promote_bits: 16,
        signed: false,
        c_min: "0",
        c_max: "65535",
        hash_symbol: "ion_hash_u16",
    },
    IntegerRow {
        name: "u32",
        ty: Type::U32,
        c_type: "uint32_t",
        c_unsigned: "uint32_t",
        width: Some(32),
        promote_bits: 32,
        signed: false,
        c_min: "0",
        c_max: "4294967295U",
        hash_symbol: "ion_hash_u32",
    },
    IntegerRow {
        name: "u64",
        ty: Type::U64,
        c_type: "uint64_t",
        c_unsigned: "uint64_t",
        width: Some(64),
        promote_bits: 64,
        signed: false,
        c_min: "0",
        c_max: "18446744073709551615ULL",
        hash_symbol: "ion_hash_u64",
    },
    IntegerRow {
        name: "uint",
        ty: Type::UInt,
        c_type: "unsigned int",
        c_unsigned: "unsigned int",
        width: None,
        promote_bits: 32,
        signed: false,
        c_min: "0",
        c_max: "4294967295U",
        hash_symbol: "ion_hash_uint",
    },
];

pub fn integer_row(ty: &Type) -> Option<&'static IntegerRow> {
    INTEGERS
        .iter()
        .find(|row| std::mem::discriminant(&row.ty) == std::mem::discriminant(ty))
}

pub fn integer_row_by_name(name: &str) -> Option<&'static IntegerRow> {
    INTEGERS.iter().find(|row| row.name == name)
}

/// Map a primitive type keyword (`int`, `i32`, …) to its `Type`.
pub fn integer_type_from_keyword(name: &str) -> Option<Type> {
    integer_row_by_name(name).map(|row| row.ty.clone())
}

/// `int::MIN` / `i32::MAX` style limits resolve to the corresponding integer type.
pub fn resolve_integer_limit(type_name: &str, member: &str) -> Option<Type> {
    if member != "MIN" && member != "MAX" {
        return None;
    }
    integer_type_from_keyword(type_name)
}

pub fn is_builtin_hash_callee(name: &str) -> bool {
    name == "ion_hash_string" || INTEGERS.iter().any(|row| row.hash_symbol == name)
}

/// Compiler `Hash` for integer primitives and `String`.
/// The symbol is a runtime function that takes `&T` (a C pointer).
pub fn builtin_hash_symbol(type_name: &str) -> Option<&'static str> {
    if type_name == "String" {
        return Some("ion_hash_string");
    }
    integer_row_by_name(type_name).map(|row| row.hash_symbol)
}

pub fn builtin_hash_type_name(ty: &Type) -> Option<&'static str> {
    match ty {
        Type::Ref { inner, .. } => builtin_hash_type_name(inner),
        Type::String => Some("String"),
        other => integer_row(other).map(|row| row.name),
    }
}

pub fn is_unsigned_integer(ty: &Type) -> bool {
    integer_row(ty).is_some_and(|row| !row.signed)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn runtime_header_contains_integer_hash_symbols() {
        let header = include_str!("../runtime/ion_runtime.h");
        for row in INTEGERS {
            assert!(
                header.contains(row.hash_symbol),
                "missing {}",
                row.hash_symbol
            );
        }
    }
}
