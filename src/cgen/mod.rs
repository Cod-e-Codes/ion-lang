mod builtins;
mod drop;
mod types;

use self::types::{
    array_type_name, fn_type_to_c_decl, fn_type_to_c_function_header, format_ret_val_decl,
    int_c_repr, mangle_module_callee, mangle_type_name, resolve_type_alias, ret_val_decl,
    substitute_type_params, tuple_type_name, type_to_c_impl, type_to_c_return_type,
};

use crate::ast::{
    BinOp, EnumDecl, ExternBlock, PatLit, Program, Span, StructDecl, Type, TypeAliasDecl,
    TypeParam, UnOp, synthetic_option_enum,
};
use crate::ir::*;
use crate::tc::TypeInfo;
use crate::types_util::{is_ref_to_vec, ref_to_vec_elem};
use std::collections::{HashMap, HashSet};

type GenericInstantiationGroups = (
    Vec<(StructDecl, Vec<Type>)>,
    Vec<(EnumDecl, Vec<Type>)>,
    Vec<(EnumDecl, Vec<Type>)>,
);

enum BoundsCheck {
    Fixed(usize),
    StringLen,
    SliceLen { by_ref: bool },
}

fn escape_c_comment_text(s: &str) -> String {
    s.replace("*/", "* /")
}

fn primitive_type_from_mangled_name(name: &str) -> Option<Type> {
    if let Some(row) = crate::integer_limits::integer_row_by_name(name) {
        return Some(row.ty.clone());
    }
    match name {
        "bool" => Some(Type::Bool),
        "f32" => Some(Type::F32),
        "f64" => Some(Type::F64),
        _ => None,
    }
}

#[derive(Clone)]
struct ScopeBinding {
    name: String,
    ty: Type,
    dropped: bool,
    read: bool,
}

#[derive(Clone)]
struct ScopeFrame {
    bindings: Vec<ScopeBinding>,
    defers: Vec<IREexpr>,
    /// Join `JoinHandle` bindings still owned here instead of detaching them.
    join_handles: bool,
}

pub struct Codegen {
    output: String,
    indent_level: usize,
    enum_map: HashMap<String, EnumDecl>, // Map enum names to declarations for variant lookups
    generated_types: HashMap<String, bool>, // Track which monomorphized types have been generated
    struct_map: HashMap<String, StructDecl>, // Map struct names to declarations
    drop_impls: HashMap<String, String>,
    generic_instantiations: HashMap<String, (String, Vec<Type>)>, // Map base name to (monomorphized_name, params)
    match_counter: usize, // Counter for unique match variable names
    type_aliases: HashMap<String, TypeAliasDecl>, // Map type alias names to their declarations
    extern_functions: HashMap<String, Vec<Type>>, // Map extern function names to their parameter types
    current_return_type: Option<Type>, // Current function's return type (for array return handling)
    function_return_types: HashMap<String, Option<Type>>, // Map function names to their return types
    function_param_types: HashMap<String, Vec<Type>>,     // Map function names to parameter types
    /// Compilation-wide callee env from TypeInfo (Ion names, alias::name, prefix_name).
    compilation_param_types: HashMap<String, Vec<Type>>,
    compilation_return_types: HashMap<String, Option<Type>>,
    closures: HashMap<String, crate::tc::ClosureSig>,
    protocols: HashMap<String, crate::ast::ProtocolDecl>,
    proto_emitted: std::collections::HashSet<String>,
    in_unsafe_block: bool, // Track if we're in an unsafe block
    /// Address-of an index must apply `&` to the element, which is an lvalue.
    addressing_index: bool,
    /// When true, enum literals emit nested designated initializers without a type cast.
    nested_designated_init: bool,
    temp_var_counter: usize, // Counter for unique temporary variable names
    current_function_params: HashMap<String, Type>, // Track current function parameter types for field access
    spawn_counter: usize,
    /// The next `generate_block` joins owned handles at exit.
    next_block_joins: bool,
    spawn_forward_decls: String,
    spawn_definitions: String,
    fn_literal_forward_decls: String,
    fn_literal_definitions: String,
    generated_fn_literals: std::collections::HashSet<String>,
    scope_stack: Vec<ScopeFrame>,
    epilogue_label: String,
    loop_continue_label: Option<String>,
    loop_break_label: Option<String>,
    loop_break_label_used: bool,
    /// Index of the current loop body frame (`scope_stack.len()` before `generate_block` of the body).
    loop_unwind_depth: usize,
    match_in_switch: u32,
    /// C temps for nested `match` (`generate_match_block` / rvalue match). Parent is
    /// `stack[len-2]` so a nested catch-all can bind the outer scrutinee.
    match_scrutinee_stack: Vec<String>,
    /// When true (single-file merge), module calls use `{alias}_{func}` C names.
    mangle_merged_module_calls: bool,
    /// Set during multi-file codegen: prefix this module's functions (`io_print_int`).
    multi_file_module: Option<String>,
    /// Owned struct fields moved in the current statement; flushed after the statement completes.
    pending_field_nulls: Vec<String>,
    pending_field_null_set: std::collections::HashSet<String>,
}

impl Default for Codegen {
    fn default() -> Self {
        Self::new()
    }
}

impl Codegen {
    pub fn new() -> Self {
        Codegen {
            output: String::new(),
            indent_level: 0,
            enum_map: HashMap::new(),
            generated_types: HashMap::new(),
            struct_map: HashMap::new(),
            drop_impls: HashMap::new(),
            generic_instantiations: HashMap::new(),
            match_counter: 0,
            type_aliases: HashMap::new(),
            extern_functions: HashMap::new(),
            current_return_type: None,
            function_return_types: HashMap::new(),
            function_param_types: HashMap::new(),
            compilation_param_types: HashMap::new(),
            compilation_return_types: HashMap::new(),
            closures: HashMap::new(),
            protocols: HashMap::new(),
            proto_emitted: std::collections::HashSet::new(),
            in_unsafe_block: false,
            addressing_index: false,
            nested_designated_init: false,
            temp_var_counter: 0,
            current_function_params: HashMap::new(),
            spawn_counter: 0,
            next_block_joins: false,
            spawn_forward_decls: String::new(),
            spawn_definitions: String::new(),
            fn_literal_forward_decls: String::new(),
            fn_literal_definitions: String::new(),
            generated_fn_literals: std::collections::HashSet::new(),
            scope_stack: Vec::new(),
            epilogue_label: "epilogue".to_string(),
            loop_continue_label: None,
            loop_break_label: None,
            loop_break_label_used: false,
            loop_unwind_depth: 0,
            match_in_switch: 0,
            match_scrutinee_stack: Vec::new(),
            mangle_merged_module_calls: false,
            multi_file_module: None,
            pending_field_nulls: Vec::new(),
            pending_field_null_set: std::collections::HashSet::new(),
        }
    }

    pub fn set_type_info(&mut self, types: &TypeInfo) {
        self.compilation_param_types = types.function_params.clone();
        self.compilation_return_types = types.function_returns.clone();
        self.closures = types.closures.clone();
        self.protocols = types.protocols.clone();
    }

    fn lookup_param_types(&self, resolved_callee: &str, func_name: &str) -> Option<&Vec<Type>> {
        self.function_param_types
            .get(resolved_callee)
            .or_else(|| self.function_param_types.get(func_name))
            .or_else(|| self.compilation_param_types.get(resolved_callee))
            .or_else(|| self.compilation_param_types.get(func_name))
            .or_else(|| self.extern_functions.get(func_name))
    }

    fn lookup_call_param_types(&self, resolved_callee: &str, func_name: &str) -> Option<Vec<Type>> {
        if let Some(params) = self.lookup_param_types(resolved_callee, func_name) {
            return Some(params.clone());
        }
        match self
            .lookup_var_type(resolved_callee)
            .or_else(|| self.lookup_var_type(func_name))
        {
            Some(Type::Fn { params, .. }) => Some(params),
            _ => None,
        }
    }

    fn parent_match_scrutinee(&self) -> Option<&str> {
        let n = self.match_scrutinee_stack.len();
        if n >= 2 {
            Some(self.match_scrutinee_stack[n - 2].as_str())
        } else {
            self.match_scrutinee_stack.last().map(String::as_str)
        }
    }

    fn lookup_return_type(&self, callee: &str) -> Option<&Option<Type>> {
        self.function_return_types
            .get(callee)
            .or_else(|| self.compilation_return_types.get(callee))
    }

    fn resolve_c_function_name(&self, callee: &str) -> String {
        if callee == "main" {
            return "main".to_string();
        }
        if self.extern_functions.contains_key(callee) {
            return callee.to_string();
        }
        let mangle_modules = self.mangle_merged_module_calls || self.multi_file_module.is_some();
        if mangle_modules && let Some(mangled) = mangle_module_callee(callee) {
            return mangled;
        }
        if let Some(module) = &self.multi_file_module {
            if callee.contains("::") {
                return callee.split("::").last().unwrap_or(callee).to_string();
            }
            return format!("{}_{}", module, callee);
        }
        if callee.contains("::") {
            callee.split("::").last().unwrap_or(callee).to_string()
        } else {
            callee.to_string()
        }
    }

    fn module_c_symbol(&self, ion_name: &str) -> String {
        self.resolve_c_function_name(ion_name)
    }

    fn emit_generated_banner(&mut self, source_ion: &str) {
        let source_ion = escape_c_comment_text(source_ion);
        self.writeln(&format!(
            "/* Generated by ion-compiler from {}. Do not edit.",
            source_ion
        ));
        self.writeln(" * GNU C (GCC/Clang). Merged stdlib/import code may appear below. */");
        self.writeln("");
    }

    fn should_silence_unused_binding(name: &str, ty: &Type) -> bool {
        matches!(ty, Type::Ref { .. } | Type::RawPtr { .. }) || name.starts_with('_')
    }

    fn emit_silence_unused_binding(&mut self, name: &str) {
        self.write_indent();
        self.writeln(&format!("(void){name};"));
    }

    fn enum_variant_index(enum_decl: &EnumDecl, variant_name: &str) -> Option<usize> {
        enum_decl
            .variants
            .iter()
            .position(|v| v.name == variant_name)
    }

    fn capture_expr_code(&mut self, expr: &IREexpr) -> String {
        let mut captured = String::new();
        let old_output = std::mem::replace(&mut self.output, captured);
        self.generate_expr(expr);
        captured = std::mem::replace(&mut self.output, old_output);
        captured
    }

    fn c_enum_literal(
        &self,
        c_type_name: &str,
        enum_name: &str,
        variant_name: &str,
        payload_c: Option<&str>,
    ) -> String {
        let idx = self
            .enum_map
            .get(enum_name)
            .and_then(|d| Self::enum_variant_index(d, variant_name))
            .unwrap_or(0);
        if let Some(payload) = payload_c {
            format!(
                "({c_type_name}){{ .tag = {idx}, .data = {{ .variant_{idx} = {{ .arg0 = {payload} }} }} }}"
            )
        } else {
            format!("({c_type_name}){{ .tag = {idx}, .data = {{ }} }}")
        }
    }

    fn channel_drop_fn_ptr(&self, elem_type: &Type) -> String {
        if self.type_needs_drop(elem_type) {
            self.channel_elem_drop_name(elem_type)
        } else {
            "NULL".to_string()
        }
    }

    fn emit_ion_channel_new(
        &mut self,
        elem_type: &Type,
        args: &[IREexpr],
        tx_name: &str,
        rx_name: &str,
    ) {
        let elem_c_type = self.type_to_c(elem_type);
        let cap = if let Some(arg) = args.first() {
            self.capture_expr_code(arg)
        } else {
            "1".to_string()
        };
        let drop_fn = self.channel_drop_fn_ptr(elem_type);
        self.write_indent();
        self.writeln(&format!(
            "if (ion_channel_new(sizeof({elem_c_type}), {cap}, {drop_fn}, &{tx_name}, &{rx_name}) != 0) ion_panic(\"channel create failed\");"
        ));
    }

    fn ensure_proto_typedef(&mut self, name: &str) {
        if !self.proto_emitted.insert(name.to_string()) {
            return;
        }
        let Some(proto) = self.protocols.get(name).cloned() else {
            return;
        };
        let mut def = "typedef struct {\n    int tag;\n    union {\n".to_string();
        for (i, step) in proto.steps.iter().enumerate() {
            let ty = match step {
                crate::ast::ProtocolStep::Send(ty) | crate::ast::ProtocolStep::Recv(ty) => ty,
            };
            def.push_str(&format!("        {} p{i};\n", self.type_to_c(ty)));
        }
        def.push_str(&format!("    }} payload;\n}} ion_proto_{name};\n"));
        self.spawn_forward_decls.push_str(&def);
    }

    fn endpoint_pair_code(&mut self, name: &str) -> String {
        self.ensure_proto_typedef(name);
        let msg = format!("ion_proto_{name}");
        let ep = Type::Endpoint {
            protocol: name.to_string(),
            step: 0,
            dual: false,
        };
        let tuple = Type::Tuple {
            elements: vec![ep.clone(), ep],
        };
        let tuple_c = self.type_to_c(&tuple);
        format!(
            "({{ ion_sender_t _c_tx, _s_tx; ion_receiver_t _c_rx, _s_rx; if (ion_channel_new(sizeof({msg}), 1, NULL, &_c_tx, &_s_rx) != 0 || ion_channel_new(sizeof({msg}), 1, NULL, &_s_tx, &_c_rx) != 0) ion_panic(\"endpoint create failed\"); {tuple_c} _pair; _pair.f0 = (ion_endpoint_t){{ _c_tx, _c_rx }}; _pair.f1 = (ion_endpoint_t){{ _s_tx, _s_rx }}; _pair; }})"
        )
    }

    fn emit_endpoint_send(
        &mut self,
        protocol: &str,
        step: usize,
        channel: &IREexpr,
        value: &IREexpr,
        value_type: &Type,
    ) {
        self.ensure_proto_typedef(protocol);
        let msg = format!("ion_proto_{protocol}");
        self.write("({ ion_endpoint_t _ep = ");
        self.generate_expr(channel);
        self.write(&format!(
            "; {msg} _msg = {{0}}; _msg.tag = {step}; _msg.payload.p{step} = "
        ));
        self.generate_expr_with_type(value, Some(value_type));
        self.write(
            "; if (ion_channel_send(&_ep.tx, &_msg) != 0) ion_panic(\"session send failed\"); _ep; })",
        );
    }

    fn emit_endpoint_recv(&mut self, protocol: &str, step: usize, channel: &IREexpr) {
        self.ensure_proto_typedef(protocol);
        let proto = self
            .protocols
            .get(protocol)
            .cloned()
            .expect("protocol exists");
        let payload = match proto.steps.get(step) {
            Some(crate::ast::ProtocolStep::Send(ty) | crate::ast::ProtocolStep::Recv(ty)) => {
                ty.clone()
            }
            None => Type::Void,
        };
        let next = Type::Endpoint {
            protocol: protocol.to_string(),
            step: step + 1,
            dual: false,
        };
        let tuple = Type::Tuple {
            elements: vec![payload, next],
        };
        let tuple_c = self.type_to_c(&tuple);
        let msg = format!("ion_proto_{protocol}");
        self.write("({ ion_endpoint_t _ep = ");
        self.generate_expr(channel);
        self.write(&format!(
            "; {msg} _msg = {{0}}; if (ion_channel_recv(&_ep.rx, &_msg) != 0) ion_panic(\"session recv failed\"); {tuple_c} _got; _got.f0 = _msg.payload.p{step}; _got.f1 = _ep; _got; }})"
        ));
    }

    fn sender_addr_code(&mut self, channel: &IREexpr) -> String {
        if let IREexpr::AddressOf { inner, .. } = channel {
            format!("&{}", self.capture_expr_code(inner))
        } else {
            format!("&{}", self.capture_expr_code(channel))
        }
    }

    fn monomorphized_payload_type(
        payload_ty: &Type,
        enum_decl: &EnumDecl,
        type_context: Option<&Type>,
    ) -> Type {
        let Some(Type::Generic { params, .. }) = type_context else {
            return payload_ty.clone();
        };
        let names = TypeParam::names(&enum_decl.generics);
        if names.len() != params.len() {
            return payload_ty.clone();
        }
        let subst: HashMap<String, &Type> = names.into_iter().zip(params.iter()).collect();
        substitute_type_params(payload_ty, &subst)
    }

    fn emit_enum_variant_compound_literal(
        &mut self,
        c_type_name: &str,
        enum_base_name: &str,
        variant_name: &str,
        args: &[IREexpr],
        named_fields: Option<&[(String, IREexpr)]>,
        type_context: Option<&Type>,
    ) {
        let enum_decl = self
            .enum_map
            .get(enum_base_name)
            .cloned()
            .unwrap_or_else(|| panic!("unknown enum `{enum_base_name}` in enum literal codegen"));
        let variant_idx = Self::enum_variant_index(&enum_decl, variant_name).unwrap_or_else(|| {
            panic!("unknown variant `{variant_name}` on enum `{enum_base_name}`")
        });
        let variant = &enum_decl.variants[variant_idx];
        let has_payloads = !variant.payload_types.is_empty() || variant.named_fields.is_some();
        let payload_tys: Vec<Type> = variant
            .payload_types
            .iter()
            .map(|ty| Self::monomorphized_payload_type(ty, &enum_decl, type_context))
            .collect();
        let named_field_tys: HashMap<String, Type> = variant
            .named_fields
            .as_ref()
            .map(|fields| {
                fields
                    .iter()
                    .map(|(name, ty)| {
                        (
                            name.clone(),
                            Self::monomorphized_payload_type(ty, &enum_decl, type_context),
                        )
                    })
                    .collect()
            })
            .unwrap_or_default();

        if self.nested_designated_init {
            self.write(&format!("{{ .tag = {variant_idx}, .data = {{"));
        } else {
            self.write(&format!(
                "({c_type_name}){{ .tag = {variant_idx}, .data = {{"
            ));
        }
        if has_payloads {
            self.write(&format!(" .variant_{variant_idx} = {{"));
            if let Some(named_fields) = named_fields {
                for (i, (field_name, field_expr)) in named_fields.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.write(&format!(" .{field_name} = "));
                    self.generate_expr_with_type(field_expr, named_field_tys.get(field_name));
                }
            } else {
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.write(&format!(" .arg{i} = "));
                    self.generate_expr_with_type(arg, payload_tys.get(i));
                }
            }
            self.write(" }");
        }
        self.write(" } }");
    }

    fn param_is_byte_ptr(param_ty: &Type) -> bool {
        match param_ty {
            Type::RawPtr { inner } => matches!(**inner, Type::U8),
            Type::Ref { inner, .. } => matches!(**inner, Type::U8),
            _ => false,
        }
    }

    fn escape_c_string_literal_content(value: &str) -> String {
        value
            .replace('\\', "\\\\")
            .replace('"', "\\\"")
            .replace('\n', "\\n")
            .replace('\r', "\\r")
            .replace('\t', "\\t")
            .replace('\0', "\\0")
    }

    fn write_ion_string_from_literal(&mut self, value: &str) {
        let escaped = Self::escape_c_string_literal_content(value);
        let n = self.temp_var_counter;
        self.temp_var_counter += 1;
        self.write(&format!(
            "({{ ion_string_t* _ion_lit{n} = ion_string_from_literal(\"{escaped}\", {}); if (!_ion_lit{n}) ion_panic(\"String allocation failed\"); _ion_lit{n}; }})",
            value.len()
        ));
    }

    pub fn generate(&mut self, program: &IRProgram, source_ion: &str) -> String {
        self.output.clear();
        self.indent_level = 0;
        self.mangle_merged_module_calls = true;
        // Build enum map for variant index lookups
        self.enum_map.clear();
        for e in &program.enums {
            self.enum_map.insert(e.name.clone(), e.clone());
        }
        // Build struct map for generic struct lookups
        self.struct_map.clear();
        for s in &program.structs {
            self.struct_map.insert(s.name.clone(), s.clone());
        }
        self.drop_impls = program.drop_impls.clone();
        // Build type alias map for type resolution
        self.type_aliases.clear();
        for alias in &program.type_aliases {
            self.type_aliases.insert(alias.name.clone(), alias.clone());
        }
        // Build function return type map
        self.function_return_types.clear();
        self.function_param_types.clear();
        for function in &program.functions {
            self.function_return_types
                .insert(function.name.clone(), function.return_type.clone());
            self.function_param_types.insert(
                function.name.clone(),
                function.params.iter().map(|p| p.ty.clone()).collect(),
            );
        }
        self.generated_types.clear();
        self.spawn_counter = 0;
        self.spawn_forward_decls.clear();
        self.spawn_definitions.clear();
        self.fn_literal_forward_decls.clear();
        self.fn_literal_definitions.clear();
        self.generated_fn_literals.clear();
        self.match_scrutinee_stack.clear();

        self.emit_generated_banner(source_ion);

        // Generate includes if needed
        self.writeln("#include <stdio.h>");
        self.writeln("#include <stdlib.h>");
        self.writeln("#include <string.h>");
        self.writeln("#include <stddef.h>");
        self.writeln("#include <stdint.h>"); // For integer types (int8_t, uint16_t, etc.)
        // Ion runtime (threads, channels, heap allocation, etc.)
        // Use ion_runtime.h and rely on compiler include paths
        self.writeln("#include \"ion_runtime.h\"");
        self.writeln("");

        // Collect all generic type instantiations used in the program
        // Store as (base_name, params) pairs - deduplicate using string keys
        let mut generic_instantiations_map: std::collections::HashMap<String, (String, Vec<Type>)> =
            std::collections::HashMap::new();
        collect_generic_instantiations(program, &mut generic_instantiations_map);

        // Also collect resolved generic types from type aliases
        // When we have Result<int> = Option<int>, we need to collect Option<int> too
        let mut resolved_instantiations = generic_instantiations_map.clone();
        for (base_name, params) in generic_instantiations_map.values() {
            // Resolve the type alias if it exists
            let ty = Type::Generic {
                name: base_name.clone(),
                params: params.clone(),
            };
            let resolved = resolve_type_alias(&ty, &self.type_aliases);
            // If resolved to a different generic type, collect it too
            if let Type::Generic {
                name: resolved_name,
                params: resolved_params,
            } = resolved
                && resolved_name != *base_name
            {
                let key = mangle_type_name(&resolved_name, &resolved_params);
                resolved_instantiations.insert(key, (resolved_name, resolved_params));
            }
        }

        self.generic_instantiations = resolved_instantiations;

        let array_typedefs = collect_array_typedefs(program);

        // Vec and slice typedefs must precede struct fields that reference them.
        // Tuple typedefs wait until generic enums (e.g. Option_int) are complete types.
        self.emit_vec_slice_typedefs(program);
        self.emit_ready_array_typedefs(&array_typedefs);

        // Forwards so Option<Box<Node>> / Option<&Op> can mention Node* / Op*
        // before those bodies exist.
        self.emit_non_generic_type_forwards(program);

        // Ensure Option template is available for monomorphization (builtin or user).
        self.ensure_option_template();

        self.emit_monomorphized_enum_forwards();

        let mut instantiations_vec: Vec<_> = self.generic_instantiations.values().collect();
        instantiations_vec.sort_by_key(|(name, _)| name.clone());

        let (struct_instantiations, early_enum_instantiations, late_enum_instantiations) =
            self.partition_generic_instantiations(&instantiations_vec);

        // Early enums: payloads only need pointers / primitives (e.g. Option<Box<Node>>).
        for (decl, params) in &early_enum_instantiations {
            self.generate_monomorphized_enum(decl, params);
            let key = mangle_type_name(&decl.name, params);
            self.generated_types.insert(key, true);
        }

        let mut early_option_types: Vec<(String, Vec<Type>)> = Vec::new();
        let mut late_option_types: Vec<(String, Vec<Type>)> = Vec::new();
        for (key, (base_name, params)) in &self.generic_instantiations {
            if base_name == "Option" && !self.generated_types.contains_key(key) {
                if params_complete_with_struct_forwards(params) {
                    early_option_types.push((key.clone(), params.clone()));
                } else {
                    late_option_types.push((key.clone(), params.clone()));
                }
            }
        }
        for (key, params) in early_option_types {
            if self.generated_types.contains_key(&key) {
                continue;
            }
            let option_decl = self.enum_map.get("Option").unwrap().clone();
            self.generate_monomorphized_enum(&option_decl, &params);
            self.generated_types.insert(key, true);
        }

        self.emit_ready_array_typedefs(&array_typedefs);

        // Non-generic enums after Option_int and peers so payloads like Option<int>
        // are complete C types (Hold { H(Option<int>) }).
        for e in &program.enums {
            if e.generics.is_empty() {
                self.generate_enum_type(e);
            }
        }
        self.emit_ready_array_typedefs(&array_typedefs);

        self.emit_tuple_typedefs(program);

        for (decl, params) in struct_instantiations {
            let key = mangle_type_name(&decl.name, &params);
            self.generate_monomorphized_struct(&decl, &params);
            self.generated_types.insert(key, true);
        }

        self.emit_non_generic_struct_bodies(program);
        self.emit_ready_array_typedefs(&array_typedefs);

        let mut late_pending: Vec<(EnumDecl, Vec<Type>)> = late_enum_instantiations;
        for (key, params) in late_option_types {
            if self.generated_types.contains_key(&key) {
                continue;
            }
            let option_decl = self.enum_map.get("Option").unwrap().clone();
            late_pending.push((option_decl, params));
        }
        self.emit_enum_instantiations_ready_first(late_pending);
        self.emit_ready_array_typedefs(&array_typedefs);

        self.emit_vec_primitive_options(program);

        self.emit_ready_array_typedefs(&array_typedefs);

        self.emit_named_drop_functions();
        self.emit_channel_drop_functions(program);

        // Generate extern function prototypes
        for extern_block in &program.extern_blocks {
            self.generate_extern_block(extern_block);
        }

        // Generate function prototypes (forward declarations) for ALL functions
        // This is required for single-file mode where functions may be called before definition
        let proto_insert_pos = self.output.len();
        for func in &program.functions {
            let param_list = self.format_ir_param_list_c(&func.params);
            if let Some(ret_ty) = func.return_type.as_ref() {
                let resolved = resolve_type_alias(ret_ty, &self.type_aliases);
                if matches!(resolved, Type::Fn { .. }) {
                    self.writeln(&format!(
                        "{};",
                        fn_type_to_c_function_header(&func.name, &param_list, &resolved)
                    ));
                    continue;
                }
            }
            let return_type = func
                .return_type
                .as_ref()
                .map(|ty| type_to_c_return_type(&resolve_type_alias(ty, &self.type_aliases)))
                .unwrap_or_else(|| "void".to_string());
            self.write(&format!("{} {}(", return_type, func.name));
            self.write(&param_list);
            self.writeln(");");
        }

        // Generate each function (populates spawn forward/definition buffers)
        for function in &program.functions {
            self.generate_function(function);
        }

        self.insert_spawn_fn_literal_forward_decls(proto_insert_pos);

        if !self.spawn_definitions.is_empty() || !self.fn_literal_definitions.is_empty() {
            self.writeln("");
            let mut trailing_defs = self.spawn_definitions.clone();
            trailing_defs.push_str(&self.fn_literal_definitions);
            self.write(&trailing_defs);
        }

        self.output.clone()
    }

    /// Generate C source file for a module (for multi-file mode)
    pub fn generate_module_source(
        &mut self,
        program: &IRProgram,
        symbol_prefix: &str,
        source_ion: &str,
        imports: &[String],
        header_stem: &str,
    ) -> String {
        self.output.clear();
        self.indent_level = 0;
        self.mangle_merged_module_calls = false;
        self.multi_file_module = Some(symbol_prefix.to_string());
        // Build enum map for variant index lookups
        self.enum_map.clear();
        for e in &program.enums {
            self.enum_map.insert(e.name.clone(), e.clone());
        }
        // Build struct map for generic struct lookups
        self.struct_map.clear();
        for s in &program.structs {
            self.struct_map.insert(s.name.clone(), s.clone());
        }
        self.drop_impls = program.drop_impls.clone();
        self.generated_types.clear();
        self.spawn_counter = 0;
        self.spawn_forward_decls.clear();
        self.spawn_definitions.clear();
        self.fn_literal_forward_decls.clear();
        self.fn_literal_definitions.clear();
        self.generated_fn_literals.clear();
        self.match_scrutinee_stack.clear();

        self.extern_functions.clear();
        self.function_return_types.clear();
        self.function_param_types.clear();
        for extern_block in &program.extern_blocks {
            Self::assert_c_extern_linkage(extern_block);
            for ext_fn in &extern_block.functions {
                self.extern_functions.insert(
                    ext_fn.name.clone(),
                    ext_fn.params.iter().map(|p| p.ty.clone()).collect(),
                );
            }
        }
        for function in &program.functions {
            let c_name = format!("{}_{}", symbol_prefix, function.name);
            self.function_return_types
                .insert(c_name.clone(), function.return_type.clone());
            self.function_param_types.insert(
                c_name,
                function.params.iter().map(|p| p.ty.clone()).collect(),
            );
        }

        self.emit_generated_banner(source_ion);

        // Generate includes
        self.writeln("#include <stdio.h>");
        self.writeln("#include <stdlib.h>");
        self.writeln("#include <string.h>");
        // Runtime header - use ion_runtime.h and rely on compiler include paths
        // The compiler will add -I. -I.. -Iruntime -I../runtime to find it
        self.writeln("#include \"ion_runtime.h\"");
        // Include the module's own header
        self.writeln(&format!("#include \"{}.h\"", header_stem));
        // Include headers for imported modules
        for import_name in imports {
            if import_name != header_stem {
                self.writeln(&format!("#include \"{}.h\"", import_name));
            }
        }
        self.writeln("");

        // Collect all generic type instantiations used in the program
        let mut generic_instantiations_map: std::collections::HashMap<String, (String, Vec<Type>)> =
            std::collections::HashMap::new();
        collect_generic_instantiations(program, &mut generic_instantiations_map);
        self.generic_instantiations = generic_instantiations_map.clone();

        let array_typedefs = collect_array_typedefs(program);

        self.emit_vec_slice_typedefs(program);
        self.emit_ready_array_typedefs(&array_typedefs);

        self.emit_non_generic_type_forwards(program);
        self.ensure_option_template();
        self.emit_monomorphized_enum_forwards();

        let mut instantiations_vec: Vec<_> = generic_instantiations_map.values().collect();
        instantiations_vec.sort_by_key(|(name, _)| name.clone());

        let (struct_instantiations, early_enum_instantiations, late_enum_instantiations) =
            self.partition_generic_instantiations(&instantiations_vec);

        for (decl, params) in &early_enum_instantiations {
            self.generate_monomorphized_enum(decl, params);
            let key = mangle_type_name(&decl.name, params);
            self.generated_types.insert(key, true);
        }
        for (key, (base_name, params)) in &generic_instantiations_map.clone() {
            if base_name == "Option"
                && !self.generated_types.contains_key(key)
                && params_complete_with_struct_forwards(params)
            {
                let option_decl = self.enum_map.get("Option").unwrap().clone();
                self.generate_monomorphized_enum(&option_decl, params);
                self.generated_types.insert(key.clone(), true);
            }
        }
        self.emit_ready_array_typedefs(&array_typedefs);

        for s in &program.structs {
            if s.generics.is_empty() {
                self.emit_struct_typedef(s);
            }
        }

        for e in &program.enums {
            if e.generics.is_empty() {
                self.generate_enum_type(e);
            }
        }
        self.emit_ready_array_typedefs(&array_typedefs);

        self.emit_tuple_typedefs(program);

        for (decl, params) in struct_instantiations {
            let key = mangle_type_name(&decl.name, &params);
            self.generate_monomorphized_struct(&decl, &params);
            self.generated_types.insert(key, true);
        }
        self.emit_enum_instantiations_ready_first(late_enum_instantiations);
        self.emit_ready_array_typedefs(&array_typedefs);

        // Remaining Option instantiations that needed complete struct payloads.
        for (key, (base_name, params)) in &generic_instantiations_map.clone() {
            if base_name == "Option" && !self.generated_types.contains_key(key) {
                let option_decl = self.enum_map.get("Option").unwrap().clone();
                self.generate_monomorphized_enum(&option_decl, params);
                self.generated_types.insert(key.clone(), true);
            }
        }
        self.emit_ready_array_typedefs(&array_typedefs);

        self.emit_vec_primitive_options(program);

        self.emit_ready_array_typedefs(&array_typedefs);

        self.emit_named_drop_functions();
        self.emit_channel_drop_functions(program);

        // Generate extern function prototypes (declarations only, implementations come from headers)
        for extern_block in &program.extern_blocks {
            Self::assert_c_extern_linkage(extern_block);
            for ext_fn in &extern_block.functions {
                let return_type = ext_fn
                    .return_type
                    .as_ref()
                    .map(|t| self.type_to_c(t))
                    .unwrap_or_else(|| "void".to_string());
                self.write(&format!("{} {}(", return_type, ext_fn.name));

                // Generate parameters
                if ext_fn.params.is_empty() && !ext_fn.variadic {
                    self.write("void");
                } else {
                    for (i, param) in ext_fn.params.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        // Special handling for array types: C syntax is "int arr[3]" not "int[3] arr"
                        match &param.ty {
                            Type::Array { inner, size, .. } => {
                                let base_type = self.type_to_c(inner);
                                self.write(&format!("{} {}[{}]", base_type, param.name, size));
                            }
                            Type::Fn { .. } => {
                                self.write(&fn_type_to_c_decl(&param.ty, &param.name));
                            }
                            _ => {
                                self.write(&format!(
                                    "{} {}",
                                    self.type_to_c(&param.ty),
                                    param.name
                                ));
                            }
                        }
                    }
                    if ext_fn.variadic {
                        if !ext_fn.params.is_empty() {
                            self.write(", ");
                        }
                        self.write("...");
                    }
                }

                self.writeln(");");
            }
        }
        self.writeln("");

        // Generate function implementations (both public and private)
        let impl_insert_pos = self.output.len();
        for function in &program.functions {
            self.generate_function(function);
        }

        self.insert_spawn_fn_literal_forward_decls(impl_insert_pos);

        if !self.spawn_definitions.is_empty() || !self.fn_literal_definitions.is_empty() {
            self.writeln("");
            let mut trailing_defs = self.spawn_definitions.clone();
            trailing_defs.push_str(&self.fn_literal_definitions);
            self.write(&trailing_defs);
        }

        self.multi_file_module = None;
        self.output.clone()
    }

    /// Generate C header file for a module (for multi-file mode)
    pub fn generate_module_header(
        &mut self,
        program: &Program,
        symbol_prefix: &str,
        header_stem: &str,
    ) -> String {
        self.output.clear();
        self.indent_level = 0;

        // Generate include guard
        let guard_name = format!("ION_{}_H", header_stem.to_uppercase().replace("-", "_"));
        self.writeln(&format!("#ifndef {}", guard_name));
        self.writeln(&format!("#define {}", guard_name));
        self.writeln("");

        // Include necessary headers
        self.writeln("#include <stdint.h>");
        self.writeln("#include \"ion_runtime.h\"");
        self.writeln("");

        let mut header_arrays: HashMap<String, Type> = HashMap::new();
        for s in &program.structs {
            if s.pub_ {
                for field in &s.fields {
                    collect_array_from_type(&field.ty, &mut header_arrays);
                }
            }
        }
        for e in &program.enums {
            if e.pub_ {
                for variant in &e.variants {
                    for ty in &variant.payload_types {
                        collect_array_from_type(ty, &mut header_arrays);
                    }
                    if let Some(fields) = &variant.named_fields {
                        for (_, ty) in fields {
                            collect_array_from_type(ty, &mut header_arrays);
                        }
                    }
                }
            }
        }
        for f in &program.functions {
            if !f.pub_ {
                continue;
            }
            if let Some(ret) = &f.return_type {
                collect_array_from_type(ret, &mut header_arrays);
            }
            for p in &f.params {
                collect_array_from_type(&p.ty, &mut header_arrays);
            }
        }
        let mut header_array_typedefs: Vec<(String, Type)> = header_arrays.into_iter().collect();
        header_array_typedefs.sort_by(|a, b| a.0.cmp(&b.0));
        self.emit_ready_array_typedefs(&header_array_typedefs);

        // Generate public struct definitions
        for s in &program.structs {
            if s.pub_ && s.generics.is_empty() {
                self.emit_struct_typedef(s);
            }
        }

        // Generate public enum definitions
        for e in &program.enums {
            if e.pub_ && e.generics.is_empty() {
                self.generate_enum_type(e);
            }
        }

        // Generate public function prototypes
        for func in &program.functions {
            if func.pub_ {
                let return_type = func
                    .return_type
                    .as_ref()
                    .map(type_to_c_impl)
                    .unwrap_or_else(|| "void".to_string());
                let c_name = format!("{}_{}", symbol_prefix, func.name);
                self.write(&format!("{} {}(", return_type, c_name));
                if func.params.is_empty() {
                    self.write("void");
                } else {
                    for (i, param) in func.params.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        // Special handling for array types: C syntax is "int arr[3]" not "int[3] arr"
                        match &param.ty {
                            Type::Array { inner, size, .. } => {
                                let base_type = type_to_c_impl(inner);
                                self.write(&format!("{} {}[{}]", base_type, param.name, size));
                            }
                            Type::Fn { .. } => {
                                self.write(&fn_type_to_c_decl(&param.ty, &param.name));
                            }
                            _ => {
                                self.write(&format!(
                                    "{} {}",
                                    type_to_c_impl(&param.ty),
                                    param.name
                                ));
                            }
                        }
                    }
                }
                self.writeln(");");
            }
        }

        // Close include guard
        self.writeln("");
        self.writeln(&format!("#endif // {}", guard_name));

        self.output.clone()
    }

    fn struct_decl_for_type(&self, ty: &Type) -> Option<(&StructDecl, HashMap<String, Type>)> {
        let resolved = resolve_type_alias(ty, &self.type_aliases);
        let (base_name, params) = match &resolved {
            Type::Struct(name) if self.struct_map.contains_key(name) => (name.as_str(), Vec::new()),
            Type::Generic { name, params } if self.struct_map.contains_key(name) => {
                (name.as_str(), params.clone())
            }
            _ => return None,
        };
        let decl = self.struct_map.get(base_name)?;
        let mut substitutions = HashMap::new();
        for (i, gen_param) in decl.generics.iter().enumerate() {
            if i < params.len() {
                substitutions.insert(gen_param.name.clone(), params[i].clone());
            }
        }
        Some((decl, substitutions))
    }

    fn enum_decl_for_type(&self, ty: &Type) -> Option<(&EnumDecl, HashMap<String, Type>)> {
        let resolved = resolve_type_alias(ty, &self.type_aliases);
        let (base_name, params) = match &resolved {
            Type::Enum(name) => (name.as_str(), Vec::new()),
            Type::Struct(name) if self.enum_map.contains_key(name) => (name.as_str(), Vec::new()),
            Type::Generic { name, params } if self.enum_map.contains_key(name) => {
                (name.as_str(), params.clone())
            }
            _ => return None,
        };
        let decl = self.enum_map.get(base_name)?;
        let mut substitutions = HashMap::new();
        for (i, gen_param) in decl.generics.iter().enumerate() {
            if i < params.len() {
                substitutions.insert(gen_param.name.clone(), params[i].clone());
            }
        }
        Some((decl, substitutions))
    }

    fn substitute_field_types(ty: &Type, substitutions: &HashMap<String, Type>) -> Type {
        let refs: HashMap<String, &Type> =
            substitutions.iter().map(|(k, v)| (k.clone(), v)).collect();
        substitute_type_params(ty, &refs)
    }

    fn scope_begin(&mut self, defers: &[IREexpr]) {
        self.scope_stack.push(ScopeFrame {
            bindings: Vec::new(),
            defers: defers.to_vec(),
            join_handles: false,
        });
    }

    fn scope_register_binding(&mut self, name: &str, ty: &Type) {
        if let Some(frame) = self.scope_stack.last_mut() {
            frame.bindings.push(ScopeBinding {
                name: name.to_string(),
                ty: ty.clone(),
                dropped: false,
                read: false,
            });
        }
    }

    fn scope_register_param_binding(&mut self, name: &str, ty: &Type) {
        self.scope_register_binding(name, ty);
        if Self::should_silence_unused_binding(name, ty) {
            self.emit_silence_unused_binding(name);
        }
    }

    fn scope_mark_binding_read(&mut self, name: &str) {
        for frame in self.scope_stack.iter_mut().rev() {
            if let Some(binding) = frame.bindings.iter_mut().rev().find(|b| b.name == name) {
                binding.read = true;
                return;
            }
        }
    }

    fn scope_mark_moved(&mut self, name: &str) {
        for frame in self.scope_stack.iter_mut().rev() {
            if let Some(binding) = frame.bindings.iter_mut().rev().find(|b| b.name == name) {
                binding.dropped = true;
                return;
            }
        }
    }

    fn lookup_binding_type(&self, name: &str) -> Option<Type> {
        for frame in self.scope_stack.iter().rev() {
            if let Some(binding) = frame.bindings.iter().rev().find(|b| b.name == name) {
                return Some(binding.ty.clone());
            }
        }
        None
    }

    fn lookup_var_type(&self, name: &str) -> Option<Type> {
        self.lookup_binding_type(name)
            .or_else(|| self.current_function_params.get(name).cloned())
    }

    fn index_element_type(ty: &Type) -> Option<Type> {
        match ty {
            Type::Array { inner, .. } | Type::Slice { inner } => Some((**inner).clone()),
            Type::Vec { elem_type } => Some((**elem_type).clone()),
            Type::String => Some(Type::U8),
            Type::Ref { inner, .. } => Self::index_element_type(inner),
            _ => None,
        }
    }

    /// Type stored on the IR node by lowering from `TypeInfo`. Does not walk struct fields or guess callees.
    fn stored_expr_type(&self, expr: &IREexpr) -> Option<Type> {
        match expr {
            IREexpr::Var(name) => self.lookup_var_type(name),
            IREexpr::Index { target_type, .. } => target_type.as_ref().and_then(|ty| {
                let resolved = resolve_type_alias(ty, &self.type_aliases);
                Self::index_element_type(&resolved)
            }),
            IREexpr::FnLiteral(_) => None,
            other => crate::ir::ir_expr_stored_type(other),
        }
    }

    /// The struct or tuple member is stored as a reference, so the C field is already that pointer.
    fn field_member_is_reference(&self, expr: &IREexpr) -> bool {
        let IREexpr::FieldAccess { base, field, .. } = expr else {
            return false;
        };
        let Some(base_ty) = self.stored_expr_type(base) else {
            return false;
        };
        let base_ty = match resolve_type_alias(&base_ty, &self.type_aliases) {
            Type::Ref { inner, .. } => resolve_type_alias(inner.as_ref(), &self.type_aliases),
            other => other,
        };
        let field_ty = match &base_ty {
            Type::Struct(name) | Type::Generic { name, .. } => {
                let Some(decl) = self.struct_map.get(name) else {
                    return false;
                };
                let params = match &base_ty {
                    Type::Generic { params, .. } => params.clone(),
                    _ => Vec::new(),
                };
                let substitutions = decl
                    .generics
                    .iter()
                    .zip(params.iter())
                    .map(|(param, ty)| (param.name.clone(), ty))
                    .collect::<HashMap<_, _>>();
                let Some(field_decl) = decl.fields.iter().find(|f| f.name == *field) else {
                    return false;
                };
                substitute_type_params(&field_decl.ty, &substitutions)
            }
            Type::Tuple { elements } => {
                let index = field
                    .strip_prefix('f')
                    .unwrap_or(field)
                    .parse::<usize>()
                    .ok();
                let Some(index) = index else {
                    return false;
                };
                let Some(elem) = elements.get(index) else {
                    return false;
                };
                elem.clone()
            }
            _ => return false,
        };
        matches!(
            resolve_type_alias(&field_ty, &self.type_aliases),
            Type::Ref { .. }
        )
    }

    fn binding_is_ref_param(&self, name: &str, pointee: fn(&Type) -> bool) -> bool {
        self.lookup_var_type(name).as_ref().is_some_and(pointee)
    }

    /// True when matching through `&Enum` / `&GenericEnum` (including parser
    /// `Struct("Name")` for enum names that have not been rewritten to `Enum`).
    fn is_ref_to_enum_scrutinee(
        scrutinee_type: Option<&Type>,
        is_enum_name: impl Fn(&str) -> bool,
    ) -> bool {
        match scrutinee_type {
            Some(Type::Ref { inner, .. }) => match inner.as_ref() {
                Type::Enum(_) | Type::Generic { .. } => true,
                Type::Struct(name) => is_enum_name(name),
                _ => false,
            },
            _ => false,
        }
    }

    /// Whether match codegen must deref-copy the scrutinee (`T x = *p`).
    /// Prefer the live binding type for `Var`s: `Vec::get_ref` copy payloads are
    /// bound as owned `T` in C even when the Ion match still sees `&T`.
    fn match_scrutinee_needs_deref(&self, expr: &IREexpr, scrutinee_type: Option<&Type>) -> bool {
        let is_enum = |name: &str| self.enum_map.contains_key(name);
        if let IREexpr::Var(name) = expr
            && let Some(ty) = self.lookup_var_type(name)
        {
            return Self::is_ref_to_enum_scrutinee(Some(&ty), is_enum);
        }
        Self::is_ref_to_enum_scrutinee(scrutinee_type, is_enum)
    }

    /// C expression for `ion_vec_t*` from a `&Vec<T>`, `&mut Vec<T>`, or owned `Vec<T>` IR arg.
    pub(crate) fn vec_ion_ptr_expr(&self, arg: &IREexpr, vec_code: &str) -> String {
        let stripped = vec_code.strip_prefix('&').unwrap_or(vec_code);
        let ref_param = match arg {
            IREexpr::Var(name) => self.binding_is_ref_param(name, is_ref_to_vec),
            IREexpr::AddressOf { inner, .. } => match inner.as_ref() {
                IREexpr::Var(name) => self.binding_is_ref_param(name, is_ref_to_vec),
                _ => false,
            },
            _ => false,
        };
        if ref_param {
            format!("(*{stripped})")
        } else {
            stripped.to_string()
        }
    }

    /// C expression for `ion_string_t*` from a `&String`, `&mut String`, or owned `String` IR arg.
    pub(crate) fn string_ion_ptr_expr(&self, arg: &IREexpr, str_code: &str) -> String {
        let stripped = str_code.strip_prefix('&').unwrap_or(str_code);
        let is_ref_string: fn(&Type) -> bool =
            |ty: &Type| matches!(ty, Type::Ref { inner, .. } if matches!(**inner, Type::String));
        let ref_param = match arg {
            IREexpr::Var(name) => self.binding_is_ref_param(name, is_ref_string),
            IREexpr::AddressOf { inner, .. } => match inner.as_ref() {
                IREexpr::Var(name) => self.binding_is_ref_param(name, is_ref_string),
                _ => false,
            },
            _ => false,
        };
        if ref_param {
            format!("(*{stripped})")
        } else {
            stripped.to_string()
        }
    }

    pub(crate) fn vec_elem_type_from_arg(&self, arg: &IREexpr) -> Option<Type> {
        let ty = self.stored_expr_type(arg)?;
        ref_to_vec_elem(&ty).cloned()
    }

    pub(crate) fn slice_elem_type_from_arg(&self, arg: &IREexpr) -> Option<Type> {
        let ty = self.stored_expr_type(arg)?;
        let resolved = resolve_type_alias(&ty, &self.type_aliases);
        crate::types_util::slice_elem_type(&resolved)
    }

    /// If `arg` is `&arr` for a fixed array, return (name, len, elem type).
    pub(crate) fn slice_arg_as_array(&self, arg: &IREexpr) -> Option<(String, usize, Type)> {
        let IREexpr::AddressOf { inner, .. } = arg else {
            return None;
        };
        let IREexpr::Var(array_name) = inner.as_ref() else {
            return None;
        };
        let var_ty = self.lookup_var_type(array_name)?;
        let var_ty = resolve_type_alias(&var_ty, &self.type_aliases);
        let Type::Array {
            inner: array_elem,
            size,
            ..
        } = var_ty
        else {
            return None;
        };
        Some((array_name.clone(), size, (*array_elem).clone()))
    }

    /// From a generated C expression for a `&[]T` / `[]T` arg, return (base expr, uses_arrow).
    pub(crate) fn slice_ion_access_from_code(
        &self,
        arg: &IREexpr,
        slice_code: &str,
    ) -> (String, bool) {
        let stripped = slice_code.strip_prefix('&').unwrap_or(slice_code);
        match arg {
            IREexpr::Var(name) => {
                let is_ref_slice = self.lookup_var_type(name).is_some_and(|ty| {
                    matches!(ty, Type::Ref { inner, .. } if matches!(*inner, Type::Slice { .. }))
                });
                (stripped.to_string(), is_ref_slice)
            }
            IREexpr::AddressOf { inner, .. } => match inner.as_ref() {
                IREexpr::Var(name) => {
                    let is_ref_slice = self.lookup_var_type(name).is_some_and(|ty| {
                        matches!(
                            ty,
                            Type::Ref { inner, .. } if matches!(*inner, Type::Slice { .. })
                        )
                    });
                    if is_ref_slice {
                        // `&s` where `s: &[]T` yields pointer-to-pointer; deref once to the slice value.
                        (format!("(*{stripped})"), false)
                    } else {
                        // `&owned_slice` where owned is `[]T`: use the slice value.
                        (stripped.to_string(), false)
                    }
                }
                _ => (stripped.to_string(), true),
            },
            _ => (stripped.to_string(), true),
        }
    }

    pub(crate) fn resolve_vec_elem_c_type(
        &self,
        vec_arg: &IREexpr,
        return_type: Option<&Type>,
    ) -> String {
        if let Some(elem) = self.vec_elem_type_from_arg(vec_arg) {
            return self.type_to_c(&elem);
        }
        if let Some(Type::Generic { name, params }) = return_type
            && name == "Option"
            && params.len() == 1
        {
            return self.type_to_c(&params[0]);
        }
        self.generic_instantiations
            .iter()
            .find_map(|(_mono_name, (base, params))| {
                if base == "Vec" && params.len() == 1 {
                    Some(self.type_to_c(&params[0]))
                } else {
                    None
                }
            })
            .unwrap_or_else(|| "int".to_string())
    }

    fn field_access_c_path(&self, expr: &IREexpr) -> Option<String> {
        match expr {
            IREexpr::Var(name) => Some(name.clone()),
            IREexpr::FieldAccess {
                base,
                field,
                is_pointer,
                ..
            } => {
                let base_path = self.field_access_c_path(base)?;
                Some(if *is_pointer {
                    format!("{base_path}->{field}")
                } else {
                    format!("{base_path}.{field}")
                })
            }
            _ => None,
        }
    }

    /// Clear a struct field after its owned value was moved out (e.g. `let v = s.items`).
    fn emit_struct_field_moved_out(&mut self, expr: &IREexpr) {
        let IREexpr::FieldAccess { .. } = expr else {
            return;
        };
        let Some(path) = self.field_access_c_path(expr) else {
            return;
        };
        let Some(ty) = self.stored_expr_type(expr) else {
            return;
        };
        if !self.needs_drop(&ty) {
            return;
        }
        if !self.pending_field_null_set.insert(path.clone()) {
            return;
        }
        self.pending_field_nulls.push(format!(
            "{} = {};",
            path,
            self.zero_value_for_scrutinee_payload(&ty)
        ));
    }

    fn flush_pending_field_nulls(&mut self) {
        let lines: Vec<String> = self.pending_field_nulls.drain(..).collect();
        self.pending_field_null_set.clear();
        for line in lines {
            self.write_indent();
            self.writeln(&line);
        }
    }

    fn is_string_compare_operand(&self, expr: &IREexpr) -> bool {
        match self.stored_expr_type(expr) {
            Some(Type::String) => true,
            Some(Type::Ref { inner, .. }) => matches!(*inner, Type::String),
            _ => false,
        }
    }

    fn emit_string_compare_operand(&mut self, expr: &IREexpr) {
        if let IREexpr::StringLit(value) = expr {
            self.write_ion_string_from_literal(value);
            return;
        }
        let needs_deref = match self.stored_expr_type(expr) {
            Some(Type::Ref { inner, .. }) => matches!(*inner, Type::String),
            _ => false,
        };
        if needs_deref {
            self.write("(*");
            self.generate_expr(expr);
            self.write(")");
        } else {
            self.generate_expr(expr);
        }
    }

    fn emit_binop_operand(&mut self, expr: &IREexpr) {
        let needs_deref = self
            .stored_expr_type(expr)
            .is_some_and(|ty| matches!(ty, Type::Ref { inner, .. } if matches!(*inner, Type::Int | Type::Bool | Type::F32 | Type::F64 | Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::UInt)));
        if needs_deref {
            self.write("(*");
            self.generate_expr(expr);
            self.write(")");
        } else {
            self.generate_expr(expr);
        }
    }

    fn peel_eq_refs(ty: &Type) -> Type {
        match ty {
            Type::Ref { inner, .. } => Self::peel_eq_refs(inner),
            other => other.clone(),
        }
    }

    fn eq_ref_depth(ty: &Type) -> usize {
        match ty {
            Type::Ref { inner, .. } => 1 + Self::eq_ref_depth(inner),
            _ => 0,
        }
    }

    fn capture_structural_operand(&mut self, expr: &IREexpr) -> String {
        let depth = self
            .stored_expr_type(expr)
            .map(|ty| Self::eq_ref_depth(&ty))
            .unwrap_or(0);
        let raw = String::new();
        let old = std::mem::replace(&mut self.output, raw);
        self.generate_expr(expr);
        let raw = std::mem::replace(&mut self.output, old);
        if depth == 0 {
            raw
        } else {
            format!("({}({raw}))", "*".repeat(depth))
        }
    }

    fn capture_binop_operand(&mut self, expr: &IREexpr) -> String {
        let code = String::new();
        let old = std::mem::replace(&mut self.output, code);
        self.emit_binop_operand(expr);
        std::mem::replace(&mut self.output, old)
    }

    fn generate_defined_binop(
        &mut self,
        op: BinOp,
        left: &IREexpr,
        right: &IREexpr,
        result_type: &Type,
        extra_parens: bool,
    ) {
        if matches!(op, BinOp::Eq | BinOp::Ne)
            && self.is_string_compare_operand(left)
            && self.is_string_compare_operand(right)
        {
            self.generate_string_equality(op, left, right, extra_parens);
            return;
        }
        if matches!(op, BinOp::Eq | BinOp::Ne)
            && let (Some(left_ty), Some(right_ty)) =
                (self.stored_expr_type(left), self.stored_expr_type(right))
        {
            let left_owned = resolve_type_alias(&Self::peel_eq_refs(&left_ty), &self.type_aliases);
            let right_owned =
                resolve_type_alias(&Self::peel_eq_refs(&right_ty), &self.type_aliases);
            if crate::tc::types_equal(&left_owned, &right_owned)
                && self.needs_structural_eq(&left_owned)
            {
                self.generate_structural_equality(op, left, right, &left_owned, extra_parens);
                return;
            }
            let resolved = resolve_type_alias(&left_ty, &self.type_aliases);
            if let Type::Tuple { elements } = resolved {
                self.generate_tuple_equality(op, left, right, &elements, extra_parens);
                return;
            }
        }
        let resolved = resolve_type_alias(result_type, &self.type_aliases);
        let arith = matches!(
            op,
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem
        );
        let shift = matches!(op, BinOp::ShiftLeft | BinOp::ShiftRight);
        let bitwise = matches!(op, BinOp::BitAnd | BinOp::BitOr | BinOp::BitXor);
        if let Some((signed_ty, unsigned_ty, bits, is_signed)) = int_c_repr(&resolved)
            && (arith || shift || bitwise)
        {
            let left_code = self.capture_binop_operand(left);
            let right_code = self.capture_binop_operand(right);
            let n = self.temp_var_counter;
            self.temp_var_counter += 1;
            let l = format!("_ion_al{n}");
            let r = format!("_ion_ar{n}");
            self.write("({ ");
            self.write(&format!("{signed_ty} {l} = ({signed_ty})({left_code}); "));
            self.write(&format!("{signed_ty} {r} = ({signed_ty})({right_code}); "));
            match op {
                BinOp::Add
                | BinOp::Sub
                | BinOp::Mul
                | BinOp::BitAnd
                | BinOp::BitOr
                | BinOp::BitXor => {
                    let cop = self.op_to_c(op);
                    self.write(&format!(
                        "({signed_ty})(({unsigned_ty}){l} {cop} ({unsigned_ty}){r}); }})"
                    ));
                }
                BinOp::Div | BinOp::Rem => {
                    let cop = self.op_to_c(op);
                    self.write(&format!("if ({r} == 0) ion_panic(\"division by zero\"); "));
                    if is_signed {
                        let min = crate::cgen::types::c_int_limit(&resolved, false);
                        self.write(&format!(
                                "if ({l} == ({signed_ty})({min}) && {r} == ({signed_ty})(-1)) ion_panic(\"signed division overflow\"); "
                            ));
                    }
                    self.write(&format!("{l} {cop} {r}; }})"));
                }
                BinOp::ShiftLeft | BinOp::ShiftRight => {
                    let width = match bits {
                        Some(b) => b.to_string(),
                        None => "(unsigned)(sizeof(int) * 8)".to_string(),
                    };
                    self.write(&format!(
                            "if ((unsigned){r} >= (unsigned)({width})) ion_panic(\"shift amount out of range\"); "
                        ));
                    if op == BinOp::ShiftRight && is_signed {
                        self.write(&format!(
                                "({signed_ty})(({l} < 0) ? ~(~({unsigned_ty}){l} >> (unsigned){r}) : (({unsigned_ty}){l} >> (unsigned){r})); }})"
                            ));
                    } else {
                        let cop = self.op_to_c(op);
                        self.write(&format!(
                            "({signed_ty})(({unsigned_ty}){l} {cop} (unsigned){r}); }})"
                        ));
                    }
                }
                _ => unreachable!(),
            }
            return;
        }
        if extra_parens {
            self.write("(");
        }
        self.emit_binop_operand(left);
        self.write(&format!(" {} ", self.op_to_c(op)));
        self.emit_binop_operand(right);
        if extra_parens {
            self.write(")");
        }
    }

    fn generate_defined_unop(&mut self, op: UnOp, operand: &IREexpr, result_type: &Type) {
        match op {
            UnOp::Not => {
                self.write("(!");
                self.generate_expr(operand);
                self.write(")");
            }
            UnOp::Neg => {
                let resolved = resolve_type_alias(result_type, &self.type_aliases);
                if let Some((signed_ty, unsigned_ty, _, _)) = int_c_repr(&resolved) {
                    let operand_code = self.capture_binop_operand(operand);
                    self.write(&format!(
                        "(({signed_ty})(0 - ({unsigned_ty})({operand_code})))"
                    ));
                } else if let IREexpr::Lit(v) = operand {
                    if *v == 2147483647i64 + 1 {
                        self.write(&crate::cgen::types::c_int_limit(&Type::Int, false));
                    } else {
                        self.write("(-");
                        self.generate_expr(operand);
                        self.write(")");
                    }
                } else {
                    self.write("(-");
                    self.generate_expr(operand);
                    self.write(")");
                }
            }
        }
    }

    fn generate_string_equality(
        &mut self,
        op: BinOp,
        left: &IREexpr,
        right: &IREexpr,
        wrap_parens: bool,
    ) {
        if wrap_parens {
            self.write("(");
        }
        if matches!(op, BinOp::Ne) {
            self.write("!");
        }
        self.write("ion_string_equals(");
        self.emit_string_compare_operand(left);
        self.write(", ");
        self.emit_string_compare_operand(right);
        self.write(")");
        if wrap_parens {
            self.write(")");
        }
    }

    fn generate_tuple_equality(
        &mut self,
        op: BinOp,
        left: &IREexpr,
        right: &IREexpr,
        elements: &[Type],
        extra_parens: bool,
    ) {
        let _ = extra_parens;
        let left_code = self.capture_binop_operand(left);
        let right_code = self.capture_binop_operand(right);
        let n = self.temp_var_counter;
        self.temp_var_counter += 1;
        let lt = format!("_ion_tl{n}");
        let rt = format!("_ion_tr{n}");
        let c_ty = self.type_to_c(&Type::Tuple {
            elements: elements.to_vec(),
        });
        self.write("({ ");
        self.write(&format!(
            "{c_ty} {lt} = {left_code}; {c_ty} {rt} = {right_code}; "
        ));
        let eq_expr = self.tuple_eq_c_expr(&lt, &rt, elements);
        if matches!(op, BinOp::Ne) {
            self.write(&format!("!({eq_expr}); }})"));
        } else {
            self.write(&format!("{eq_expr}; }})"));
        }
    }

    fn tuple_eq_c_expr(&self, left: &str, right: &str, elements: &[Type]) -> String {
        let parts: Vec<String> = elements
            .iter()
            .enumerate()
            .map(|(i, ty)| {
                let l = format!("{left}.f{i}");
                let r = format!("{right}.f{i}");
                self.value_eq_c_expr(&l, &r, ty)
            })
            .collect();
        if parts.is_empty() {
            "1".to_string()
        } else {
            format!("({})", parts.join(" && "))
        }
    }

    fn needs_structural_eq(&self, ty: &Type) -> bool {
        let resolved = resolve_type_alias(ty, &self.type_aliases);
        matches!(resolved, Type::Array { .. })
            || self.struct_decl_for_type(&resolved).is_some()
            || self.enum_decl_for_type(&resolved).is_some()
    }

    fn generate_structural_equality(
        &mut self,
        op: BinOp,
        left: &IREexpr,
        right: &IREexpr,
        ty: &Type,
        extra_parens: bool,
    ) {
        let _ = extra_parens;
        let left_code = self.capture_structural_operand(left);
        let right_code = self.capture_structural_operand(right);
        self.write("({ ");
        let cmp = if matches!(ty, Type::Array { .. }) {
            self.value_eq_c_expr(&left_code, &right_code, ty)
        } else {
            let n = self.temp_var_counter;
            self.temp_var_counter += 1;
            let lt = format!("_ion_el{n}");
            let rt = format!("_ion_er{n}");
            let c_ty = self.type_to_c(ty);
            self.write(&format!(
                "{c_ty} {lt} = {left_code}; {c_ty} {rt} = {right_code}; "
            ));
            self.value_eq_c_expr(&lt, &rt, ty)
        };
        if matches!(op, BinOp::Ne) {
            self.write(&format!("!({cmp}); }})"));
        } else {
            self.write(&format!("{cmp}; }})"));
        }
    }

    fn value_eq_c_expr(&self, left: &str, right: &str, ty: &Type) -> String {
        let resolved = resolve_type_alias(ty, &self.type_aliases);
        match resolved {
            Type::String | Type::Str => format!("ion_string_equals({left}, {right})"),
            Type::Tuple { elements } => self.tuple_eq_c_expr(left, right, &elements),
            Type::Array { inner, size, .. } => self.array_eq_c_expr(left, right, &inner, size),
            other if self.struct_decl_for_type(&other).is_some() => {
                self.struct_eq_c_expr(left, right, &other)
            }
            other if self.enum_decl_for_type(&other).is_some() => {
                self.enum_eq_c_expr(left, right, &other)
            }
            _ => format!("(({left}) == ({right}))"),
        }
    }

    fn array_eq_c_expr(&self, left: &str, right: &str, elem: &Type, size: usize) -> String {
        if size == 0 {
            return "1".to_string();
        }
        let parts: Vec<String> = (0..size)
            .map(|i| {
                self.value_eq_c_expr(&format!("({left})[{i}]"), &format!("({right})[{i}]"), elem)
            })
            .collect();
        format!("({})", parts.join(" && "))
    }

    fn struct_eq_c_expr(&self, left: &str, right: &str, ty: &Type) -> String {
        let Some((decl, subs)) = self.struct_decl_for_type(ty) else {
            return format!("(({left}) == ({right}))");
        };
        let fields: Vec<(String, Type)> = decl
            .fields
            .iter()
            .map(|field| {
                (
                    field.name.clone(),
                    Self::substitute_field_types(&field.ty, &subs),
                )
            })
            .collect();
        if fields.is_empty() {
            return "1".to_string();
        }
        let parts: Vec<String> = fields
            .iter()
            .map(|(name, field_ty)| {
                self.value_eq_c_expr(
                    &format!("{left}.{name}"),
                    &format!("{right}.{name}"),
                    field_ty,
                )
            })
            .collect();
        format!("({})", parts.join(" && "))
    }

    fn enum_eq_c_expr(&self, left: &str, right: &str, ty: &Type) -> String {
        let Some((decl, subs)) = self.enum_decl_for_type(ty) else {
            return format!("(({left}) == ({right}))");
        };
        let arms: Vec<(usize, Vec<(String, Type)>)> = decl
            .variants
            .iter()
            .enumerate()
            .map(|(index, variant)| {
                let fields = if let Some(named) = &variant.named_fields {
                    named
                        .iter()
                        .map(|(name, field_ty)| {
                            (name.clone(), Self::substitute_field_types(field_ty, &subs))
                        })
                        .collect()
                } else {
                    variant
                        .payload_types
                        .iter()
                        .enumerate()
                        .map(|(arg_index, field_ty)| {
                            (
                                format!("arg{arg_index}"),
                                Self::substitute_field_types(field_ty, &subs),
                            )
                        })
                        .collect()
                };
                (index, fields)
            })
            .collect();
        let checks: Vec<String> = arms
            .iter()
            .filter(|(_, fields)| !fields.is_empty())
            .map(|(index, fields)| {
                let body = fields
                    .iter()
                    .map(|(name, field_ty)| {
                        self.value_eq_c_expr(
                            &format!("{left}.data.variant_{index}.{name}"),
                            &format!("{right}.data.variant_{index}.{name}"),
                            field_ty,
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(" && ");
                format!("({left}.tag != {index} || ({body}))")
            })
            .collect();
        let payload = if checks.is_empty() {
            "1".to_string()
        } else {
            checks.join(" && ")
        };
        format!("(({left}.tag == {right}.tag) && ({payload}))")
    }

    fn slice_struct_name_for_elem(&self, elem_ty: &Type) -> String {
        format!(
            "ion_slice_{}",
            mangle_type_name(&self.type_to_c(elem_ty), &[])
        )
    }

    /// When init is `&arr` for `[T; N]` and expected is `&[]T`, return (array name, N, elem type).
    fn match_array_to_slice_coercion(
        &self,
        expected: &Type,
        init: &IREexpr,
    ) -> Option<(String, usize, Type)> {
        let expected = resolve_type_alias(expected, &self.type_aliases);
        let Type::Ref {
            inner: expected_inner,
            mutable: expected_mut,
        } = expected
        else {
            return None;
        };
        let Type::Slice { .. } = expected_inner.as_ref() else {
            return None;
        };
        let IREexpr::AddressOf {
            inner,
            mutable: init_mut,
            ..
        } = init
        else {
            return None;
        };
        if expected_mut != *init_mut {
            return None;
        }
        let IREexpr::Var(array_name) = inner.as_ref() else {
            return None;
        };
        let var_ty = self.lookup_var_type(array_name)?;
        let var_ty = resolve_type_alias(&var_ty, &self.type_aliases);
        let Type::Array {
            inner: array_elem,
            size,
            ..
        } = var_ty
        else {
            return None;
        };
        Some((array_name.clone(), size, (*array_elem).clone()))
    }

    fn emit_array_to_slice_ptr(&mut self, array_name: &str, size: usize, elem_ty: &Type) {
        let slice_type = self.slice_struct_name_for_elem(elem_ty);
        self.write("&(");
        self.write(&slice_type);
        self.write("){");
        self.write(array_name);
        self.write(", ");
        self.write(&size.to_string());
        self.write("}");
    }

    /// Mirror tc `check_expr_for_moves`: mark bindings consumed by this expression.
    fn mark_moves_in_expr(&mut self, expr: &IREexpr) {
        match expr {
            IREexpr::Var(name) => {
                if let Some(ty) = self.lookup_binding_type(name)
                    && self.needs_drop(&ty)
                {
                    self.scope_mark_moved(name);
                }
            }
            IREexpr::FieldAccess { .. } => {
                self.emit_struct_field_moved_out(expr);
            }
            IREexpr::StructLit { fields, .. } => {
                for field in fields {
                    self.mark_moves_in_expr(&field.value);
                }
            }
            IREexpr::TupleLit { elements, .. } => {
                for elem in elements {
                    self.mark_moves_in_expr(elem);
                }
            }
            IREexpr::EnumLit {
                args, named_fields, ..
            } => {
                for arg in args {
                    self.mark_moves_in_expr(arg);
                }
                if let Some(named_fields) = named_fields {
                    for (_, value) in named_fields {
                        self.mark_moves_in_expr(value);
                    }
                }
            }
            IREexpr::Send { channel, value, .. } => {
                if matches!(self.stored_expr_type(channel), Some(Type::Endpoint { .. })) {
                    self.mark_moves_in_expr(channel);
                }
                self.mark_moves_in_expr(value);
            }
            IREexpr::Call { args, .. } => {
                for arg in args {
                    self.mark_moves_in_expr(arg);
                }
            }
            IREexpr::ArrayLiteral {
                elements, repeat, ..
            } => {
                for elem in elements {
                    self.mark_moves_in_expr(elem);
                }
                if let Some((value, _)) = repeat {
                    self.mark_moves_in_expr(value);
                }
            }
            IREexpr::Cast { expr, .. } => self.mark_moves_in_expr(expr),
            IREexpr::Assign { value, .. } => self.mark_moves_in_expr(value),
            IREexpr::AssignIndex { value, .. } => self.mark_moves_in_expr(value),
            IREexpr::AssignField { value, .. } => self.mark_moves_in_expr(value),
            IREexpr::Match { expr, .. } => self.mark_moves_in_expr(expr),
            IREexpr::Recv { channel, .. } => {
                if matches!(self.stored_expr_type(channel), Some(Type::Endpoint { .. })) {
                    self.mark_moves_in_expr(channel);
                }
            }
            _ => {}
        }
    }

    fn scope_emit_exit(&mut self) {
        let Some(frame) = self.scope_stack.pop() else {
            return;
        };
        self.emit_frame_cleanup(&frame);
    }

    /// Emit cleanup for all active scopes (innermost first) without popping the stack.
    /// Used on `return` so sibling statements in the same function still codegen correctly.
    fn scope_emit_return_unwind(&mut self) {
        let frames: Vec<ScopeFrame> = self.scope_stack.iter().rev().cloned().collect();
        for frame in frames {
            self.emit_frame_cleanup(&frame);
        }
    }

    /// Emit cleanup for frames from the loop body through the innermost nested block.
    /// Clones only; does not pop or mark `dropped` (fall-through after `if` still uses those bindings).
    fn scope_emit_loop_unwind(&mut self) {
        let depth = self.loop_unwind_depth;
        if depth >= self.scope_stack.len() {
            return;
        }
        let frames: Vec<ScopeFrame> = self.scope_stack[depth..].iter().rev().cloned().collect();
        for frame in frames {
            self.emit_frame_cleanup(&frame);
        }
    }

    /// Specified interleaving: this block's defers (LIFO), then remaining locals (LIFO).
    fn emit_frame_cleanup(&mut self, frame: &ScopeFrame) {
        for defer_expr in frame.defers.iter().rev() {
            self.write_indent();
            self.write("(void)(");
            self.generate_expr(defer_expr);
            self.writeln(");");
        }
        for binding in frame.bindings.iter().rev() {
            if !binding.dropped
                && !binding.read
                && !Self::should_silence_unused_binding(&binding.name, &binding.ty)
            {
                self.emit_silence_unused_binding(&binding.name);
            }
        }
        for binding in frame.bindings.iter().rev() {
            if !binding.dropped && self.needs_drop(&binding.ty) {
                if frame.join_handles && matches!(binding.ty, Type::JoinHandle { .. }) {
                    self.emit_scope_join(&binding.name, &binding.ty);
                } else {
                    self.emit_drop(&binding.name, &binding.ty);
                }
            }
        }
    }

    fn emit_scope_join(&mut self, name: &str, ty: &Type) {
        let Type::JoinHandle { result } = ty else {
            return;
        };
        if matches!(result.as_ref(), Type::Void) {
            self.write_indent();
            self.writeln(&format!(
                "if (ion_join(&({name})) != 0) ion_panic(\"join failed\");"
            ));
            return;
        }
        let c_ty = self.type_to_c(result);
        let tmp = format!("_ion_scope_ret_{}", self.temp_var_counter);
        self.temp_var_counter += 1;
        self.write_indent();
        self.writeln("{");
        self.indent_level += 1;
        self.write_indent();
        self.writeln(&format!(
            "{c_ty}* _ion_slot = 0; if (ion_join_value(&({name}), (void**)&_ion_slot) != 0 || !_ion_slot) ion_panic(\"join failed\");"
        ));
        self.write_indent();
        self.writeln(&format!("{c_ty} {tmp} = *_ion_slot;"));
        self.write_indent();
        self.writeln("free(_ion_slot);");
        if self.needs_drop(result) {
            self.emit_drop(&tmp, result);
        }
        self.indent_level -= 1;
        self.write_indent();
        self.writeln("}");
    }

    /// Drop owned bindings in the current scope frame without popping it.
    /// Used when leaving a grouped `switch` arm early (guard match) before case-level cleanup.
    fn scope_emit_top_frame_drops(&mut self) {
        let Some(frame) = self.scope_stack.last().cloned() else {
            return;
        };
        self.emit_frame_cleanup(&frame);
        let to_drop: Vec<String> = frame
            .bindings
            .iter()
            .filter(|b| !b.dropped && self.needs_drop(&b.ty))
            .map(|b| b.name.clone())
            .collect();
        let Some(top) = self.scope_stack.last_mut() else {
            return;
        };
        for name in to_drop {
            if let Some(binding) = top.bindings.iter_mut().find(|b| b.name == name) {
                binding.dropped = true;
            }
        }
    }

    /// Assign `ret_val` (when needed), run scope unwind, and jump to the function epilogue.
    fn emit_function_return(&mut self, ret: &crate::ir::IRReturn) {
        self.write_indent();
        if let Some(ref value) = ret.value {
            let is_array_return = if let Some(ref ret_ty) = self.current_return_type {
                matches!(ret_ty, Type::Array { .. })
            } else {
                false
            };

            if is_array_return {
                if let Some(ret_ty) = self.current_return_type.clone()
                    && let Type::Array { inner, size, .. } = &ret_ty
                {
                    let base_type = self.type_to_c(inner);
                    self.writeln(&format!("static {} _ret_array[{}] = ", base_type, size));
                    self.write_indent();
                    self.write("    ");
                    self.generate_expr_with_type(value, Some(&ret_ty));
                    self.writeln(";");
                    self.write_indent();
                    self.writeln("ret_val = _ret_array;");
                }
            } else {
                let return_ty = self.current_return_type.clone();
                if let IREexpr::Match {
                    expr: match_expr,
                    enum_type,
                    arms,
                    scrutinee_type,
                    ..
                } = value
                {
                    if let Some(ref ty) = return_ty {
                        self.generate_match_block(
                            match_expr,
                            enum_type,
                            arms,
                            Some(("ret_val", ty)),
                            scrutinee_type.as_ref(),
                        );
                    } else {
                        self.generate_match_block(
                            match_expr,
                            enum_type,
                            arms,
                            None,
                            scrutinee_type.as_ref(),
                        );
                    }
                } else {
                    self.write("ret_val = ");

                    let mut needs_memcpy = false;
                    let mut array_field_name = String::new();
                    let mut array_var_name = String::new();

                    if let IREexpr::StructLit { type_name, fields } = value
                        && let Some(struct_decl) = self.struct_map.get(type_name)
                    {
                        for field in fields.iter() {
                            if let IREexpr::Var(ref var_name) = field.value
                                && let Some(field_decl) =
                                    struct_decl.fields.iter().find(|f| f.name == field.name)
                                && let Type::Array { .. } = field_decl.ty
                            {
                                needs_memcpy = true;
                                array_field_name = field.name.clone();
                                array_var_name = var_name.clone();
                                break;
                            }
                        }
                    }

                    self.generate_expr_with_type(value, return_ty.as_ref());
                    self.writeln(";");

                    if needs_memcpy {
                        self.write_indent();
                        self.writeln(&format!(
                            "memcpy(&ret_val.{}, &{}, sizeof(ret_val.{}));",
                            array_field_name, array_var_name, array_field_name
                        ));
                    }
                }
            }
        }
        if let Some(value) = &ret.value {
            self.mark_moves_in_expr(value);
        }
        self.flush_pending_field_nulls();
        self.scope_emit_return_unwind();
        self.write_indent();
        self.writeln(&format!("goto {};", self.epilogue_label));
    }

    fn c_struct_field_decl(&self, name: &str, ty: &Type) -> String {
        match ty {
            Type::Array { inner, size, .. } => {
                format!("{} {}[{}]", self.type_to_c(inner), name, size)
            }
            _ => format!("{} {}", self.type_to_c(ty), name),
        }
    }

    fn c_named_decl(&self, name: &str, ty: &Type) -> String {
        match ty {
            Type::Fn { .. } => fn_type_to_c_decl(ty, name),
            other => self.c_struct_field_decl(name, other),
        }
    }

    fn format_ir_param_list_c(&self, params: &[IRParam]) -> String {
        if params.is_empty() {
            return "void".to_string();
        }
        params
            .iter()
            .map(|param| self.c_named_decl(&param.name, &param.ty))
            .collect::<Vec<_>>()
            .join(", ")
    }

    fn generate_function(&mut self, function: &IRFunction) {
        // Store current return type for use in return statements
        self.current_return_type = function.return_type.clone();
        self.scope_stack.clear();
        self.epilogue_label = "epilogue".to_string();
        self.pending_field_nulls.clear();
        self.pending_field_null_set.clear();

        // Store parameter types for field access resolution
        self.current_function_params.clear();
        for param in &function.params {
            self.current_function_params
                .insert(param.name.clone(), param.ty.clone());
        }

        let c_name = self.module_c_symbol(&function.name);
        let param_list = self.format_ir_param_list_c(&function.params);

        if let Some(ret_ty) = function.return_type.as_ref() {
            let resolved = resolve_type_alias(ret_ty, &self.type_aliases);
            if matches!(resolved, Type::Fn { .. }) {
                self.write(&fn_type_to_c_function_header(
                    &c_name,
                    &param_list,
                    &resolved,
                ));
                self.writeln(" {");
            } else {
                let return_type = match ret_ty {
                    Type::Array { inner, .. } => format!("{}*", self.type_to_c(inner)),
                    _ => self.type_to_c(ret_ty),
                };
                self.write(&format!("{} {}(", return_type, c_name));
                self.write(&param_list);
                self.writeln(") {");
            }
        } else {
            self.write(&format!("void {}(", c_name));
            self.write(&param_list);
            self.writeln(") {");
        }

        self.indent_level += 1;

        // Synthetic return variable for functions with a return type.
        if let Some(ref ty) = function.return_type {
            self.write_indent();
            let resolved_ret = resolve_type_alias(ty, &self.type_aliases);
            self.writeln(&format_ret_val_decl(&ret_val_decl(&resolved_ret)));
        }

        self.scope_begin(&[]);
        for param in &function.params {
            self.scope_register_param_binding(&param.name, &param.ty);
        }

        // Generate function body (blocks)
        for block in &function.blocks {
            self.generate_block(block);
        }

        // Check if the last statement in the last block is a return
        // If not, and the function has a return type, add fallthrough goto
        let last_stmt_is_return = function
            .blocks
            .last()
            .and_then(|block| block.statements.last())
            .map(|stmt| matches!(stmt, IRStmt::Return(_)))
            .unwrap_or(false);

        // If control reaches the end without an explicit return, fall through to
        // epilogue (default-initialized ret_val for value-returning functions).
        if !last_stmt_is_return {
            self.scope_emit_return_unwind();
            self.write_indent();
            self.writeln("goto epilogue;");
        }

        // Epilogue: return (scope cleanup already ran on `return` or fallthrough above).
        self.writeln("epilogue:");
        self.indent_level += 1;

        if function.return_type.is_some() {
            self.write_indent();
            self.writeln("return ret_val;");
        } else {
            self.write_indent();
            self.writeln("return;");
        }

        self.indent_level -= 1;

        self.indent_level -= 1;
        self.writeln("}");
        self.writeln("");
    }

    fn generate_fn_literal(&mut self, lit: &crate::ir::IRFnLiteral) {
        if self.generated_fn_literals.contains(&lit.symbol) {
            return;
        }
        self.generated_fn_literals.insert(lit.symbol.clone());
        if let Some(struct_name) = &lit.env_struct {
            self.fn_literal_forward_decls
                .push_str(&format!("typedef struct {struct_name} {{\n"));
            for (name, ty) in &lit.captures {
                self.fn_literal_forward_decls
                    .push_str(&format!("    {} {name};\n", self.type_to_c(ty)));
            }
            self.fn_literal_forward_decls
                .push_str(&format!("}} {struct_name};\n"));
        }

        let return_type_c = lit
            .return_type
            .as_ref()
            .map(|t| match t {
                Type::Array { inner, .. } => format!("{}*", self.type_to_c(inner)),
                _ => self.type_to_c(t),
            })
            .unwrap_or_else(|| "void".to_string());

        let param_list = self.format_ir_param_list_c(&lit.params);
        self.fn_literal_forward_decls.push_str(&format!(
            "static {} {}({});\n",
            return_type_c, lit.symbol, param_list
        ));

        let epilogue = format!("{}_epilogue", lit.symbol);
        let mut def = String::new();
        def.push_str(&format!(
            "static {} {}({}) {{\n",
            return_type_c, lit.symbol, param_list
        ));

        if let Some(ty) = &lit.return_type {
            let resolved = resolve_type_alias(ty, &self.type_aliases);
            def.push_str(&format!(
                "    {}\n",
                format_ret_val_decl(&ret_val_decl(&resolved))
            ));
        }

        let saved_output = std::mem::take(&mut self.output);
        let saved_indent = self.indent_level;
        let saved_scope = std::mem::take(&mut self.scope_stack);
        let saved_epilogue = self.epilogue_label.clone();
        let saved_params = self.current_function_params.clone();
        let saved_return = self.current_return_type.clone();

        self.indent_level = 1;
        self.scope_stack.clear();
        self.epilogue_label = epilogue.clone();
        self.current_return_type = lit.return_type.clone();
        self.current_function_params.clear();
        for param in &lit.params {
            self.current_function_params
                .insert(param.name.clone(), param.ty.clone());
        }

        self.scope_begin(&[]);
        for param in &lit.params {
            self.scope_register_param_binding(&param.name, &param.ty);
        }
        self.generate_block(&lit.body);
        self.scope_emit_exit();

        let last_stmt_is_return = lit
            .body
            .statements
            .last()
            .map(|stmt| matches!(stmt, IRStmt::Return(_)))
            .unwrap_or(false);
        if lit.return_type.is_some() && !last_stmt_is_return {
            self.write_indent();
            self.writeln(&format!("goto {};", epilogue));
        }

        let body_code = std::mem::take(&mut self.output);
        def.push_str(&body_code);
        def.push_str(&format!("{}:\n", epilogue));
        if lit.return_type.is_some() {
            def.push_str("    return ret_val;\n");
        }
        def.push_str("}\n\n");

        self.output = saved_output;
        self.indent_level = saved_indent;
        self.scope_stack = saved_scope;
        self.epilogue_label = saved_epilogue;
        self.current_function_params = saved_params;
        self.current_return_type = saved_return;

        self.fn_literal_definitions.push_str(&def);
    }

    fn spawn_capture_fill_lines(&self, spawn: &IRSpawn, ctx_name: &str) -> Vec<String> {
        let mut lines = vec![
            format!("{ctx_name}* ctx = ({ctx_name}*)malloc(sizeof({ctx_name}));"),
            "if (!ctx) { ion_panic(\"spawn allocation failed\"); }".to_string(),
        ];
        for (name, ty) in &spawn.captures {
            lines.push(format!("ctx->{name} = {name};"));
            lines.push(format!("{name} = {};", self.zero_value_for_type(ty)));
        }
        lines
    }

    fn generate_spawn(&mut self, spawn: &IRSpawn) {
        if self
            .scope_stack
            .last()
            .is_some_and(|frame| frame.join_handles)
        {
            self.generate_scoped_statement_spawn(spawn);
            return;
        }
        let spawn_id = self.spawn_counter;
        self.spawn_counter += 1;
        let ctx_name = format!("ion_spawn_ctx_{}", spawn_id);
        let entry_name = format!("ion_spawn_entry_{}", spawn_id);
        self.emit_spawn_entry(spawn, spawn_id, &ctx_name, &entry_name);

        self.write_indent();
        self.writeln("{");
        self.indent_level += 1;
        self.write_indent();
        if spawn.captures.is_empty() {
            self.writeln(&format!(
                "if (ion_spawn({}, NULL) != 0) {{ ion_panic(\"spawn failed\"); }}",
                entry_name
            ));
        } else {
            let lines = self.spawn_capture_fill_lines(spawn, &ctx_name);
            for (i, line) in lines.iter().enumerate() {
                if i > 0 {
                    self.write_indent();
                }
                self.writeln(line);
            }
            for (name, _) in &spawn.captures {
                self.scope_mark_moved(name);
            }
            self.write_indent();
            self.writeln(&format!(
                "if (ion_spawn({}, ctx) != 0) {{ free(ctx); ion_panic(\"spawn failed\"); }}",
                entry_name
            ));
        }
        self.indent_level -= 1;
        self.write_indent();
        self.writeln("}");
    }

    fn generate_scoped_statement_spawn(&mut self, spawn: &IRSpawn) {
        let spawn_id = self.spawn_counter;
        self.spawn_counter += 1;
        let ctx_name = format!("ion_spawn_ctx_{}", spawn_id);
        let entry_name = format!("ion_spawn_entry_{}", spawn_id);
        let handle = format!("_ion_scope_jh_{spawn_id}");
        self.emit_spawn_entry(spawn, spawn_id, &ctx_name, &entry_name);
        self.write_indent();
        self.writeln(&format!("ion_thread_t {handle};"));
        self.scope_register_binding(
            &handle,
            &Type::JoinHandle {
                result: Box::new(spawn.result.clone()),
            },
        );
        self.write_indent();
        if spawn.captures.is_empty() {
            self.writeln(&format!(
                "if (ion_spawn_joinable({entry_name}, NULL, &{handle}) != 0) {{ ion_panic(\"spawn failed\"); }}"
            ));
        } else {
            for line in self.spawn_capture_fill_lines(spawn, &ctx_name) {
                self.writeln(&line);
                self.write_indent();
            }
            for (name, _) in &spawn.captures {
                self.scope_mark_moved(name);
            }
            self.writeln(&format!(
                "if (ion_spawn_joinable({entry_name}, ctx, &{handle}) != 0) {{ free(ctx); ion_panic(\"spawn failed\"); }}"
            ));
        }
    }

    fn generate_spawn_expr(&mut self, spawn: &IRSpawn) {
        let spawn_id = self.spawn_counter;
        self.spawn_counter += 1;
        let ctx_name = format!("ion_spawn_ctx_{}", spawn_id);
        let entry_name = format!("ion_spawn_entry_{}", spawn_id);
        self.emit_spawn_entry(spawn, spawn_id, &ctx_name, &entry_name);
        self.write("({ ion_thread_t _ion_jh; ");
        if spawn.captures.is_empty() {
            self.write(&format!(
                "if (ion_spawn_joinable({entry_name}, NULL, &_ion_jh) != 0) {{ ion_panic(\"spawn failed\"); }} "
            ));
        } else {
            for line in self.spawn_capture_fill_lines(spawn, &ctx_name) {
                self.write(&line);
                self.write(" ");
            }
            for (name, _) in &spawn.captures {
                self.scope_mark_moved(name);
            }
            self.write(&format!(
                "if (ion_spawn_joinable({entry_name}, ctx, &_ion_jh) != 0) {{ free(ctx); ion_panic(\"spawn failed\"); }} "
            ));
        }
        self.write("_ion_jh; })");
    }

    fn emit_spawn_entry(
        &mut self,
        spawn: &IRSpawn,
        spawn_id: usize,
        ctx_name: &str,
        entry_name: &str,
    ) {
        let spawn_epilogue = format!("spawn_{}_epilogue", spawn_id);
        self.spawn_forward_decls
            .push_str(&format!("static void* {}(void* arg);\n", entry_name));
        if !spawn.captures.is_empty() {
            self.spawn_forward_decls.push_str("typedef struct {\n");
            for (name, ty) in &spawn.captures {
                self.spawn_forward_decls.push_str(&format!(
                    "    {} {};\n",
                    self.type_to_c(ty),
                    name
                ));
            }
            self.spawn_forward_decls
                .push_str(&format!("}} {};\n", ctx_name));
        }
        let mut def = String::new();
        def.push_str(&format!("static void* {}(void* arg) {{\n", entry_name));
        let returns_value = !matches!(spawn.result, Type::Void);
        if returns_value {
            def.push_str("    ");
            def.push_str(&format_ret_val_decl(&ret_val_decl(&spawn.result)));
            def.push('\n');
        }
        if !spawn.captures.is_empty() {
            def.push_str(&format!("    {}* ctx = ({}*)arg;\n", ctx_name, ctx_name));
            def.push_str("    if (!ctx) { ion_panic(\"spawn null context\"); }\n");
            for (name, ty) in &spawn.captures {
                def.push_str(&format!(
                    "    {} {} = ctx->{};\n",
                    self.type_to_c(ty),
                    name,
                    name
                ));
            }
            def.push_str("    free(ctx);\n");
        } else {
            def.push_str("    (void)arg;\n");
        }
        let saved_output = std::mem::take(&mut self.output);
        let saved_indent = self.indent_level;
        let saved_scope = std::mem::take(&mut self.scope_stack);
        let saved_epilogue = self.epilogue_label.clone();
        let saved_return = self.current_return_type.clone();
        self.indent_level = 1;
        self.scope_stack.clear();
        self.epilogue_label = spawn_epilogue.clone();
        if returns_value {
            self.current_return_type = Some(spawn.result.clone());
        }
        self.scope_begin(&[]);
        for (name, ty) in &spawn.captures {
            self.scope_register_param_binding(name, ty);
        }
        self.generate_block(&spawn.body);
        self.scope_emit_exit();
        self.write_indent();
        self.writeln(&format!("goto {};", spawn_epilogue));
        let body_code = std::mem::take(&mut self.output);
        self.output = saved_output;
        self.indent_level = saved_indent;
        self.scope_stack = saved_scope;
        self.epilogue_label = saved_epilogue;
        self.current_return_type = saved_return;
        def.push_str(&body_code);
        def.push_str(&format!("{}:\n", spawn_epilogue));
        if returns_value {
            let c_ty = self.type_to_c(&spawn.result);
            def.push_str(&format!(
                "    {{ {c_ty}* _ion_slot = ({c_ty}*)malloc(sizeof({c_ty})); if (!_ion_slot) ion_panic(\"spawn result allocation failed\"); *_ion_slot = ret_val; return _ion_slot; }}\n"
            ));
        } else {
            def.push_str("    return NULL;\n");
        }
        def.push_str("}\n\n");
        self.spawn_definitions.push_str(&def);
    }

    fn generate_select(&mut self, sel: &IRSelect) {
        let n = sel.recv_arms.len();
        let sid = self.temp_var_counter;
        self.temp_var_counter += 1;
        self.write_indent();
        self.writeln("{");
        self.indent_level += 1;
        let timeout_ms = if sel.default_body.is_some() {
            "0".to_string()
        } else if sel.timeout_ms.is_some() {
            let t = format!("_ion_sel_ms{sid}");
            self.write_indent();
            self.write("int ");
            self.write(&t);
            self.write(" = ");
            if let Some(ms) = &sel.timeout_ms {
                self.generate_expr(ms);
            }
            self.writeln(";");
            self.write_indent();
            self.writeln(&format!(
                "if ({t} < 0) ion_panic(\"select timeout must be >= 0\");"
            ));
            t
        } else {
            "-1".to_string()
        };
        if n > 0 {
            self.write_indent();
            self.writeln(&format!("ion_select_arm_t _ion_sel_arms{sid}[{n}];"));
            for (i, arm) in sel.recv_arms.iter().enumerate() {
                let tmp = format!("_ion_sel_v{sid}_{i}");
                let c_ty = self.type_to_c(&arm.elem_type);
                self.write_indent();
                self.writeln(&format!("{c_ty} {tmp} = {{0}};"));
                self.write_indent();
                self.write(&format!("_ion_sel_arms{sid}[{i}].rx = "));
                self.write("(");
                self.generate_expr(&arm.channel);
                self.writeln(");");
                self.write_indent();
                self.writeln(&format!("_ion_sel_arms{sid}[{i}].out = &{tmp};"));
            }
        }
        self.write_indent();
        self.writeln(&format!("int _ion_sel_st{sid} = 0;"));
        self.write_indent();
        if n == 0 {
            self.writeln(&format!(
                "int _ion_sel_i{sid} = ion_channel_select(NULL, 0, {timeout_ms}, &_ion_sel_st{sid});"
            ));
        } else {
            self.writeln(&format!(
                "int _ion_sel_i{sid} = ion_channel_select(_ion_sel_arms{sid}, {n}, {timeout_ms}, &_ion_sel_st{sid});"
            ));
        }
        for (i, arm) in sel.recv_arms.iter().enumerate() {
            self.write_indent();
            self.writeln(&format!("if (_ion_sel_i{sid} == {i}) {{"));
            self.indent_level += 1;
            if let Some(name) = &arm.binding {
                let option_ty = Type::Generic {
                    name: "Option".to_string(),
                    params: vec![arm.elem_type.clone()],
                };
                let option_c = self.type_to_c(&option_ty);
                let tmp = format!("_ion_sel_v{sid}_{i}");
                let some = self.c_enum_literal(&option_c, "Option", "Some", Some(&tmp));
                let none = self.c_enum_literal(&option_c, "Option", "None", None);
                self.write_indent();
                self.writeln(&format!("{option_c} {name};"));
                self.write_indent();
                self.writeln(&format!(
                    "if (_ion_sel_st{sid} == 0) {{ {name} = {some}; }} else {{ {name} = {none}; }}"
                ));
                self.scope_begin(&[]);
                self.scope_register_param_binding(name, &option_ty);
            }
            self.generate_block(&arm.body);
            if arm.binding.is_some() {
                self.scope_emit_exit();
            }
            self.indent_level -= 1;
            self.write_indent();
            self.writeln("}");
        }
        let after_recv = n;
        if let Some(body) = &sel.default_body {
            self.write_indent();
            self.writeln(&format!("if (_ion_sel_i{sid} == {after_recv}) {{"));
            self.indent_level += 1;
            self.generate_block(body);
            self.indent_level -= 1;
            self.write_indent();
            self.writeln("}");
        } else if let Some(body) = &sel.timeout_body {
            self.write_indent();
            self.writeln(&format!("if (_ion_sel_i{sid} == {after_recv}) {{"));
            self.indent_level += 1;
            self.generate_block(body);
            self.indent_level -= 1;
            self.write_indent();
            self.writeln("}");
        }
        self.indent_level -= 1;
        self.write_indent();
        self.writeln("}");
    }

    fn emit_break_statement(&mut self) {
        self.scope_emit_loop_unwind();
        self.write_indent();
        if let Some(ref label) = self.loop_break_label {
            self.loop_break_label_used = true;
            self.writeln(&format!("goto {};", label));
        } else {
            self.writeln("break;");
        }
    }

    fn emit_continue_statement(&mut self) {
        self.scope_emit_loop_unwind();
        self.write_indent();
        if let Some(ref label) = self.loop_continue_label {
            self.writeln(&format!("goto {};", label));
        } else {
            self.writeln("continue;");
        }
    }

    fn generate_block(&mut self, block: &IRBlock) {
        let depth_at_entry = self.scope_stack.len();
        let join_handles = self.next_block_joins;
        self.next_block_joins = false;
        self.scope_stack.push(ScopeFrame {
            bindings: Vec::new(),
            defers: block.defers.clone(),
            join_handles,
        });
        let mut i = 0;
        while i < block.statements.len() {
            // Check for consecutive channel tuple destructuring
            if let (IRStmt::Let(let1), Some(IRStmt::Let(let2))) =
                (&block.statements[i], block.statements.get(i + 1))
                && let Some(IREexpr::Call {
                    callee: callee1,
                    return_type: return_type1,
                    tuple_destructure_index: idx1,
                    ..
                }) = &let1.init
                && let Some(IREexpr::Call {
                    callee: callee2,
                    return_type: return_type2,
                    tuple_destructure_index: idx2,
                    ..
                }) = &let2.init
                && callee1 == "channel"
                && callee2 == "channel"
                && *idx1 == Some(0)
                && *idx2 == Some(1)
                && return_type1.is_some()
                && return_type2.is_some()
            {
                // Generate both declarations and single channel_new call
                if let Some(Type::Tuple { elements }) = return_type1
                    && elements.len() == 2
                    && let Type::Sender { elem_type } = &elements[0]
                    && let Some(IREexpr::Call { args, .. }) = &let1.init
                {
                    self.write_indent();
                    self.write(&format!("{} {}", self.type_to_c(&let1.ty), let1.name));
                    self.writeln(";");
                    self.write_indent();
                    self.write(&format!("{} {}", self.type_to_c(&let2.ty), let2.name));
                    self.writeln(";");

                    self.emit_ion_channel_new(elem_type, args, &let1.name, &let2.name);

                    self.scope_register_binding(&let1.name, &let1.ty);
                    self.scope_register_binding(&let2.name, &let2.ty);

                    i += 2;
                    continue;
                }
            }

            // Normal statement generation
            self.generate_stmt(&block.statements[i]);
            i += 1;
        }
        let ends_with_jump = matches!(
            block.statements.last(),
            Some(IRStmt::Return(_) | IRStmt::Break | IRStmt::Continue)
        );
        if ends_with_jump {
            while self.scope_stack.len() > depth_at_entry {
                self.scope_stack.pop();
            }
        } else {
            self.scope_emit_exit();
        }
    }

    fn generate_stmt(&mut self, stmt: &IRStmt) {
        match stmt {
            IRStmt::Let(let_stmt) => {
                if let Some(ref init) = let_stmt.init
                    && let Some((array_name, size, elem_ty)) =
                        self.match_array_to_slice_coercion(&let_stmt.ty, init)
                {
                    let slice_type = self.slice_struct_name_for_elem(&elem_ty);
                    let temp = format!("__ion_arr_slice_{}", self.temp_var_counter);
                    self.temp_var_counter += 1;
                    self.write_indent();
                    self.writeln(&format!(
                        "{} {} = {{ {}, {} }};",
                        slice_type, temp, array_name, size
                    ));
                    self.write_indent();
                    self.write(&format!(
                        "{} {} = &{};",
                        self.type_to_c(&let_stmt.ty),
                        let_stmt.name,
                        temp
                    ));
                    self.writeln(";");
                    self.scope_register_binding(&let_stmt.name, &let_stmt.ty);
                    self.mark_moves_in_expr(init);
                    self.flush_pending_field_nulls();
                    return;
                }
                self.write_indent();
                // Special handling: if initialized from function call that returns array,
                // declare as pointer (C doesn't allow returning arrays)
                // Otherwise, if type is array, declare as array
                // Check if this is a function call returning an array (regardless of let_stmt.ty)
                let call_returns_array_type = if let Some(IREexpr::Call {
                    callee,
                    return_type,
                    ..
                }) = let_stmt.init.as_ref()
                {
                    // First check if return_type is set in the Call expression
                    let from_call = return_type.as_ref().and_then(|rt| {
                        if let Type::Array { inner, .. } = rt {
                            Some(self.type_to_c(inner))
                        } else {
                            None
                        }
                    });
                    // If not set, look up the function's return type
                    if from_call.is_none() {
                        self.lookup_return_type(callee).and_then(|ret_ty_opt| {
                            ret_ty_opt.as_ref().and_then(|rt| {
                                if let Type::Array { inner, .. } = rt {
                                    Some(self.type_to_c(inner))
                                } else {
                                    None
                                }
                            })
                        })
                    } else {
                        from_call
                    }
                } else {
                    None
                };

                if let Some(base_type) = call_returns_array_type {
                    // Function call returning array - declare as pointer
                    self.write(&format!("{}* {}", base_type, let_stmt.name));
                } else {
                    self.write(&self.c_named_decl(&let_stmt.name, &let_stmt.ty));
                }

                if let Some(ref init) = let_stmt.init {
                    if let IREexpr::Call {
                        callee,
                        args: _,
                        return_type,
                        tuple_destructure_index,
                    } = init
                        && callee == "channel"
                        && tuple_destructure_index.is_some()
                    {
                        if *tuple_destructure_index == Some(0) {
                            if let Some(Type::Tuple { elements }) = return_type
                                && elements.len() == 2
                                && let Type::Sender { elem_type } = &elements[0]
                            {
                                let temp_rx_name = format!("_channel_rx_{}", let_stmt.name);
                                self.writeln(";");
                                self.write_indent();
                                self.writeln(&format!(
                                    "{} {};",
                                    self.type_to_c(&elements[1]),
                                    temp_rx_name
                                ));
                                self.emit_ion_channel_new(
                                    elem_type,
                                    &[],
                                    &let_stmt.name,
                                    &temp_rx_name,
                                );
                                self.scope_register_binding(&let_stmt.name, &let_stmt.ty);
                                return;
                            }
                        } else if *tuple_destructure_index == Some(1) {
                            self.write(" = _channel_rx_temp;");
                            self.writeln("");
                            return;
                        }
                    }

                    if let IREexpr::Match {
                        expr: match_expr,
                        enum_type,
                        arms,
                        scrutinee_type,
                        ..
                    } = init
                    {
                        self.writeln(";");
                        self.generate_match_block(
                            match_expr,
                            enum_type,
                            arms,
                            Some((&let_stmt.name, &let_stmt.ty)),
                            scrutinee_type.as_ref(),
                        );
                        self.scope_register_binding(&let_stmt.name, &let_stmt.ty);
                        self.mark_moves_in_expr(match_expr);
                        self.flush_pending_field_nulls();
                        return;
                    }

                    self.write(" = ");
                    // Special handling: if assigning String type from string literal, convert it
                    if matches!(let_stmt.ty, Type::String) {
                        if let IREexpr::StringLit(value) = init {
                            self.write_ion_string_from_literal(value);
                        } else {
                            self.generate_expr_with_type(init, Some(&let_stmt.ty));
                        }
                    } else if matches!(&let_stmt.ty, Type::Generic { .. }) {
                        self.generate_expr_with_type(init, Some(&let_stmt.ty));
                    } else if matches!(let_stmt.ty, Type::Int) {
                        // Special handling: if declared type is Int but init is a variable,
                        // it might actually be a Vec pointer (type inference limitation).
                        // Check if the init expression looks like it should be cast.
                        if let IREexpr::Var(_) = init {
                            // For variables, generate as-is - the type might be wrong but
                            // we'll let the C compiler handle it or fix in a later pass
                            self.generate_expr_with_type(init, Some(&let_stmt.ty));
                        } else {
                            self.generate_expr_with_type(init, Some(&let_stmt.ty));
                        }
                    } else {
                        // Pass type context so enum/struct literals can use monomorphized names
                        self.generate_expr_with_type(init, Some(&let_stmt.ty));
                    }
                } else if self.needs_drop(&let_stmt.ty) {
                    self.write(" = 0");
                }

                self.writeln(";");
                self.scope_register_binding(&let_stmt.name, &let_stmt.ty);
                if Self::should_silence_unused_binding(&let_stmt.name, &let_stmt.ty) {
                    self.emit_silence_unused_binding(&let_stmt.name);
                }
                if let Some(init) = &let_stmt.init {
                    self.mark_moves_in_expr(init);
                }
                self.flush_pending_field_nulls();
            }
            IRStmt::Return(ret) => {
                self.emit_function_return(ret);
            }
            IRStmt::Break => {
                self.emit_break_statement();
            }
            IRStmt::Continue => {
                self.emit_continue_statement();
            }
            IRStmt::Expr(expr) => {
                // Special handling: match expressions used as statements should be blocks, not statement expressions
                match expr {
                    IREexpr::Match {
                        expr: match_expr,
                        enum_type,
                        arms,
                        scrutinee_type,
                        ..
                    } => {
                        self.generate_match_block(
                            match_expr,
                            enum_type,
                            arms,
                            None,
                            scrutinee_type.as_ref(),
                        );
                        self.mark_moves_in_expr(match_expr);
                        self.flush_pending_field_nulls();
                    }
                    IREexpr::Send {
                        channel,
                        value,
                        value_type,
                    } => {
                        let sender_addr = self.sender_addr_code(channel);
                        let val_tmp = format!("_send_val_{}", self.temp_var_counter);
                        self.temp_var_counter += 1;
                        let st_tmp = format!("_send_st_{}", self.temp_var_counter);
                        self.temp_var_counter += 1;
                        let c_ty = self.type_to_c(value_type);
                        self.write_indent();
                        self.writeln("{");
                        self.indent_level += 1;
                        self.write_indent();
                        self.write(&format!("{c_ty} {val_tmp} = "));
                        self.generate_expr_with_type(value, Some(value_type));
                        self.writeln(";");
                        self.write_indent();
                        self.writeln(&format!(
                            "int {st_tmp} = ion_channel_send({sender_addr}, &{val_tmp});"
                        ));
                        self.write_indent();
                        self.writeln(&format!("if ({st_tmp} != 0) {{"));
                        self.indent_level += 1;
                        self.emit_drop_at_path(&val_tmp, value_type);
                        self.indent_level -= 1;
                        self.write_indent();
                        self.writeln("}");
                        self.indent_level -= 1;
                        self.write_indent();
                        self.writeln("}");
                        self.mark_moves_in_expr(value.as_ref());
                        self.flush_pending_field_nulls();
                    }
                    IREexpr::Call {
                        callee,
                        args,
                        return_type,
                        ..
                    } if callee == "try_send" && args.len() == 2 => {
                        let sender_addr = self.sender_addr_code(&args[0]);
                        let value_type = match return_type.as_ref() {
                            Some(Type::Generic { name, params })
                                if name == "TrySendResult" && params.len() == 1 =>
                            {
                                params[0].clone()
                            }
                            _ => panic!("compiler bug: try_send missing TrySendResult type"),
                        };
                        let val_tmp = format!("_try_send_val_{}", self.temp_var_counter);
                        self.temp_var_counter += 1;
                        let st_tmp = format!("_try_send_st_{}", self.temp_var_counter);
                        self.temp_var_counter += 1;
                        let c_ty = self.type_to_c(&value_type);
                        self.write_indent();
                        self.writeln("{");
                        self.indent_level += 1;
                        self.write_indent();
                        self.write(&format!("{c_ty} {val_tmp} = "));
                        self.generate_expr_with_type(&args[1], Some(&value_type));
                        self.writeln(";");
                        self.write_indent();
                        self.writeln(&format!(
                            "int {st_tmp} = ion_channel_try_send({sender_addr}, &{val_tmp});"
                        ));
                        self.write_indent();
                        self.writeln(&format!("if ({st_tmp} != 0) {{"));
                        self.indent_level += 1;
                        self.emit_drop_at_path(&val_tmp, &value_type);
                        self.indent_level -= 1;
                        self.write_indent();
                        self.writeln("}");
                        self.indent_level -= 1;
                        self.write_indent();
                        self.writeln("}");
                        self.mark_moves_in_expr(&args[1]);
                        self.flush_pending_field_nulls();
                    }
                    IREexpr::Call { callee, .. } if callee == "Vec::set" => {
                        self.write_indent();
                        self.write("(void)(");
                        self.generate_expr(expr);
                        self.writeln(");");
                        self.mark_moves_in_expr(expr);
                        self.flush_pending_field_nulls();
                    }
                    IREexpr::AssignField {
                        target,
                        value,
                        field_ty,
                    } => {
                        if self.type_needs_drop(field_ty) {
                            let path = self.capture_expr_code(target);
                            let tmp = format!("_ion_set_{}", self.temp_var_counter);
                            self.temp_var_counter += 1;
                            let c_ty = self.type_to_c(field_ty);
                            self.write_indent();
                            self.write(&format!("{c_ty} {tmp} = "));
                            self.generate_expr_with_type(value, Some(field_ty));
                            self.writeln(";");
                            self.emit_drop_at_path(&path, field_ty);
                            self.write_indent();
                            self.writeln(&format!("{path} = {tmp};"));
                        } else {
                            self.write_indent();
                            self.generate_expr(target);
                            self.write(" = ");
                            self.generate_expr_with_type(value, Some(field_ty));
                            self.writeln(";");
                        }
                        self.mark_moves_in_expr(expr);
                        self.flush_pending_field_nulls();
                    }
                    _ => {
                        self.write_indent();
                        self.generate_expr(expr);
                        self.writeln(";");
                        self.mark_moves_in_expr(expr);
                        self.flush_pending_field_nulls();
                    }
                }
            }
            IRStmt::Defer(expr) => {
                self.mark_moves_in_expr(expr);
            }
            IRStmt::Spawn(spawn) => {
                self.generate_spawn(spawn);
            }
            IRStmt::Select(sel) => {
                self.generate_select(sel);
            }
            IRStmt::If(ir_if) => {
                // Generate: if (cond) { ... } else { ... }
                self.write_indent();
                self.write("if (");
                self.generate_expr_conditional(&ir_if.cond);
                self.writeln(") {");
                self.indent_level += 1;
                self.generate_block(&ir_if.then_block);
                self.indent_level -= 1;
                self.write_indent();
                if let Some(ref else_block) = ir_if.else_block {
                    self.writeln("} else {");
                    self.indent_level += 1;
                    self.generate_block(else_block);
                    self.indent_level -= 1;
                    self.write_indent();
                    self.writeln("}");
                } else {
                    self.writeln("}");
                }
            }
            IRStmt::While(ir_while) => {
                let break_label = format!("loop_break_{}", self.temp_var_counter);
                self.temp_var_counter += 1;
                let prev_break = self.loop_break_label.take();
                let prev_break_used = self.loop_break_label_used;
                let prev_continue_label = self.loop_continue_label.take();
                let prev_unwind_depth = self.loop_unwind_depth;
                self.loop_break_label_used = false;
                self.loop_break_label = Some(break_label.clone());
                self.loop_continue_label = ir_while.continue_label.clone();
                self.loop_unwind_depth = self.scope_stack.len();
                self.write_indent();
                self.write("while (");
                self.generate_expr_conditional(&ir_while.cond);
                self.writeln(") {");
                self.indent_level += 1;
                self.generate_block(&ir_while.body);
                if let Some(ref step) = ir_while.step {
                    if let Some(ref label) = ir_while.continue_label {
                        self.write_indent();
                        self.writeln(&format!("goto {};", label));
                        self.write_indent();
                        // Empty statement: a declaration may follow, and Clang rejects a label on a declaration.
                        self.writeln(&format!("{}: ;", label));
                    }
                    self.generate_block(step);
                }
                self.indent_level -= 1;
                self.write_indent();
                self.writeln("}");
                if self.loop_break_label_used {
                    self.write_indent();
                    // Empty statement: a declaration may follow, and Clang rejects a label on a declaration.
                    self.writeln(&format!("{}: ;", break_label));
                }
                self.loop_break_label = prev_break;
                self.loop_break_label_used = prev_break_used;
                self.loop_continue_label = prev_continue_label;
                self.loop_unwind_depth = prev_unwind_depth;
            }
            IRStmt::UnsafeBlock(unsafe_block) => {
                // Unsafe blocks lower to regular C blocks (no special syntax)
                // Track unsafe context to disable bounds checking
                let prev_unsafe = self.in_unsafe_block;
                self.in_unsafe_block = true;

                self.write_indent();
                self.writeln("{");
                self.indent_level += 1;
                self.generate_block(&unsafe_block.body);
                self.indent_level -= 1;
                self.write_indent();
                self.writeln("}");

                self.in_unsafe_block = prev_unsafe;
            }
            IRStmt::Scope(scope) => {
                self.write_indent();
                self.writeln("{");
                self.indent_level += 1;
                self.next_block_joins = true;
                self.generate_block(&scope.body);
                self.indent_level -= 1;
                self.write_indent();
                self.writeln("}");
            }
        }
    }

    fn generate_expr(&mut self, expr: &IREexpr) {
        self.generate_expr_with_type(expr, None);
    }

    fn generate_expr_conditional(&mut self, expr: &IREexpr) {
        // Generate expression for conditional context (if/while) without extra parentheses
        match expr {
            IREexpr::BinOp {
                op,
                left,
                right,
                result_type,
            } => {
                self.generate_defined_binop(*op, left, right, result_type, false);
            }
            IREexpr::UnOp {
                op,
                operand,
                result_type,
            } => self.generate_defined_unop(*op, operand, result_type),
            _ => {
                // For other expressions, generate normally
                self.generate_expr(expr);
            }
        }
    }

    fn generate_expr_with_type(&mut self, expr: &IREexpr, type_context: Option<&Type>) {
        match expr {
            IREexpr::Lit(value) => {
                self.write(&value.to_string());
            }
            IREexpr::IntLimit { ty, max } => {
                self.write(&crate::cgen::types::c_int_limit(ty, *max));
            }
            IREexpr::BoolLiteral(value) => {
                // Boolean literals: true -> 1, false -> 0
                self.write(if *value { "1" } else { "0" });
            }
            IREexpr::FloatLiteral(value) => {
                // Float literals: output as-is (C will handle the format)
                self.write(&value.to_string());
            }
            IREexpr::Var(name) => {
                if name == crate::ir::MATCH_PARENT_SCRUTINEE {
                    let resolved = self.parent_match_scrutinee().unwrap_or(name).to_string();
                    self.scope_mark_binding_read(&resolved);
                    self.write(&resolved);
                } else {
                    self.scope_mark_binding_read(name);
                    self.write(name);
                }
            }
            IREexpr::AddressOf {
                inner, mutable: _, ..
            } => {
                if matches!(inner.as_ref(), IREexpr::Index { .. }) {
                    self.addressing_index = true;
                    self.generate_expr(inner);
                    self.addressing_index = false;
                } else {
                    self.write("&");
                    self.generate_expr(inner);
                }
            }
            IREexpr::BinOp {
                op,
                left,
                right,
                result_type,
            } => {
                self.generate_defined_binop(*op, left, right, result_type, true);
            }
            IREexpr::UnOp {
                op,
                operand,
                result_type,
            } => self.generate_defined_unop(*op, operand, result_type),
            IREexpr::Send {
                channel,
                value,
                value_type,
            } => {
                if let Some(Type::Endpoint {
                    protocol,
                    step,
                    dual: _,
                }) = self.stored_expr_type(channel)
                {
                    self.emit_endpoint_send(&protocol, step, channel, value, value_type);
                } else {
                    let sender_addr = self.sender_addr_code(channel);
                    let val_tmp = format!("_send_val_{}", self.temp_var_counter);
                    self.temp_var_counter += 1;
                    let st_tmp = format!("_send_st_{}", self.temp_var_counter);
                    self.temp_var_counter += 1;
                    let c_ty = self.type_to_c(value_type);
                    let result_ty = Type::Generic {
                        name: "SendResult".to_string(),
                        params: vec![value_type.clone()],
                    };
                    let result_c = self.type_to_c(&result_ty);
                    let sent = self.c_enum_literal(&result_c, "SendResult", "Sent", None);
                    let closed =
                        self.c_enum_literal(&result_c, "SendResult", "Closed", Some(&val_tmp));
                    self.write("({ ");
                    self.write(&format!("{c_ty} {val_tmp} = "));
                    self.generate_expr_with_type(value, Some(value_type));
                    self.write("; ");
                    self.write(&format!(
                        "int {st_tmp} = ion_channel_send({sender_addr}, &{val_tmp}); "
                    ));
                    self.write(&format!("{result_c} _send_res; "));
                    self.write(&format!(
                    "if ({st_tmp} == 0) {{ _send_res = {sent}; }} else {{ _send_res = {closed}; }} "
                ));
                    self.write("_send_res; })");
                }
            }
            IREexpr::Recv { channel, elem_type } => {
                if let Some(Type::Endpoint {
                    protocol,
                    step,
                    dual: _,
                }) = self.stored_expr_type(channel)
                {
                    self.emit_endpoint_recv(&protocol, step, channel);
                } else {
                    let recv_addr = self.sender_addr_code(channel);
                    let tmp = format!("_recv_tmp_{}", self.temp_var_counter);
                    self.temp_var_counter += 1;
                    let st_tmp = format!("_recv_st_{}", self.temp_var_counter);
                    self.temp_var_counter += 1;
                    let c_ty = self.type_to_c(elem_type);
                    let option_ty = Type::Generic {
                        name: "Option".to_string(),
                        params: vec![elem_type.clone()],
                    };
                    let option_c = self.type_to_c(&option_ty);
                    let some = self.c_enum_literal(&option_c, "Option", "Some", Some(&tmp));
                    let none = self.c_enum_literal(&option_c, "Option", "None", None);
                    self.write("({ ");
                    self.write(&format!("{c_ty} {tmp} = {{0}}; "));
                    self.write(&format!(
                        "int {st_tmp} = ion_channel_recv({recv_addr}, &{tmp}); "
                    ));
                    self.write(&format!("{option_c} _recv_opt; "));
                    self.write(&format!(
                    "if ({st_tmp} == 0) {{ _recv_opt = {some}; }} else {{ _recv_opt = {none}; }} "
                ));
                    self.write("_recv_opt; })");
                }
            }
            IREexpr::StructLit { type_name, fields } => {
                // C99 compound literal: (Type){ .field1 = v1, .field2 = v2 }
                // Use type context to get monomorphized name if it's a generic type
                let struct_type_name = if let Some(context_ty) = type_context {
                    self.type_to_c(context_ty)
                } else {
                    type_name.clone()
                };

                // Check if any field is an array that needs special handling
                let mut needs_memcpy = false;
                let mut array_field_name = String::new();
                let mut array_var_name = String::new();

                // Try to get struct definition from type_context or type_name
                let struct_name_to_check = if let Some(context_ty) = type_context {
                    if let Type::Struct(struct_name) = context_ty {
                        Some(struct_name)
                    } else {
                        None
                    }
                } else {
                    Some(type_name)
                };

                if let Some(struct_name) = struct_name_to_check
                    && let Some(struct_decl) = self.struct_map.get(struct_name)
                {
                    for field in fields.iter() {
                        if let IREexpr::Var(ref var_name) = field.value {
                            // Check if this field is an array type
                            if let Some(field_decl) =
                                struct_decl.fields.iter().find(|f| f.name == field.name)
                                && let Type::Array { .. } = field_decl.ty
                            {
                                needs_memcpy = true;
                                array_field_name = field.name.clone();
                                array_var_name = var_name.clone();
                                break;
                            }
                        }
                    }
                }

                if needs_memcpy {
                    // When assigning an array variable to an array field, we can't do it directly in C
                    // Generate struct with zero initialization - memcpy will be handled separately
                    // We'll generate a comment marker that the caller can detect
                    self.write(&format!(
                        "({}){{{{0}}}} /* ARRAY_FIELD:{}:{} */",
                        struct_type_name, array_field_name, array_var_name
                    ));
                } else {
                    self.write(&format!("({}){{", struct_type_name));
                    for (i, field) in fields.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.write(&format!(".{} = ", field.name));
                        let field_ty = struct_name_to_check
                            .and_then(|sn| self.struct_map.get(sn))
                            .and_then(|decl| {
                                decl.fields
                                    .iter()
                                    .find(|f| f.name == field.name)
                                    .map(|f| f.ty.clone())
                            });
                        let prev = self.nested_designated_init;
                        self.nested_designated_init = true;
                        self.generate_expr_with_type(&field.value, field_ty.as_ref());
                        self.nested_designated_init = prev;
                    }
                    self.write("}");
                }
            }
            IREexpr::FieldAccess {
                base,
                field,
                is_pointer,
                ..
            } => {
                // Special handling for String literal field access
                if let IREexpr::Var(ref var_name) = **base {
                    // Check if this variable is a function parameter of type String
                    let should_use_pointer =
                        if let Some(param_ty) = self.current_function_params.get(var_name) {
                            match param_ty {
                                Type::String => true,
                                Type::Ref { inner, .. } => matches!(
                                    **inner,
                                    Type::Struct(_)
                                        | Type::Generic { .. }
                                        | Type::Tuple { .. }
                                        | Type::String
                                ),
                                _ => *is_pointer,
                            }
                        } else if let Some(ty) = self.lookup_var_type(var_name) {
                            matches!(
                                ty,
                                Type::Ref {
                                    inner,
                                    ..
                                } if matches!(
                                    *inner,
                                    Type::Struct(_)
                                        | Type::Generic { .. }
                                        | Type::Tuple { .. }
                                        | Type::String
                                )
                            )
                        } else {
                            *is_pointer
                        };

                    self.generate_expr(base);
                    if should_use_pointer {
                        self.write("->");
                    } else {
                        self.write(".");
                    }
                    self.write(field);
                } else if let IREexpr::StringLit(ref s) = **base {
                    match field.as_str() {
                        "data" => {
                            // "string".data -> uint8_t pointer for byte APIs
                            self.write("(uint8_t*)");
                            self.generate_expr(base);
                        }
                        "len" => {
                            // "string".len -> length of the string
                            self.write(&s.len().to_string());
                        }
                        _ => {
                            // Should not happen if type checker is correct
                            self.generate_expr(base);
                            self.write(".");
                            self.write(field);
                        }
                    }
                } else {
                    // Reborrowed embedded struct fields are C places (struct
                    // values at base->field), not pointers. Further field
                    // access must use '.' even when Ion types the base as
                    // &Struct (ION_SPEC §5.3 field reborrow).
                    let use_arrow = match base.as_ref() {
                        IREexpr::FieldAccess { .. } => false,
                        _ => {
                            let ty = self.stored_expr_type(base).unwrap_or_else(|| {
                                panic!("compiler bug: field access base missing checked type")
                            });
                            matches!(
                                ty,
                                Type::Ref { inner, .. }
                                    if matches!(
                                        *inner,
                                        Type::Struct(_) | Type::Generic { .. } | Type::Tuple { .. }
                                    )
                            )
                        }
                    };
                    self.generate_expr(base);
                    if use_arrow {
                        self.write("->");
                    } else {
                        self.write(".");
                    }
                    self.write(field);
                }
            }
            IREexpr::EnumLit {
                enum_name,
                variant,
                args,
                named_fields,
                ty,
            } => {
                let context_ty = type_context.or(Some(ty));
                let is_generic = self
                    .enum_map
                    .get(enum_name)
                    .map(|e| !e.generics.is_empty())
                    .unwrap_or(false);
                let monomorphized_enum_name = if is_generic {
                    if let Some(context_ty) = context_ty {
                        self.type_to_c(context_ty)
                    } else {
                        enum_name.clone()
                    }
                } else {
                    enum_name.clone()
                };
                self.emit_enum_variant_compound_literal(
                    &monomorphized_enum_name,
                    enum_name,
                    variant,
                    args,
                    named_fields.as_deref(),
                    context_ty,
                );
            }
            IREexpr::Match {
                expr,
                enum_type,
                arms,
                scrutinee_type,
                result_type,
            } => {
                self.write("({ ");
                self.scope_begin(&[]);
                if !matches!(result_type, Type::Void) {
                    let result_name = format!("_ion_match_res_{}", self.temp_var_counter);
                    self.temp_var_counter += 1;
                    self.write_indent();
                    self.writeln(&format!("{} {result_name};", self.type_to_c(result_type)));
                    self.generate_match_block(
                        expr,
                        enum_type,
                        arms,
                        Some((&result_name, result_type)),
                        scrutinee_type.as_ref(),
                    );
                    self.scope_emit_exit();
                    self.write(&format!("{result_name}; }})"));
                } else {
                    self.generate_match_block(expr, enum_type, arms, None, scrutinee_type.as_ref());
                    self.scope_emit_exit();
                    self.write("0; })");
                }
            }
            IREexpr::Call {
                callee,
                args,
                return_type,
                tuple_destructure_index: _,
            } => {
                let resolved_callee = callee.clone();

                // Handle special built-in functions
                let builtin_return_type = match resolved_callee.as_str() {
                    "Vec::new" | "Vec::with_capacity" => type_context.or(return_type.as_ref()),
                    // Prefer let/field expected type over a wrong IR fallback (e.g. Box<int>).
                    s if s.starts_with("Box::new") => type_context.or(return_type.as_ref()),
                    "Box::unwrap" => type_context.or(return_type.as_ref()),
                    _ => return_type.as_ref(),
                };
                if let Some(code) =
                    self.generate_builtin_call(resolved_callee.as_str(), args, builtin_return_type)
                {
                    self.write(&code);
                    for arg in args {
                        self.mark_moves_in_expr(arg);
                    }
                } else if let Some(sig) = self.lookup_var_type(&resolved_callee).and_then(|ty| {
                    let Type::Struct(name) = ty else {
                        return None;
                    };
                    self.closures.get(&name).cloned()
                }) {
                    self.write(&sig.symbol);
                    self.write("(&");
                    self.write(&resolved_callee);
                    for arg in args {
                        self.write(", ");
                        self.generate_expr(arg);
                        self.mark_moves_in_expr(arg);
                    }
                    self.write(")");
                    if sig.consumes {
                        self.scope_mark_moved(&resolved_callee);
                    }
                } else {
                    // Regular function call.
                    let func_name = self.resolve_c_function_name(&resolved_callee);

                    self.write(&func_name);
                    self.write("(");
                    let param_types = self.lookup_call_param_types(&resolved_callee, &func_name);
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        let param_ty = param_types.as_ref().and_then(|pts| pts.get(i).cloned());
                        // If this is an extern function expecting int by value, convert &int (immutable) arguments to int
                        // For &int -> int: just use the inner expression (the variable itself)
                        // For &mut int -> int*: keep the address-of (don't dereference)
                        if let Some(param_types) = param_types.as_ref()
                            && i < param_types.len()
                            && let Some((array_name, size, elem_ty)) =
                                self.match_array_to_slice_coercion(&param_types[i], arg)
                        {
                            self.emit_array_to_slice_ptr(&array_name, size, &elem_ty);
                            continue;
                        }
                        if let Some(param_types) = self.extern_functions.get(&func_name)
                            && i < param_types.len()
                            && let Type::Ref {
                                inner: boxed_int,
                                mutable: param_mutable,
                            } = &param_types[i]
                            && matches!(**boxed_int, Type::Int)
                            && let IREexpr::AddressOf {
                                inner,
                                mutable: arg_mutable,
                                ..
                            } = arg
                        {
                            // Only dereference if parameter is &int (immutable) and argument is also &int (immutable)
                            // For &mut int parameters, we need to pass int*, so keep the address-of
                            if !param_mutable && !arg_mutable {
                                // &int -> int: just use the inner expression (the variable itself)
                                // This converts &client_fd to client_fd
                                self.generate_expr(inner);
                                continue;
                            }
                            // For &mut int, fall through to generate &var normally
                        }
                        if let IREexpr::StringLit(value) = arg {
                            if matches!(param_ty, Some(Type::String))
                                || (param_ty.is_none()
                                    && !self.extern_functions.contains_key(&func_name))
                            {
                                self.write_ion_string_from_literal(value);
                                continue;
                            }
                            if matches!(param_ty.as_ref(), Some(pt) if Self::param_is_byte_ptr(pt))
                            {
                                self.write("(uint8_t*)");
                            }
                        }
                        // Non-copy field through &Struct / &mut Struct is already &Field in
                        // Ion (ION_SPEC §5.3). C still loads the field (c->data as Vec*), so
                        // take its address when the callee expects &T / &mut T (Vec**).
                        // A field whose own type is a reference is already that pointer
                        // (`h.v` is `int*`). Another `&` would be `int**`.
                        if matches!(param_ty, Some(Type::Ref { .. }))
                            && matches!(arg, IREexpr::FieldAccess { .. })
                            && matches!(self.stored_expr_type(arg), Some(Type::Ref { .. }))
                            && !self.field_member_is_reference(arg)
                        {
                            self.write("&(");
                            self.generate_expr(arg);
                            self.write(")");
                            continue;
                        }
                        if let Some(ref pty) = param_ty {
                            if matches!(arg, IREexpr::ArrayLiteral { .. })
                                && let Type::Array { .. } = pty
                            {
                                self.write(&format!("({})", self.type_to_c(pty)));
                            }
                            self.generate_expr_with_type(arg, Some(pty));
                        } else {
                            self.generate_expr(arg);
                        }
                    }
                    self.write(")");
                    for arg in args {
                        self.mark_moves_in_expr(arg);
                    }
                }
            }
            IREexpr::StringLit(value) => {
                if matches!(type_context, Some(Type::String)) {
                    self.write_ion_string_from_literal(value);
                } else {
                    let escaped = Self::escape_c_string_literal_content(value);
                    self.write(&format!("\"{}\"", escaped));
                }
            }
            IREexpr::TupleLit {
                elements,
                elem_types,
            } => {
                let name = tuple_type_name(elem_types);
                self.write(&format!("({}){{", name));
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.write(&format!(".f{} = ", i));
                    let prev = self.nested_designated_init;
                    self.nested_designated_init = true;
                    self.generate_expr_with_type(elem, elem_types.get(i));
                    self.nested_designated_init = prev;
                }
                self.write("}");
            }
            IREexpr::ArrayLiteral { elements, repeat } => {
                let elem_ty = match type_context {
                    Some(Type::Array { inner, .. }) => Some(inner.as_ref().clone()),
                    _ => None,
                };
                if let Some((value_expr, count)) = repeat {
                    // Array repeat: [value; count]
                    // For zero initialization, use {0} syntax
                    // For other values, we'll generate a compound literal with explicit initialization
                    if let IREexpr::Lit(0) = **value_expr {
                        // Zero initialization: [0; count] -> {0}
                        self.write("{0}");
                    } else {
                        // Non-zero: generate explicit initialization
                        // Note: C doesn't support [value; count] directly, so we generate
                        // a compound literal. For large arrays, this might be inefficient,
                        // but it's correct for now.
                        self.write("{");
                        let count_val = *count;
                        for i in 0..count_val {
                            if i > 0 {
                                self.write(", ");
                            }
                            self.generate_expr_with_type(value_expr, elem_ty.as_ref());
                        }
                        self.write("}");
                    }
                } else {
                    // Regular array literal: [1, 2, 3] -> {1, 2, 3}
                    self.write("{");
                    for (i, elem) in elements.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.generate_expr_with_type(elem, elem_ty.as_ref());
                    }
                    self.write("}");
                }
            }
            IREexpr::Index {
                target,
                index,
                target_type,
            } => {
                let bounds_check = self.bounds_check_for_target_type(target_type.as_ref());

                if self.in_unsafe_block {
                    self.emit_index_access(target, index, bounds_check.as_ref());
                } else {
                    let temp_var = format!("__ion_idx_{}", self.temp_var_counter);
                    self.temp_var_counter += 1;

                    match bounds_check {
                        Some(BoundsCheck::Fixed(len)) => {
                            self.write("({ int ");
                            self.write(&temp_var);
                            self.write(" = ");
                            self.generate_expr(index);
                            self.write("; (");
                            self.write(&temp_var);
                            self.write(" >= 0 && ");
                            self.write(&temp_var);
                            self.write(&format!(" < {}) ? ", len));
                            if self.addressing_index {
                                self.write("&");
                            }
                            self.generate_expr(target);
                            self.write("[");
                            self.write(&temp_var);
                            self.write("] : (ion_panic(\"Array index out of bounds\"), ");
                            if self.addressing_index {
                                self.write("&");
                            }
                            self.generate_expr(target);
                            self.write("[0]); })");
                        }
                        Some(BoundsCheck::StringLen) => {
                            self.write("({ int ");
                            self.write(&temp_var);
                            self.write(" = ");
                            self.generate_expr(index);
                            self.write("; (");
                            self.write(&temp_var);
                            self.write(" >= 0 && ");
                            self.write(&temp_var);
                            self.write(" < (int)(");
                            self.generate_expr(target);
                            self.write("->len)) ? ");
                            if self.addressing_index {
                                self.write("&");
                            }
                            self.generate_expr(target);
                            self.write("->data[");
                            self.write(&temp_var);
                            if self.addressing_index {
                                self.write("] : (ion_panic(\"String index out of bounds\"), &");
                                self.generate_expr(target);
                                self.write("->data[0]); })");
                            } else {
                                self.write(
                                    "] : (ion_panic(\"String index out of bounds\"), (uint8_t)0); })",
                                );
                            }
                        }
                        Some(BoundsCheck::SliceLen { by_ref }) => {
                            self.write("({ int ");
                            self.write(&temp_var);
                            self.write(" = ");
                            self.generate_expr(index);
                            self.write("; (");
                            self.write(&temp_var);
                            self.write(" >= 0 && ");
                            self.write(&temp_var);
                            self.write(" < ");
                            self.emit_slice_len(target, by_ref);
                            self.write(") ? ");
                            if self.addressing_index {
                                self.write("&");
                            }
                            self.emit_slice_data_index(target, &temp_var, by_ref);
                            self.write(" : (ion_panic(\"Slice index out of bounds\"), ");
                            if self.addressing_index {
                                self.write("&");
                            }
                            self.emit_slice_data_index(target, "0", by_ref);
                            self.write("); })");
                        }
                        None => {
                            self.emit_index_access(target, index, None);
                        }
                    }
                }
            }
            IREexpr::Cast { expr, target_type } => {
                // Cast: (target_type)expr
                self.write("(");
                self.write(&self.type_to_c(target_type));
                self.write(")");
                self.generate_expr(expr);
            }
            IREexpr::Assign { target, value } => {
                self.write(target);
                self.write(" = ");
                let ty = self.lookup_var_type(target);
                self.generate_expr_with_type(value, ty.as_ref());
            }
            IREexpr::AssignIndex {
                target,
                index,
                value,
                target_type,
            } => {
                let resolved_target = target_type.clone().unwrap_or_else(|| {
                    panic!("compiler bug: index assignment missing checked target type")
                });
                let elem_ty = Self::index_element_type(&resolve_type_alias(
                    &resolved_target,
                    &self.type_aliases,
                ));
                let bounds_check = self.bounds_check_for_target_type(Some(&resolved_target));
                let mut value_code = String::new();
                let old = std::mem::replace(&mut self.output, value_code);
                self.generate_expr_with_type(value, elem_ty.as_ref());
                value_code = std::mem::replace(&mut self.output, old);
                if self.in_unsafe_block {
                    self.emit_index_access(target, index, bounds_check.as_ref());
                    self.write(" = ");
                    self.write(&value_code);
                } else {
                    match bounds_check {
                        None => {
                            self.emit_index_access(target, index, None);
                            self.write(" = ");
                            self.write(&value_code);
                        }
                        Some(BoundsCheck::Fixed(len)) => {
                            let temp_var = format!("__ion_idx_{}", self.temp_var_counter);
                            self.temp_var_counter += 1;
                            self.write("({ int ");
                            self.write(&temp_var);
                            self.write(" = ");
                            self.generate_expr(index);
                            self.write("; ");
                            self.write(&format!(
                                "if (!({temp_var} >= 0 && {temp_var} < {len})) ion_panic(\"Array index out of bounds\"); "
                            ));
                            self.generate_expr(target);
                            self.write("[");
                            self.write(&temp_var);
                            self.write("] = ");
                            self.write(&value_code);
                            self.write("; })");
                        }
                        Some(BoundsCheck::StringLen) => {
                            let temp_var = format!("__ion_idx_{}", self.temp_var_counter);
                            self.temp_var_counter += 1;
                            self.write("({ int ");
                            self.write(&temp_var);
                            self.write(" = ");
                            self.generate_expr(index);
                            self.write("; ");
                            self.write(&format!("if (!({temp_var} >= 0 && {temp_var} < (int)("));
                            self.generate_expr(target);
                            self.write("->len))) ion_panic(\"String index out of bounds\"); ");
                            self.generate_expr(target);
                            self.write("->data[");
                            self.write(&temp_var);
                            self.write("] = ");
                            self.write(&value_code);
                            self.write("; })");
                        }
                        Some(BoundsCheck::SliceLen { by_ref }) => {
                            let temp_var = format!("__ion_idx_{}", self.temp_var_counter);
                            self.temp_var_counter += 1;
                            self.write("({ int ");
                            self.write(&temp_var);
                            self.write(" = ");
                            self.generate_expr(index);
                            self.write("; ");
                            self.write(&format!("if (!({temp_var} >= 0 && {temp_var} < "));
                            self.emit_slice_len(target, by_ref);
                            self.write(")) ion_panic(\"Slice index out of bounds\"); ");
                            self.emit_slice_data_index(target, &temp_var, by_ref);
                            self.write(" = ");
                            self.write(&value_code);
                            self.write("; })");
                        }
                    }
                }
            }
            IREexpr::AssignField {
                target,
                value,
                field_ty,
            } => {
                self.generate_expr(target);
                self.write(" = ");
                self.generate_expr_with_type(value, Some(field_ty));
            }
            IREexpr::FnLiteral(lit) => {
                self.generate_fn_literal(lit);
                if lit.captures.is_empty() {
                    self.write(&lit.symbol);
                } else {
                    let struct_name = lit.env_struct.as_deref().unwrap_or("ion_closure");
                    self.write(&format!("({struct_name}){{"));
                    for (i, (name, _)) in lit.captures.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.write(&format!(".{name} = {name}"));
                        if let Some(ty) = lit.captures.get(i).map(|(_, ty)| ty)
                            && self.type_needs_drop(ty)
                        {
                            self.scope_mark_moved(name);
                        }
                    }
                    self.write("}");
                }
            }
            IREexpr::Spawn {
                captures,
                body,
                result,
            } => {
                self.generate_spawn_expr(&IRSpawn {
                    captures: captures.clone(),
                    body: body.clone(),
                    result: result.clone(),
                });
            }
        }
    }

    fn type_to_c(&self, ty: &Type) -> String {
        let resolved = resolve_type_alias(ty, &self.type_aliases);
        type_to_c_impl(&resolved)
    }

    fn bounds_check_for_target_type(&self, ty: Option<&Type>) -> Option<BoundsCheck> {
        let resolved = ty.map(|t| resolve_type_alias(t, &self.type_aliases));
        match resolved.as_ref() {
            Some(Type::Array { size, .. }) => Some(BoundsCheck::Fixed(*size)),
            Some(Type::String) => Some(BoundsCheck::StringLen),
            Some(Type::Slice { .. }) => Some(BoundsCheck::SliceLen { by_ref: false }),
            Some(Type::Ref { inner, .. }) => match inner.as_ref() {
                Type::Array { size, .. } => Some(BoundsCheck::Fixed(*size)),
                Type::String => Some(BoundsCheck::StringLen),
                Type::Slice { .. } => Some(BoundsCheck::SliceLen { by_ref: true }),
                _ => None,
            },
            _ => None,
        }
    }

    fn emit_slice_len(&mut self, target: &IREexpr, by_ref: bool) {
        self.generate_expr(target);
        if by_ref {
            self.write("->len");
        } else {
            self.write(".len");
        }
    }

    fn emit_slice_data_index(&mut self, target: &IREexpr, index: &str, by_ref: bool) {
        self.generate_expr(target);
        if by_ref {
            self.write("->data[");
        } else {
            self.write(".data[");
        }
        self.write(index);
        self.write("]");
    }

    fn emit_index_access(
        &mut self,
        target: &IREexpr,
        index: &IREexpr,
        bounds_check: Option<&BoundsCheck>,
    ) {
        match bounds_check {
            Some(BoundsCheck::SliceLen { by_ref }) => {
                let index_c = self.capture_expr_code(index);
                if self.addressing_index {
                    self.write("&");
                }
                self.emit_slice_data_index(target, &index_c, *by_ref);
            }
            Some(BoundsCheck::StringLen) => {
                if self.addressing_index {
                    self.write("&");
                }
                self.generate_expr(target);
                self.write("->data[");
                self.generate_expr(index);
                self.write("]");
            }
            _ => {
                if self.addressing_index {
                    self.write("&");
                }
                self.generate_expr(target);
                self.write("[");
                self.generate_expr(index);
                self.write("]");
            }
        }
    }

    /// Zero-initialize a moved capture in the parent scope after ownership transfers to the thread.
    fn zero_value_for_type(&self, ty: &Type) -> String {
        let resolved = resolve_type_alias(ty, &self.type_aliases);
        match resolved {
            Type::Struct(_)
            | Type::Enum(_)
            | Type::Sender { .. }
            | Type::Receiver { .. }
            | Type::JoinHandle { .. }
            | Type::File
            | Type::Allocator
            | Type::Endpoint { .. } => {
                format!("({}){{0}}", type_to_c_impl(&resolved))
            }
            _ => "0".to_string(),
        }
    }

    /// C initializer that clears a moved-out enum variant payload field so a later scrutinee drop
    /// cannot double-free the same owned value.
    fn zero_value_for_scrutinee_payload(&self, ty: &Type) -> String {
        let resolved = resolve_type_alias(ty, &self.type_aliases);
        match resolved {
            Type::String | Type::Box { .. } | Type::Vec { .. } => "NULL".to_string(),
            Type::Struct(_)
            | Type::Enum(_)
            | Type::Generic { .. }
            | Type::Tuple { .. }
            | Type::Sender { .. }
            | Type::Receiver { .. }
            | Type::File
            | Type::Allocator
            | Type::Endpoint { .. }
            | Type::JoinHandle { .. } => {
                format!("({}){{0}}", self.type_to_c(&resolved))
            }
            _ => "0".to_string(),
        }
    }

    fn emit_match_scrutinee_payload_moved_out(
        &mut self,
        match_var_name: &str,
        variant_idx: usize,
        field_name: &str,
        ty: &Type,
    ) {
        if !self.needs_drop(ty) {
            return;
        }
        self.write_indent();
        self.writeln(&format!(
            "{}.data.variant_{}.{} = {};",
            match_var_name,
            variant_idx,
            field_name,
            self.zero_value_for_scrutinee_payload(ty)
        ));
    }

    fn emit_match_scrutinee_whole_enum_moved_out(
        &mut self,
        match_var_name: &str,
        enum_decl: &EnumDecl,
        type_params: &[Type],
    ) {
        let substitutions: std::collections::HashMap<String, &Type> = enum_decl
            .generics
            .iter()
            .zip(type_params.iter())
            .map(|(param, ty)| (param.name.clone(), ty))
            .collect();

        self.write_indent();
        self.writeln(&format!("switch ({}.tag) {{", match_var_name));
        self.indent_level += 1;
        for (variant_idx, variant) in enum_decl.variants.iter().enumerate() {
            let has_payloads = !variant.payload_types.is_empty() || variant.named_fields.is_some();
            if !has_payloads {
                continue;
            }
            self.write_indent();
            self.writeln(&format!("case {variant_idx}:"));
            self.indent_level += 1;
            if let Some(named_fields) = &variant.named_fields {
                for (field_name, field_ty) in named_fields {
                    let concrete_ty = if !substitutions.is_empty() {
                        substitute_type_params(field_ty, &substitutions)
                    } else {
                        field_ty.clone()
                    };
                    self.emit_match_scrutinee_payload_moved_out(
                        match_var_name,
                        variant_idx,
                        field_name,
                        &concrete_ty,
                    );
                }
            } else {
                for (arg_idx, payload_ty) in variant.payload_types.iter().enumerate() {
                    let concrete_ty = if !substitutions.is_empty() {
                        substitute_type_params(payload_ty, &substitutions)
                    } else {
                        payload_ty.clone()
                    };
                    self.emit_match_scrutinee_payload_moved_out(
                        match_var_name,
                        variant_idx,
                        &format!("arg{arg_idx}"),
                        &concrete_ty,
                    );
                }
            }
            self.write_indent();
            self.writeln("break;");
            self.indent_level -= 1;
        }
        self.indent_level -= 1;
        self.write_indent();
        self.writeln("}");
    }

    fn op_to_c(&self, op: BinOp) -> String {
        match op {
            BinOp::Add => "+".to_string(),
            BinOp::Sub => "-".to_string(),
            BinOp::Mul => "*".to_string(),
            BinOp::Div => "/".to_string(),
            BinOp::Rem => "%".to_string(),
            BinOp::Lt => "<".to_string(),
            BinOp::Gt => ">".to_string(),
            BinOp::Le => "<=".to_string(),
            BinOp::Ge => ">=".to_string(),
            BinOp::Eq => "==".to_string(),
            BinOp::Ne => "!=".to_string(),
            BinOp::And => "&&".to_string(),
            BinOp::BitAnd => "&".to_string(),
            BinOp::BitOr => "|".to_string(),
            BinOp::BitXor => "^".to_string(),
            BinOp::ShiftLeft => "<<".to_string(),
            BinOp::ShiftRight => ">>".to_string(),
            BinOp::Or => "||".to_string(),
        }
    }

    fn write(&mut self, s: &str) {
        self.output.push_str(s);
    }

    fn writeln(&mut self, s: &str) {
        self.write(s);
        self.output.push('\n');
    }

    fn write_indent(&mut self) {
        for _ in 0..self.indent_level {
            self.write("    ");
        }
    }

    fn generate_enum_type(&mut self, enum_decl: &EnumDecl) {
        // Generate enum type with tag and union: typedef struct EnumName { int tag; union { ... } data; } EnumName;
        let enum_name = &enum_decl.name;
        self.write(&format!("typedef struct {} {{\n", enum_name));
        self.indent_level += 1;
        self.write_indent();
        self.writeln("int tag;");
        self.write_indent();
        self.write("union {\n");
        self.indent_level += 1;

        // Generate union members for each variant with payloads
        for (variant_idx, variant) in enum_decl.variants.iter().enumerate() {
            // Check if variant has payloads (either tuple or struct variant)
            let has_payloads = !variant.payload_types.is_empty() || variant.named_fields.is_some();
            if has_payloads {
                self.write_indent();
                self.write("struct {\n");
                self.indent_level += 1;

                // Handle struct variants with named fields
                if let Some(ref named_fields) = variant.named_fields {
                    for (field_name, field_ty) in named_fields {
                        self.write_indent();
                        self.write(&format!("{} {};", self.type_to_c(field_ty), field_name));
                        self.writeln("");
                    }
                } else {
                    // Handle tuple variants with positional arguments
                    for (i, payload_ty) in variant.payload_types.iter().enumerate() {
                        self.write_indent();
                        self.write(&format!("{} arg{};", self.type_to_c(payload_ty), i));
                        self.writeln("");
                    }
                }

                self.indent_level -= 1;
                self.write_indent();
                self.write(&format!("}} variant_{};\n", variant_idx));
            }
        }

        self.indent_level -= 1;
        self.write_indent();
        self.writeln("} data;");
        self.indent_level -= 1;
        self.writeln(&format!("}} {};", enum_name));
        self.writeln("");
        self.generated_types.insert(enum_name.clone(), true);
    }

    fn assert_c_extern_linkage(extern_block: &ExternBlock) {
        if extern_block.linkage != "C" {
            panic!("compiler bug: non-C extern reached cgen");
        }
    }

    fn generate_extern_block(&mut self, extern_block: &ExternBlock) {
        Self::assert_c_extern_linkage(extern_block);

        // Generate extern function prototypes
        for extern_fn in &extern_block.functions {
            // Store parameter types for later use in call generation
            let param_types: Vec<Type> = extern_fn.params.iter().map(|p| p.ty.clone()).collect();
            self.extern_functions
                .insert(extern_fn.name.clone(), param_types);

            let return_type = extern_fn
                .return_type
                .as_ref()
                .map(|t| self.type_to_c(t))
                .unwrap_or_else(|| "void".to_string());

            self.write(&format!("extern {} {}(", return_type, extern_fn.name));

            // Generate parameters
            if extern_fn.params.is_empty() && !extern_fn.variadic {
                self.write("void");
            } else {
                for (i, param) in extern_fn.params.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    // Special handling: for &int (immutable) parameters in extern functions, generate as int
                    // (POSIX functions expect int by value, not int*)
                    // &mut int should remain int* (for output parameters like addrlen in accept)
                    let param_c_type = match &param.ty {
                        Type::Ref { inner, mutable } => {
                            if let Type::Int = **inner {
                                if *mutable {
                                    // &mut int -> int* (for output parameters)
                                    "int*".to_string()
                                } else {
                                    // &int -> int (for POSIX compatibility, input parameters)
                                    "int".to_string()
                                }
                            } else {
                                self.type_to_c(&param.ty)
                            }
                        }
                        Type::Array { inner, size, .. } => {
                            let base_type = self.type_to_c(inner);
                            format!("{} {}[{}]", base_type, param.name, size)
                        }
                        _ => self.type_to_c(&param.ty),
                    };
                    // Special handling for array types: C syntax is "int arr[3]" not "int[3] arr"
                    match &param.ty {
                        Type::Array { inner, size, .. } => {
                            let base_type = self.type_to_c(inner);
                            self.write(&format!("{} {}[{}]", base_type, param.name, size));
                        }
                        Type::Fn { .. } => {
                            self.write(&fn_type_to_c_decl(&param.ty, &param.name));
                        }
                        _ => {
                            self.write(&format!("{} {}", param_c_type, param.name));
                        }
                    }
                }
                if extern_fn.variadic {
                    if !extern_fn.params.is_empty() {
                        self.write(", ");
                    }
                    self.write("...");
                }
            }

            self.writeln(");");
        }
        self.writeln("");
    }

    /// Find the monomorphized enum name and type parameters for a given base enum name
    fn find_enum_instantiation(
        &self,
        enum_type: &str,
        enum_decl: Option<&EnumDecl>,
    ) -> (String, Vec<Type>) {
        if let Some((mono_name, params)) = self
            .generic_instantiations
            .iter()
            .find(|(_, (base, _))| base == enum_type)
            .map(|(mono, (_, params))| (mono.clone(), params.clone()))
        {
            (mono_name, params)
        } else if let Some(decl) = enum_decl {
            if !decl.generics.is_empty() {
                panic!("compiler bug: generic enum '{enum_type}' match missing instantiation")
            }
            (enum_type.to_string(), Vec::new())
        } else {
            (enum_type.to_string(), Vec::new())
        }
    }

    /// Resolve monomorphized `Option<T>` for match/for from scrutinee type or call shape.
    fn resolve_option_match_instantiation(
        &self,
        scrutinee: &IREexpr,
        enum_type: &str,
        enum_decl: Option<&EnumDecl>,
        scrutinee_type: Option<&Type>,
    ) -> (String, Vec<Type>) {
        let preferred = match scrutinee {
            IREexpr::Var(name) => self
                .lookup_var_type(name)
                .or_else(|| scrutinee_type.cloned()),
            _ => scrutinee_type.cloned(),
        };
        if let Some(ty) = preferred.as_ref() {
            let peeled = match ty {
                Type::Ref { inner, .. } => inner.as_ref(),
                other => other,
            };
            if let Type::Generic { name, params } = peeled
                && name == enum_type
                && !params.is_empty()
            {
                return (mangle_type_name(name, params), params.clone());
            }
        }
        if let IREexpr::Call {
            callee,
            args,
            return_type,
            ..
        } = scrutinee
        {
            if crate::tc::is_option_producing_builtin(callee) {
                if let Some(Type::Generic { name, params }) = return_type
                    && name == "Option"
                    && params.len() == 1
                {
                    let mono_name = mangle_type_name("Option", params);
                    return (mono_name, params.clone());
                }
                if callee == "String::from_utf8" {
                    let elem = Type::String;
                    let mono_name = mangle_type_name("Option", std::slice::from_ref(&elem));
                    return (mono_name, vec![elem]);
                }
                if callee == "String::get" {
                    let elem = Type::U8;
                    let mono_name = mangle_type_name("Option", std::slice::from_ref(&elem));
                    return (mono_name, vec![elem]);
                }
                if let Some(first_arg) = args.first() {
                    if callee == "Slice::get_ref" {
                        if let Some(elem_type) = self.slice_elem_type_from_arg(first_arg) {
                            let ref_elem = Type::Ref {
                                inner: Box::new(elem_type.clone()),
                                mutable: false,
                            };
                            let mono_name =
                                mangle_type_name("Option", std::slice::from_ref(&ref_elem));
                            return (mono_name, vec![ref_elem]);
                        }
                    } else if let Some(elem_type) = self.vec_elem_type_from_arg(first_arg) {
                        if callee == "Vec::get_ref" {
                            let ref_elem = Type::Ref {
                                inner: Box::new(elem_type.clone()),
                                mutable: false,
                            };
                            let mono_name =
                                mangle_type_name("Option", std::slice::from_ref(&ref_elem));
                            return (mono_name, vec![ref_elem]);
                        }
                        let mono_name =
                            mangle_type_name("Option", std::slice::from_ref(&elem_type));
                        return (mono_name, vec![elem_type]);
                    }
                }
            } else if let Some(Type::Generic { name, params }) = return_type
                && name == enum_type
                && !params.is_empty()
            {
                let mono_name = mangle_type_name(name, params);
                return (mono_name, params.clone());
            }
        }
        self.find_enum_instantiation(enum_type, enum_decl)
    }

    fn value_pattern_condition(&self, pattern: &IRPattern, var: &str) -> Option<String> {
        match pattern {
            IRPattern::Lit { lit } => Some(match lit {
                PatLit::Int(n) => format!("{var} == {n}"),
                PatLit::Bool(true) => var.to_string(),
                PatLit::Bool(false) => format!("!{var}"),
                PatLit::Str(text) => {
                    let escaped = Self::escape_c_string_literal_content(text);
                    let len = text.len();
                    if len == 0 {
                        format!("({var} && {var}->len == 0)")
                    } else {
                        format!(
                            "({var} && {var}->len == {len} && memcmp({var}->data, \"{escaped}\", {len}) == 0)"
                        )
                    }
                }
            }),
            IRPattern::Range { lo, hi } => Some(format!("{var} >= {lo} && {var} <= {hi}")),
            IRPattern::Struct { fields, .. } => {
                let parts: Vec<String> = fields
                    .iter()
                    .filter_map(|(field, pat)| {
                        self.value_pattern_condition(pat, &format!("{var}.{field}"))
                    })
                    .collect();
                if parts.is_empty() {
                    None
                } else {
                    Some(parts.join(" && "))
                }
            }
            IRPattern::At { pattern, .. } => self.value_pattern_condition(pattern, var),
            IRPattern::Or { alts } => {
                let parts: Vec<String> = alts
                    .iter()
                    .filter_map(|alt| self.value_pattern_condition(alt, var))
                    .collect();
                if parts.is_empty() {
                    None
                } else {
                    Some(format!("({})", parts.join(" || ")))
                }
            }
            IRPattern::Binding { .. }
            | IRPattern::Wildcard
            | IRPattern::Rest
            | IRPattern::Variant { .. } => None,
        }
    }

    fn emit_value_pattern_bindings(
        &mut self,
        pattern: &IRPattern,
        ty: &Type,
        src: &str,
        through_ref: bool,
    ) {
        match pattern {
            IRPattern::Binding { name, .. } => {
                self.emit_pattern_binding(ty, name, src, through_ref);
            }
            IRPattern::At { name, pattern } => {
                self.emit_pattern_binding(ty, name, src, through_ref);
                let next = if through_ref {
                    format!("(*{name})")
                } else {
                    name.clone()
                };
                self.emit_value_pattern_bindings(pattern, ty, &next, through_ref);
            }
            IRPattern::Struct { name, fields, .. } => {
                let decl = self.struct_map.get(name).cloned().unwrap_or_else(|| {
                    panic!("compiler bug: struct pattern '{name}' has no struct declaration")
                });
                let substitutions: HashMap<String, &Type> = match ty {
                    Type::Generic { params, .. } => decl
                        .generics
                        .iter()
                        .zip(params.iter())
                        .map(|(param, ty)| (param.name.clone(), ty))
                        .collect(),
                    _ => HashMap::new(),
                };
                for (field, field_pattern) in fields {
                    let field_ty = decl
                        .fields
                        .iter()
                        .find(|item| item.name == *field)
                        .map(|item| item.ty.clone())
                        .unwrap_or_else(|| {
                            panic!("compiler bug: struct '{name}' has no field '{field}'")
                        });
                    let field_ty = if substitutions.is_empty() {
                        field_ty
                    } else {
                        substitute_type_params(&field_ty, &substitutions)
                    };
                    let field_src = format!("({src}).{field}");
                    self.emit_value_pattern_bindings(
                        field_pattern,
                        &field_ty,
                        &field_src,
                        through_ref,
                    );
                }
            }
            IRPattern::Or { alts } => {
                if let Some(alt) = alts.first() {
                    self.emit_value_pattern_bindings(alt, ty, src, through_ref);
                }
            }
            _ => {}
        }
    }

    /// Bind `src`. Through `&` / `&mut`, a non-copy place is a reborrow (`&src`)
    /// and is not cleared or dropped. An owned non-copy place is moved out.
    fn emit_pattern_binding(&mut self, ty: &Type, name: &str, src: &str, through_ref: bool) {
        if through_ref
            && !crate::tc::type_is_copy(ty, &self.struct_map, &self.enum_map, &self.drop_impls)
        {
            let ref_ty = Type::Ref {
                inner: Box::new(ty.clone()),
                mutable: false,
            };
            self.write_indent();
            self.writeln(&format!("{} {name} = &({src});", self.type_to_c(&ref_ty)));
            self.scope_register_binding(name, &ref_ty);
            if Self::should_silence_unused_binding(name, &ref_ty) {
                self.emit_silence_unused_binding(name);
            }
            return;
        }
        if through_ref {
            let c_ty = self.type_to_c(ty);
            self.write_indent();
            self.writeln(&format!("{c_ty} {name} = {src};"));
            self.scope_register_binding(name, ty);
            if Self::should_silence_unused_binding(name, ty) {
                self.emit_silence_unused_binding(name);
            }
            return;
        }
        self.emit_moved_value_binding(ty, name, src);
    }

    /// Copy `src` into `name`. A type that needs drop is moved: the source place is cleared
    /// so the scrutinee drop does not free it again.
    fn emit_moved_value_binding(&mut self, ty: &Type, name: &str, src: &str) {
        let c_ty = self.type_to_c(ty);
        if matches!(ty, Type::Array { .. }) {
            self.write_indent();
            self.writeln(&format!("{c_ty} {name};"));
            self.write_indent();
            self.writeln(&format!("memcpy(&{name}, &({src}), sizeof({name}));"));
            if self.needs_drop(ty) {
                self.write_indent();
                self.writeln(&format!("memset(&({src}), 0, sizeof({src}));"));
            }
        } else {
            self.write_indent();
            self.writeln(&format!("{c_ty} {name} = {src};"));
            if self.needs_drop(ty) {
                self.write_indent();
                self.writeln(&format!(
                    "{src} = {};",
                    self.zero_value_for_scrutinee_payload(ty)
                ));
            }
        }
        self.scope_register_binding(name, ty);
        if Self::should_silence_unused_binding(name, ty) {
            self.emit_silence_unused_binding(name);
        }
    }

    fn emit_value_match(
        &mut self,
        expr: &IREexpr,
        ty: &Type,
        arms: &[IRMatchArm],
        match_result: Option<(&str, &Type)>,
    ) {
        let match_var_name = format!("match_val_{}", self.match_counter);
        self.match_counter += 1;
        let (place_ty, through_ref) = match ty {
            Type::Ref { inner, .. } => (inner.as_ref().clone(), true),
            other => (other.clone(), false),
        };
        let stored_ty = if through_ref {
            Type::Ref {
                inner: Box::new(place_ty.clone()),
                mutable: false,
            }
        } else {
            place_ty.clone()
        };
        let c_ty = self.type_to_c(&stored_ty);
        self.write_indent();
        self.write(&format!("{c_ty} {match_var_name} = "));
        self.generate_expr(expr);
        self.writeln(";");
        self.mark_moves_in_expr(expr);
        self.scope_register_binding(&match_var_name, &stored_ty);
        let src = if through_ref {
            format!("(*{match_var_name})")
        } else {
            match_var_name.clone()
        };

        let mut opened = false;
        for arm in arms {
            let pattern_cond = self.value_pattern_condition(&arm.pattern, &src);
            let guard_cond = arm.guard.is_some();
            self.write_indent();
            if pattern_cond.is_some() || guard_cond {
                if opened {
                    self.write("else if (");
                } else {
                    self.write("if (");
                }
                if let Some(cond) = &pattern_cond {
                    self.write(cond);
                }
                if let Some(guard) = &arm.guard {
                    if pattern_cond.is_some() {
                        self.write(" && ");
                    }
                    self.generate_expr(guard);
                }
                self.writeln(") {");
            } else if opened {
                self.writeln("else {");
            } else {
                self.writeln("{");
            }
            opened = true;
            self.indent_level += 1;
            self.scope_begin(&[]);
            self.emit_value_pattern_bindings(&arm.pattern, &place_ty, &src, through_ref);
            if let Some((result_var, result_type)) = match_result {
                self.emit_match_arm_result_stmts(&arm.body, result_var, result_type);
            } else {
                self.generate_block(&arm.body);
            }
            self.scope_emit_exit();
            self.indent_level -= 1;
            self.write_indent();
            self.writeln("}");
        }
    }

    fn generate_match_block(
        &mut self,
        expr: &IREexpr,
        enum_type: &str,
        arms: &[IRMatchArm],
        match_result: Option<(&str, &Type)>,
        scrutinee_type: Option<&Type>,
    ) {
        if let Some(ty) = scrutinee_type {
            let peeled = match ty {
                Type::Ref { inner, .. } => inner.as_ref(),
                other => other,
            };
            if crate::types_util::is_value_scrutinee(peeled) {
                self.emit_value_match(expr, ty, arms, match_result);
                return;
            }
        }
        // Generate match as a block (for statement context)
        // Get monomorphized enum name if it's generic
        let enum_decl = self.enum_map.get(enum_type).cloned();

        // Try to extract type from the expression if it's a Call to Vec::pop or Vec::get
        let (monomorphized_enum_name, type_params) = self.resolve_option_match_instantiation(
            expr,
            enum_type,
            enum_decl.as_ref(),
            scrutinee_type,
        );

        // Ensure Option<int> is in generic_instantiations
        // Note: The actual type generation happens at file scope before functions
        if !type_params.is_empty() && enum_type == "Option" {
            let key = monomorphized_enum_name.clone();
            if !self.generated_types.contains_key(&key) {
                // Add to generic_instantiations if not already there
                if !self.generic_instantiations.contains_key(&key) {
                    self.generic_instantiations
                        .insert(key.clone(), ("Option".to_string(), type_params.clone()));
                }
            }
        }

        let match_var_name = format!("match_val_{}", self.match_counter);
        self.match_counter += 1;

        self.write_indent();
        if self.match_scrutinee_needs_deref(expr, scrutinee_type) {
            self.write(&format!(
                "{} {} = *",
                monomorphized_enum_name, match_var_name
            ));
            self.generate_expr(expr);
            self.writeln(";");
        } else {
            self.write(&format!(
                "{} {} = ",
                monomorphized_enum_name, match_var_name
            ));
            self.generate_expr(expr);
            self.writeln(";");
            self.mark_moves_in_expr(expr);
        }
        self.match_in_switch += 1;
        self.match_scrutinee_stack.push(match_var_name.clone());
        self.write_indent();
        self.write(&format!("switch ({}.tag) {{", match_var_name));
        self.writeln("");
        self.emit_grouped_match_arms(
            arms,
            &monomorphized_enum_name,
            enum_decl.as_ref(),
            &match_var_name,
            &type_params,
            match_result,
        );
        self.write_indent();
        self.writeln("}");
        self.match_scrutinee_stack.pop();
        self.match_in_switch = self.match_in_switch.saturating_sub(1);
    }

    fn pattern_variant_name(pattern: &IRPattern) -> Option<&str> {
        match pattern {
            IRPattern::Variant { variant, .. } => Some(variant.as_str()),
            IRPattern::At { pattern, .. } => Self::pattern_variant_name(pattern),
            _ => None,
        }
    }

    fn emit_grouped_match_arms(
        &mut self,
        arms: &[IRMatchArm],
        enum_type: &str,
        enum_decl: Option<&EnumDecl>,
        match_var_name: &str,
        type_params: &[Type],
        match_result: Option<(&str, &Type)>,
    ) {
        self.indent_level += 1;
        let mut grouped: std::collections::BTreeMap<usize, Vec<&IRMatchArm>> =
            std::collections::BTreeMap::new();
        for arm in arms {
            let variant_idx = match Self::pattern_variant_name(&arm.pattern) {
                Some(variant) => enum_decl
                    .and_then(|e| e.variants.iter().position(|v| v.name == variant))
                    .unwrap_or(0),
                None => usize::MAX,
            };
            grouped.entry(variant_idx).or_default().push(arm);
        }
        for (variant_idx, group_arms) in grouped {
            if variant_idx == usize::MAX {
                for arm in group_arms {
                    self.generate_match_arm(
                        arm,
                        enum_type,
                        enum_decl,
                        match_var_name,
                        type_params,
                        match_result,
                    );
                }
            } else {
                let variant_name = enum_decl
                    .and_then(|e| e.variants.get(variant_idx))
                    .map(|v| v.name.as_str())
                    .unwrap_or("variant");
                self.write_indent();
                self.writeln(&format!("case {}: {{ // {}", variant_idx, variant_name));
                self.indent_level += 1;
                self.scope_begin(&[]);
                if let Some(first_arm) = group_arms.first() {
                    self.generate_match_arm_payload_bindings(
                        first_arm,
                        enum_decl,
                        enum_type,
                        match_var_name,
                        type_params,
                        variant_idx,
                    );
                }
                for arm in group_arms {
                    if let Some(ref guard) = arm.guard {
                        self.write_indent();
                        self.write("if (");
                        self.generate_expr(guard);
                        self.writeln(") {");
                        self.indent_level += 1;
                        if let Some((result_var, result_type)) = match_result {
                            self.emit_match_arm_result_stmts(&arm.body, result_var, result_type);
                        } else {
                            self.generate_block(&arm.body);
                        }
                        self.scope_emit_top_frame_drops();
                        self.write_indent();
                        self.writeln("break;");
                        self.indent_level -= 1;
                        self.write_indent();
                        self.writeln("}");
                    } else {
                        self.generate_match_arm_body(arm, match_result, false);
                    }
                }
                self.scope_emit_exit();
                self.write_indent();
                self.writeln("break;");
                self.indent_level -= 1;
                self.write_indent();
                self.writeln("}");
            }
        }
        self.indent_level -= 1;
    }

    fn generate_match_arm_payload_bindings(
        &mut self,
        arm: &IRMatchArm,
        enum_decl: Option<&EnumDecl>,
        enum_type: &str,
        match_var_name: &str,
        type_params: &[Type],
        variant_idx: usize,
    ) {
        let (at_name, pattern) = match &arm.pattern {
            IRPattern::At { name, pattern } => (Some(name.as_str()), pattern.as_ref()),
            other => (None, other),
        };
        if let Some(name) = at_name {
            self.write_indent();
            self.writeln(&format!("{enum_type} {name} = {match_var_name};"));
            self.scope_register_binding(name, &Type::Enum(enum_type.to_string()));
        }
        let IRPattern::Variant {
            variant,
            sub_patterns,
            named_fields,
            ..
        } = pattern
        else {
            return;
        };

        if let Some(variant_decl) =
            enum_decl.and_then(|e| e.variants.iter().find(|v| v.name == *variant))
        {
            // Build substitution map for generic parameters
            let substitutions: std::collections::HashMap<String, &Type> = enum_decl
                .map(|e| {
                    e.generics
                        .iter()
                        .zip(type_params.iter())
                        .map(|(name, ty)| (name.name.clone(), ty))
                        .collect()
                })
                .unwrap_or_default();

            // Handle struct variants with named fields
            if let Some(named_fields_patterns) = named_fields {
                if let Some(variant_named_fields) = &variant_decl.named_fields {
                    for (field_name, field_pattern) in named_fields_patterns {
                        // Find the field type in the variant declaration
                        if let Some((_, field_ty)) = variant_named_fields
                            .iter()
                            .find(|(name, _)| name == field_name)
                        {
                            // Substitute generic parameters in field type
                            let concrete_field_ty = if !substitutions.is_empty() {
                                substitute_type_params(field_ty, &substitutions)
                            } else {
                                field_ty.clone()
                            };

                            match field_pattern {
                                IRPattern::Binding { name, ty } => {
                                    let bound_ty = ty.clone().unwrap_or(concrete_field_ty.clone());
                                    let src = format!(
                                        "{}.data.variant_{}.{}",
                                        match_var_name, variant_idx, field_name
                                    );
                                    self.write_indent();
                                    self.emit_ref_or_moved_field_binding(
                                        &bound_ty,
                                        &concrete_field_ty,
                                        name,
                                        &src,
                                        (match_var_name, variant_idx, field_name),
                                    );
                                }
                                IRPattern::Wildcard => {
                                    // Wildcard - don't extract, field is ignored
                                }
                                IRPattern::Variant { .. } => {
                                    // Nested constructors are specialized to nested IR Match in lowering.
                                }
                                IRPattern::Lit { .. }
                                | IRPattern::Range { .. }
                                | IRPattern::Or { .. }
                                | IRPattern::Struct { .. }
                                | IRPattern::At { .. }
                                | IRPattern::Rest => {}
                            }
                        }
                    }
                }
            } else {
                // Handle tuple variants with positional patterns
                for (i, payload_ty) in variant_decl.payload_types.iter().enumerate() {
                    // Substitute generic parameters in payload type
                    let concrete_payload_ty = if !substitutions.is_empty() {
                        substitute_type_params(payload_ty, &substitutions)
                    } else {
                        // If no substitutions available, use the payload type as-is
                        // (This shouldn't happen in well-typed code, but handle gracefully)
                        payload_ty.clone()
                    };

                    // Get the pattern for this payload position (or use wildcard)
                    if let Some(sub_pattern) = sub_patterns.get(i) {
                        match sub_pattern {
                            IRPattern::Binding { name, ty } => {
                                let bound_ty = ty.clone().unwrap_or(concrete_payload_ty.clone());
                                let payload_field = format!("arg{i}");
                                let src = format!(
                                    "{}.data.variant_{}.{}",
                                    match_var_name, variant_idx, payload_field
                                );
                                self.write_indent();
                                self.emit_ref_or_moved_field_binding(
                                    &bound_ty,
                                    &concrete_payload_ty,
                                    name,
                                    &src,
                                    (match_var_name, variant_idx, &payload_field),
                                );
                            }
                            IRPattern::Wildcard => {
                                // Wildcard - don't extract, payload is ignored
                            }
                            IRPattern::Variant { .. } => {
                                // Nested constructors are specialized to nested IR Match in lowering.
                            }
                            IRPattern::Lit { .. }
                            | IRPattern::Range { .. }
                            | IRPattern::Or { .. }
                            | IRPattern::Struct { .. }
                            | IRPattern::At { .. }
                            | IRPattern::Rest => {}
                        }
                    } else {
                        // No pattern specified - treat as wildcard
                    }
                }
            }
        }
    }

    /// Bind a match field. A reference binding whose field is an owned value
    /// points at that field and does not move it. A field that is already a
    /// reference is the pointer. Anything else is copied or moved out.
    fn emit_ref_or_moved_field_binding(
        &mut self,
        bound_ty: &Type,
        field_ty: &Type,
        name: &str,
        src: &str,
        moved_out: (&str, usize, &str),
    ) {
        if matches!(bound_ty, Type::Ref { .. }) && !matches!(field_ty, Type::Ref { .. }) {
            self.write(&format!("{} {name} = &({src});", self.type_to_c(bound_ty)));
            self.writeln("");
            self.scope_register_binding(name, bound_ty);
        } else if matches!(bound_ty, Type::Ref { .. }) {
            self.write(&format!("{} {name} = {src};", self.type_to_c(bound_ty)));
            self.writeln("");
            self.scope_register_binding(name, bound_ty);
        } else {
            self.emit_binding_from_c_expr(bound_ty, name, src);
            self.writeln("");
            self.emit_match_scrutinee_payload_moved_out(
                moved_out.0,
                moved_out.1,
                moved_out.2,
                bound_ty,
            );
            self.scope_register_binding(name, bound_ty);
        }
        if Self::should_silence_unused_binding(name, bound_ty) {
            self.emit_silence_unused_binding(name);
        }
    }

    /// Bind `dest` from a C rvalue. Arrays are not assignable, so copy with memcpy.
    fn emit_binding_from_c_expr(&mut self, ty: &Type, dest: &str, src: &str) {
        let c_ty = self.type_to_c(ty);
        if matches!(ty, Type::Array { .. }) {
            self.write(&format!("{c_ty} {dest};"));
            self.writeln("");
            self.write_indent();
            self.write(&format!("memcpy(&{dest}, &({src}), sizeof({dest}));"));
        } else {
            self.write(&format!("{c_ty} {dest} = {src};"));
        }
    }

    fn generate_match_arm(
        &mut self,
        arm: &IRMatchArm,
        enum_type: &str,
        _enum_decl: Option<&EnumDecl>,
        match_var_name: &str,
        type_params: &[Type],
        match_result: Option<(&str, &Type)>,
    ) {
        match &arm.pattern {
            IRPattern::Wildcard => {
                self.write_indent();
                self.writeln("default: {");
                self.indent_level += 1;
                self.generate_match_arm_body(arm, match_result, true);
                self.indent_level -= 1;
                self.write_indent();
                self.writeln("}");
            }
            IRPattern::Binding { name, .. } => {
                self.write_indent();
                self.writeln(&format!("default: {{ // binding {}", name));
                self.indent_level += 1;
                self.write_indent();
                self.writeln(&format!("{} {} = {};", enum_type, name, match_var_name));
                if let Some(enum_decl) = _enum_decl {
                    self.emit_match_scrutinee_whole_enum_moved_out(
                        match_var_name,
                        enum_decl,
                        type_params,
                    );
                }
                self.generate_match_arm_body(arm, match_result, true);
                self.indent_level -= 1;
                self.write_indent();
                self.writeln("}");
            }
            IRPattern::Variant { .. } => {
                // Variant arms are handled by grouped generation in generate_match_block.
            }
            IRPattern::Lit { .. }
            | IRPattern::Range { .. }
            | IRPattern::Or { .. }
            | IRPattern::Struct { .. }
            | IRPattern::At { .. }
            | IRPattern::Rest => {}
        }
    }

    fn generate_match_arm_body(
        &mut self,
        arm: &IRMatchArm,
        match_result: Option<(&str, &Type)>,
        trailing_break: bool,
    ) {
        if let Some(ref guard) = arm.guard {
            self.write_indent();
            self.write("if (");
            self.generate_expr(guard);
            self.writeln(") {");
            self.indent_level += 1;
            if let Some((result_var, result_type)) = match_result {
                self.emit_match_arm_result_stmts(&arm.body, result_var, result_type);
            } else {
                self.generate_block(&arm.body);
            }
            self.indent_level -= 1;
            self.write_indent();
            self.writeln("}");
            if trailing_break {
                self.write_indent();
                self.writeln("break;");
            }
        } else if let Some((result_var, result_type)) = match_result {
            self.emit_match_arm_result_stmts(&arm.body, result_var, result_type);
            if trailing_break {
                self.write_indent();
                self.writeln("break;");
            }
        } else {
            self.generate_block(&arm.body);
            if trailing_break {
                self.write_indent();
                self.writeln("break;");
            }
        }
    }

    fn emit_match_arm_result_stmts(
        &mut self,
        body: &IRBlock,
        result_var: &str,
        result_type: &Type,
    ) {
        self.emit_match_arm_result_from_stmts(&body.statements, 0, result_var, result_type);
    }

    /// Lower rvalue-match arm statements into assignments / control flow inside a `switch`.
    /// Function exits (`return`) use `emit_function_return` like every other return site.
    fn emit_match_arm_result_from_stmts(
        &mut self,
        stmts: &[IRStmt],
        idx: usize,
        result_var: &str,
        result_type: &Type,
    ) {
        if idx >= stmts.len() {
            return;
        }

        let is_last = idx + 1 == stmts.len();
        match &stmts[idx] {
            IRStmt::Return(ret) => {
                self.emit_function_return(ret);
            }
            IRStmt::Break => {
                self.emit_break_statement();
            }
            IRStmt::Continue => {
                self.emit_continue_statement();
            }
            IRStmt::Expr(expr) => {
                if is_last {
                    if let IREexpr::Match {
                        expr: match_expr,
                        enum_type,
                        arms,
                        scrutinee_type,
                        ..
                    } = expr
                    {
                        self.generate_match_block(
                            match_expr,
                            enum_type,
                            arms,
                            Some((result_var, result_type)),
                            scrutinee_type.as_ref(),
                        );
                    } else {
                        self.write_indent();
                        self.write(&format!("{} = ", result_var));
                        self.generate_expr_with_type(expr, Some(result_type));
                        self.writeln(";");
                        self.mark_moves_in_expr(expr);
                    }
                } else {
                    self.generate_stmt(&stmts[idx]);
                    self.emit_match_arm_result_from_stmts(stmts, idx + 1, result_var, result_type);
                }
            }
            IRStmt::If(ir_if) => {
                if is_last {
                    self.write_indent();
                    self.write("if (");
                    self.generate_expr(&ir_if.cond);
                    self.writeln(") {");
                    self.indent_level += 1;
                    self.emit_match_arm_result_from_stmts(
                        &ir_if.then_block.statements,
                        0,
                        result_var,
                        result_type,
                    );
                    self.indent_level -= 1;
                    if let Some(else_blk) = &ir_if.else_block {
                        self.write_indent();
                        self.writeln("} else {");
                        self.indent_level += 1;
                        self.emit_match_arm_result_from_stmts(
                            &else_blk.statements,
                            0,
                            result_var,
                            result_type,
                        );
                        self.indent_level -= 1;
                    }
                    self.write_indent();
                    self.writeln("}");
                } else {
                    self.generate_stmt(&stmts[idx]);
                    self.emit_match_arm_result_from_stmts(stmts, idx + 1, result_var, result_type);
                }
            }
            IRStmt::UnsafeBlock(unsafe_blk) => {
                self.write_indent();
                self.writeln("{");
                self.indent_level += 1;
                if is_last {
                    self.emit_match_arm_result_from_stmts(
                        &unsafe_blk.body.statements,
                        0,
                        result_var,
                        result_type,
                    );
                } else {
                    self.generate_block(&unsafe_blk.body);
                    self.emit_match_arm_result_from_stmts(stmts, idx + 1, result_var, result_type);
                }
                self.indent_level -= 1;
                self.write_indent();
                self.writeln("}");
            }
            _ => {
                self.generate_stmt(&stmts[idx]);
                self.emit_match_arm_result_from_stmts(stmts, idx + 1, result_var, result_type);
            }
        }
    }
}

struct ReferencedTypes {
    arrays: HashMap<String, Type>,
    slices: HashSet<String>,
    tuples: HashMap<String, Vec<Type>>,
    vecs: HashSet<String>,
    generics: HashMap<String, (String, Vec<Type>)>,
}

fn note_referenced_type(ty: &Type, refs: &mut ReferencedTypes) {
    collect_array_from_type(ty, &mut refs.arrays);
    collect_slice_types_from_type(ty, &mut refs.slices);
    collect_tuple_types_from_type(ty, &mut refs.tuples);
    collect_vec_types_from_type(ty, &mut refs.vecs);
    collect_generic_from_type(ty, &mut refs.generics);
}

fn walk_referenced_block(block: &IRBlock, refs: &mut ReferencedTypes) {
    for stmt in &block.statements {
        walk_referenced_stmt(stmt, refs);
    }
}

fn collect_referenced_types(program: &IRProgram) -> ReferencedTypes {
    let mut refs = ReferencedTypes {
        arrays: HashMap::new(),
        slices: HashSet::new(),
        tuples: HashMap::new(),
        vecs: HashSet::new(),
        generics: HashMap::new(),
    };
    for function in &program.functions {
        if let Some(ret_ty) = &function.return_type {
            note_referenced_type(ret_ty, &mut refs);
        }
        for param in &function.params {
            note_referenced_type(&param.ty, &mut refs);
        }
        for block in &function.blocks {
            walk_referenced_block(block, &mut refs);
        }
    }
    for struct_decl in &program.structs {
        for field in &struct_decl.fields {
            note_referenced_type(&field.ty, &mut refs);
        }
    }
    visit_enum_payload_types(program, &mut |ty| note_referenced_type(ty, &mut refs));
    refs
}

fn walk_referenced_stmt(stmt: &IRStmt, refs: &mut ReferencedTypes) {
    match stmt {
        IRStmt::Let(let_stmt) => {
            note_referenced_type(&let_stmt.ty, refs);
            if let Some(init) = &let_stmt.init {
                walk_referenced_expr(init, refs);
            }
        }
        IRStmt::Return(ret) => {
            if let Some(value) = &ret.value {
                walk_referenced_expr(value, refs);
            }
        }
        IRStmt::Break | IRStmt::Continue => {}
        IRStmt::Expr(expr) | IRStmt::Defer(expr) => walk_referenced_expr(expr, refs),
        IRStmt::If(ir_if) => {
            walk_referenced_expr(&ir_if.cond, refs);
            walk_referenced_block(&ir_if.then_block, refs);
            if let Some(else_block) = &ir_if.else_block {
                walk_referenced_block(else_block, refs);
            }
        }
        IRStmt::While(ir_while) => {
            walk_referenced_expr(&ir_while.cond, refs);
            walk_referenced_block(&ir_while.body, refs);
            if let Some(step) = &ir_while.step {
                walk_referenced_block(step, refs);
            }
        }
        IRStmt::Spawn(spawn) => walk_referenced_block(&spawn.body, refs),
        IRStmt::Select(sel) => {
            for arm in &sel.recv_arms {
                walk_referenced_expr(&arm.channel, refs);
                note_referenced_type(&arm.elem_type, refs);
                let option_ty = Type::Generic {
                    name: "Option".to_string(),
                    params: vec![arm.elem_type.clone()],
                };
                collect_generic_from_type(&option_ty, &mut refs.generics);
                walk_referenced_block(&arm.body, refs);
            }
            if let Some(body) = &sel.default_body {
                walk_referenced_block(body, refs);
            }
            if let Some(ms) = &sel.timeout_ms {
                walk_referenced_expr(ms, refs);
            }
            if let Some(body) = &sel.timeout_body {
                walk_referenced_block(body, refs);
            }
        }
        IRStmt::UnsafeBlock(unsafe_block) => walk_referenced_block(&unsafe_block.body, refs),
        IRStmt::Scope(scope) => walk_referenced_block(&scope.body, refs),
    }
}

fn walk_referenced_expr(expr: &IREexpr, refs: &mut ReferencedTypes) {
    match expr {
        IREexpr::Lit(_)
        | IREexpr::BoolLiteral(_)
        | IREexpr::FloatLiteral(_)
        | IREexpr::IntLimit { .. }
        | IREexpr::Var(_)
        | IREexpr::StringLit(_) => {}
        IREexpr::BinOp { left, right, .. } => {
            walk_referenced_expr(left, refs);
            walk_referenced_expr(right, refs);
        }
        IREexpr::UnOp { operand, .. } | IREexpr::AddressOf { inner: operand, .. } => {
            walk_referenced_expr(operand, refs);
        }
        IREexpr::Send {
            channel,
            value,
            value_type,
        } => {
            walk_referenced_expr(channel, refs);
            walk_referenced_expr(value, refs);
            collect_generic_from_type(value_type, &mut refs.generics);
            let send_result = Type::Generic {
                name: "SendResult".to_string(),
                params: vec![value_type.clone()],
            };
            collect_generic_from_type(&send_result, &mut refs.generics);
        }
        IREexpr::Recv {
            elem_type, channel, ..
        } => {
            walk_referenced_expr(channel, refs);
            note_referenced_type(elem_type, refs);
            let endpoint_result = matches!(
                elem_type,
                Type::Tuple { elements }
                    if elements.len() == 2 && matches!(elements[1], Type::Endpoint { .. })
            );
            if !endpoint_result {
                let option_ty = Type::Generic {
                    name: "Option".to_string(),
                    params: vec![elem_type.clone()],
                };
                collect_generic_from_type(&option_ty, &mut refs.generics);
            }
        }
        IREexpr::Spawn { body, .. } => walk_referenced_block(body, refs),
        IREexpr::StructLit { fields, .. } => {
            for field in fields {
                walk_referenced_expr(&field.value, refs);
            }
        }
        IREexpr::FieldAccess { base, .. } => walk_referenced_expr(base, refs),
        IREexpr::EnumLit {
            args,
            named_fields,
            ty,
            ..
        } => {
            collect_generic_from_type(ty, &mut refs.generics);
            for arg in args {
                walk_referenced_expr(arg, refs);
            }
            if let Some(fields) = named_fields {
                for (_, value) in fields {
                    walk_referenced_expr(value, refs);
                }
            }
        }
        IREexpr::Match { expr, arms, .. } => {
            walk_referenced_expr(expr, refs);
            if let IREexpr::Call {
                callee,
                return_type,
                ..
            } = expr.as_ref()
                && crate::tc::is_option_producing_builtin(callee)
            {
                if let Some(Type::Generic { name, params }) = return_type
                    && name == "Option"
                    && params.len() == 1
                {
                    let option_key = mangle_type_name("Option", params);
                    refs.generics
                        .entry(option_key)
                        .or_insert_with(|| ("Option".to_string(), params.clone()));
                } else if callee == "Vec::get_ref" || callee == "Slice::get_ref" {
                    if let Some(elem_type) =
                        refs.generics
                            .iter()
                            .find_map(|(_mono_name, (base, params))| {
                                if (base == "Vec" || base == "Slice") && params.len() == 1 {
                                    Some(params[0].clone())
                                } else {
                                    None
                                }
                            })
                    {
                        let ref_elem = Type::Ref {
                            inner: Box::new(elem_type),
                            mutable: false,
                        };
                        let option_key =
                            mangle_type_name("Option", std::slice::from_ref(&ref_elem));
                        refs.generics
                            .entry(option_key)
                            .or_insert_with(|| ("Option".to_string(), vec![ref_elem]));
                    }
                } else if callee == "String::get" {
                    let option_key = mangle_type_name("Option", std::slice::from_ref(&Type::U8));
                    refs.generics
                        .entry(option_key)
                        .or_insert_with(|| ("Option".to_string(), vec![Type::U8]));
                } else if callee == "String::from_utf8" {
                    let option_key =
                        mangle_type_name("Option", std::slice::from_ref(&Type::String));
                    refs.generics
                        .entry(option_key)
                        .or_insert_with(|| ("Option".to_string(), vec![Type::String]));
                } else if let Some(elem_type) =
                    refs.generics
                        .iter()
                        .find_map(|(_mono_name, (base, params))| {
                            if base == "Vec" && params.len() == 1 {
                                Some(params[0].clone())
                            } else {
                                None
                            }
                        })
                {
                    let option_key = mangle_type_name("Option", std::slice::from_ref(&elem_type));
                    refs.generics
                        .entry(option_key)
                        .or_insert_with(|| ("Option".to_string(), vec![elem_type]));
                }
            }
            for arm in arms {
                walk_referenced_block(&arm.body, refs);
            }
        }
        IREexpr::Call {
            return_type, args, ..
        } => {
            if let Some(ret_ty) = return_type {
                note_referenced_type(ret_ty, refs);
            }
            for arg in args {
                walk_referenced_expr(arg, refs);
            }
        }
        IREexpr::TupleLit {
            elem_types,
            elements,
            ..
        } => {
            for ty in elem_types {
                note_referenced_type(ty, refs);
            }
            for elem in elements {
                walk_referenced_expr(elem, refs);
            }
        }
        IREexpr::FnLiteral(lit) => {
            for param in &lit.params {
                note_referenced_type(&param.ty, refs);
            }
            if let Some(ret) = &lit.return_type {
                note_referenced_type(ret, refs);
            }
            walk_referenced_block(&lit.body, refs);
        }
        IREexpr::ArrayLiteral {
            elements, repeat, ..
        } => {
            for elem in elements {
                walk_referenced_expr(elem, refs);
            }
            if let Some((value_expr, _)) = repeat {
                walk_referenced_expr(value_expr, refs);
            }
        }
        IREexpr::Index {
            target,
            index,
            target_type,
        } => {
            walk_referenced_expr(target, refs);
            walk_referenced_expr(index, refs);
            if let Some(ty) = target_type {
                note_referenced_type(ty, refs);
            }
        }
        IREexpr::Cast { expr, .. } => walk_referenced_expr(expr, refs),
        IREexpr::Assign { value, .. } => walk_referenced_expr(value, refs),
        IREexpr::AssignIndex {
            target,
            index,
            value,
            ..
        } => {
            walk_referenced_expr(target, refs);
            walk_referenced_expr(index, refs);
            walk_referenced_expr(value, refs);
        }
        IREexpr::AssignField { target, value, .. } => {
            walk_referenced_expr(target, refs);
            walk_referenced_expr(value, refs);
        }
    }
}

fn visit_enum_payload_types(program: &IRProgram, visit: &mut impl FnMut(&Type)) {
    for e in &program.enums {
        for variant in &e.variants {
            for ty in &variant.payload_types {
                visit(ty);
            }
            if let Some(fields) = &variant.named_fields {
                for (_, ty) in fields {
                    visit(ty);
                }
            }
        }
    }
}

fn collect_array_typedefs(program: &IRProgram) -> Vec<(String, Type)> {
    let mut out: Vec<(String, Type)> = collect_referenced_types(program)
        .arrays
        .into_iter()
        .collect();
    out.sort_by(|a, b| a.0.cmp(&b.0));
    out
}

fn collect_array_from_type(ty: &Type, arrays: &mut HashMap<String, Type>) {
    match ty {
        Type::Array { inner, size, .. } => {
            arrays.insert(array_type_name(inner, *size), ty.clone());
            collect_array_from_type(inner, arrays);
        }
        Type::Ref { inner, .. }
        | Type::RawPtr { inner }
        | Type::Box { inner }
        | Type::Slice { inner } => collect_array_from_type(inner, arrays),
        Type::Vec { elem_type }
        | Type::Channel { elem_type }
        | Type::Sender { elem_type }
        | Type::Receiver { elem_type } => collect_array_from_type(elem_type, arrays),
        Type::Tuple { elements } => {
            for elem in elements {
                collect_array_from_type(elem, arrays);
            }
        }
        Type::Generic { params, .. } => {
            for param in params {
                collect_array_from_type(param, arrays);
            }
        }
        Type::Fn {
            params,
            return_type,
        } => {
            for param in params {
                collect_array_from_type(param, arrays);
            }
            collect_array_from_type(return_type, arrays);
        }
        _ => {}
    }
}

fn collect_slice_types_impl(
    program: &IRProgram,
    slice_types: &mut std::collections::HashSet<String>,
) {
    slice_types.extend(collect_referenced_types(program).slices);
}

fn collect_slice_types_from_type(ty: &Type, slice_types: &mut std::collections::HashSet<String>) {
    match ty {
        Type::Void
        | Type::Int
        | Type::Bool
        | Type::F32
        | Type::F64
        | Type::I8
        | Type::I16
        | Type::I32
        | Type::I64
        | Type::U8
        | Type::U16
        | Type::U32
        | Type::U64
        | Type::UInt
        | Type::String
        | Type::Str
        | Type::JoinHandle { .. }
        | Type::File
        | Type::Allocator
        | Type::Endpoint { .. } => {}
        Type::Slice { inner } => {
            let slice_type_name = format!(
                "ion_slice_{}",
                mangle_type_name(&type_to_c_impl(inner), &[])
            );
            slice_types.insert(slice_type_name);
            collect_slice_types_from_type(inner, slice_types);
        }
        Type::Array { inner, .. } => {
            collect_slice_types_from_type(inner, slice_types);
        }
        Type::Ref { inner, .. } => {
            collect_slice_types_from_type(inner, slice_types);
        }
        Type::RawPtr { inner } => {
            collect_slice_types_from_type(inner, slice_types);
        }
        Type::Box { inner } => {
            collect_slice_types_from_type(inner, slice_types);
        }
        Type::Vec { elem_type } => {
            collect_slice_types_from_type(elem_type, slice_types);
        }
        Type::Channel { elem_type } => {
            collect_slice_types_from_type(elem_type, slice_types);
        }
        Type::Sender { elem_type } => {
            collect_slice_types_from_type(elem_type, slice_types);
        }
        Type::Receiver { elem_type } => {
            collect_slice_types_from_type(elem_type, slice_types);
        }
        Type::Tuple { elements } => {
            for elem in elements {
                collect_slice_types_from_type(elem, slice_types);
            }
        }
        Type::Fn {
            params,
            return_type,
        } => {
            for param in params {
                collect_slice_types_from_type(param, slice_types);
            }
            collect_slice_types_from_type(return_type, slice_types);
        }
        Type::Struct(_) | Type::Enum(_) | Type::Generic { .. } => {
            // User-defined types - would need to check fields/variants
        }
    }
}

fn collect_tuple_types_impl(
    program: &IRProgram,
    tuple_types: &mut std::collections::HashMap<String, Vec<Type>>,
) {
    tuple_types.extend(collect_referenced_types(program).tuples);
}

fn collect_tuple_types_from_type(
    ty: &Type,
    tuple_types: &mut std::collections::HashMap<String, Vec<Type>>,
) {
    if let Type::Tuple { elements } = ty {
        tuple_types.insert(tuple_type_name(elements), elements.clone());
        for elem in elements {
            collect_tuple_types_from_type(elem, tuple_types);
        }
        return;
    }
    match ty {
        Type::Ref { inner, .. } => collect_tuple_types_from_type(inner, tuple_types),
        Type::RawPtr { inner } => collect_tuple_types_from_type(inner, tuple_types),
        Type::Box { inner } => collect_tuple_types_from_type(inner, tuple_types),
        Type::Vec { elem_type } => collect_tuple_types_from_type(elem_type, tuple_types),
        Type::Channel { elem_type } => collect_tuple_types_from_type(elem_type, tuple_types),
        Type::Sender { elem_type } => collect_tuple_types_from_type(elem_type, tuple_types),
        Type::Receiver { elem_type } => collect_tuple_types_from_type(elem_type, tuple_types),
        Type::Array { inner, .. } => collect_tuple_types_from_type(inner, tuple_types),
        Type::Slice { inner } => collect_tuple_types_from_type(inner, tuple_types),
        Type::Generic { params, .. } => {
            for param in params {
                collect_tuple_types_from_type(param, tuple_types);
            }
        }
        Type::Fn {
            params,
            return_type,
        } => {
            for param in params {
                collect_tuple_types_from_type(param, tuple_types);
            }
            collect_tuple_types_from_type(return_type, tuple_types);
        }
        _ => {}
    }
}

fn collect_vec_types_impl(program: &IRProgram, vec_types: &mut std::collections::HashSet<String>) {
    vec_types.extend(collect_referenced_types(program).vecs);
}

fn collect_vec_types_from_type(ty: &Type, vec_types: &mut std::collections::HashSet<String>) {
    match ty {
        Type::Vec { elem_type } => {
            let vec_name = mangle_type_name("Vec", std::slice::from_ref(elem_type));
            vec_types.insert(vec_name);
            // Recursively collect nested Vec types
            collect_vec_types_from_type(elem_type, vec_types);
        }
        Type::Generic { name, params } if name == "Vec" && params.len() == 1 => {
            let vec_name = mangle_type_name("Vec", params);
            vec_types.insert(vec_name);
            // Recursively collect nested Vec types
            for param in params {
                collect_vec_types_from_type(param, vec_types);
            }
        }
        Type::Ref { inner, .. } => {
            collect_vec_types_from_type(inner, vec_types);
        }
        Type::Box { inner } => {
            collect_vec_types_from_type(inner, vec_types);
        }
        Type::Channel { elem_type } => {
            collect_vec_types_from_type(elem_type, vec_types);
        }
        Type::Array { inner, .. } | Type::Slice { inner } => {
            collect_vec_types_from_type(inner, vec_types);
        }
        Type::Tuple { elements } => {
            for elem in elements {
                collect_vec_types_from_type(elem, vec_types);
            }
        }
        _ => {}
    }
}

impl Codegen {
    fn insert_spawn_fn_literal_forward_decls(&mut self, insert_at: usize) {
        if self.spawn_forward_decls.is_empty() && self.fn_literal_forward_decls.is_empty() {
            return;
        }
        let mut forward_decls = self.spawn_forward_decls.clone();
        forward_decls.push_str(&self.fn_literal_forward_decls);
        forward_decls.push('\n');
        self.output.insert_str(insert_at, &forward_decls);
    }

    fn ensure_option_template(&mut self) {
        if self.enum_map.contains_key("Option") {
            return;
        }
        let option_template = synthetic_option_enum(Span {
            start: 0,
            end: 0,
            line: 0,
            column: 0,
        });
        self.enum_map.insert("Option".to_string(), option_template);
    }

    fn partition_generic_instantiations(
        &self,
        instantiations: &[&(String, Vec<Type>)],
    ) -> GenericInstantiationGroups {
        let mut struct_instantiations = Vec::new();
        let mut early_enum_instantiations = Vec::new();
        let mut late_enum_instantiations = Vec::new();
        for &(base_name, params) in instantiations {
            if let Some(decl) = self.struct_map.get(base_name) {
                struct_instantiations.push((decl.clone(), params.clone()));
            } else if let Some(decl) = self.enum_map.get(base_name) {
                if params_complete_with_struct_forwards(params) {
                    early_enum_instantiations.push((decl.clone(), params.clone()));
                } else {
                    late_enum_instantiations.push((decl.clone(), params.clone()));
                }
            }
        }
        (
            struct_instantiations,
            early_enum_instantiations,
            late_enum_instantiations,
        )
    }

    fn emit_vec_primitive_options(&mut self, program: &IRProgram) {
        let mut vec_types = std::collections::HashSet::new();
        collect_vec_types_impl(program, &mut vec_types);
        for vec_type_name in &vec_types {
            let Some(elem_type_str) = vec_type_name.strip_prefix("Vec_") else {
                continue;
            };
            let option_type_name = format!("Option_{elem_type_str}");
            if self.generated_types.contains_key(&option_type_name) {
                continue;
            }
            let Some(elem_type) = primitive_type_from_mangled_name(elem_type_str) else {
                continue;
            };
            let option_decl = synthetic_option_enum(Span {
                start: 0,
                end: 0,
                line: 0,
                column: 0,
            });
            self.generate_monomorphized_enum(&option_decl, std::slice::from_ref(&elem_type));
            self.generated_types.insert(option_type_name.clone(), true);
            self.generic_instantiations
                .insert(option_type_name, ("Option".to_string(), vec![elem_type]));
        }
    }

    fn emit_non_generic_type_forwards(&mut self, program: &IRProgram) {
        let mut any = false;
        for s in &program.structs {
            if s.generics.is_empty() {
                self.writeln(&format!("typedef struct {} {};", s.name, s.name));
                any = true;
            }
        }
        for e in &program.enums {
            if e.generics.is_empty() {
                self.writeln(&format!("typedef struct {} {};", e.name, e.name));
                any = true;
            }
        }
        if any {
            self.writeln("");
        }
    }

    fn emit_ready_array_typedefs(&mut self, arrays: &[(String, Type)]) {
        let none: HashSet<String> = HashSet::new();
        loop {
            let mut progressed = false;
            for (name, ty) in arrays {
                if self.generated_types.contains_key(name) {
                    continue;
                }
                let Type::Array { inner, size, .. } = ty else {
                    continue;
                };
                if !type_ready_for_by_value(inner, &self.generated_types, &none) {
                    continue;
                }
                if let Type::Array {
                    inner: nested,
                    size: nested_size,
                    ..
                } = inner.as_ref()
                    && !self
                        .generated_types
                        .contains_key(&array_type_name(nested, *nested_size))
                {
                    continue;
                }
                self.writeln(&format!(
                    "typedef {} {}[{}];",
                    self.type_to_c(inner),
                    name,
                    size
                ));
                self.generated_types.insert(name.clone(), true);
                progressed = true;
            }
            if !progressed {
                break;
            }
        }
    }

    fn emit_vec_slice_typedefs(&mut self, program: &IRProgram) {
        let mut vec_types = std::collections::HashSet::new();
        collect_vec_types_impl(program, &mut vec_types);
        for vec_type_name in &vec_types {
            self.generate_vec_struct(vec_type_name);
        }

        let mut slice_types = std::collections::HashSet::new();
        collect_slice_types_impl(program, &mut slice_types);
        for slice_type_name in &slice_types {
            self.generate_slice_struct(slice_type_name);
        }
    }

    fn emit_tuple_typedefs(&mut self, program: &IRProgram) {
        let mut tuple_types: HashMap<String, Vec<Type>> = HashMap::new();
        collect_tuple_types_impl(program, &mut tuple_types);
        let mut emitting = HashSet::new();
        let names: Vec<String> = tuple_types.keys().cloned().collect();
        for name in names {
            self.emit_tuple_typedef_ordered(&name, &tuple_types, &mut emitting);
        }
    }

    fn emit_tuple_typedef_ordered(
        &mut self,
        name: &str,
        all: &HashMap<String, Vec<Type>>,
        emitting: &mut HashSet<String>,
    ) {
        if !emitting.insert(name.to_string()) {
            return;
        }
        let Some(elements) = all.get(name) else {
            return;
        };
        for elem in elements {
            self.emit_nested_tuple_typedefs(elem, all, emitting);
        }
        self.generate_tuple_struct(name, elements);
    }

    fn emit_nested_tuple_typedefs(
        &mut self,
        ty: &Type,
        all: &HashMap<String, Vec<Type>>,
        emitting: &mut HashSet<String>,
    ) {
        match ty {
            Type::Tuple { elements } => {
                let inner_name = tuple_type_name(elements);
                self.emit_tuple_typedef_ordered(&inner_name, all, emitting);
            }
            Type::Ref { inner, .. }
            | Type::RawPtr { inner }
            | Type::Box { inner }
            | Type::Array { inner, .. }
            | Type::Slice { inner } => {
                self.emit_nested_tuple_typedefs(inner, all, emitting);
            }
            Type::Vec { elem_type }
            | Type::Channel { elem_type }
            | Type::Sender { elem_type }
            | Type::Receiver { elem_type } => {
                self.emit_nested_tuple_typedefs(elem_type, all, emitting);
            }
            Type::Generic { params, .. } => {
                for p in params {
                    self.emit_nested_tuple_typedefs(p, all, emitting);
                }
            }
            Type::Fn {
                params,
                return_type,
            } => {
                for p in params {
                    self.emit_nested_tuple_typedefs(p, all, emitting);
                }
                self.emit_nested_tuple_typedefs(return_type, all, emitting);
            }
            _ => {}
        }
    }

    fn generate_vec_struct(&mut self, vec_type_name: &str) {
        // Layout matches ion_vec_t: allocator header, then pointer, length, capacity, element size.
        self.write(&format!("typedef struct {} {{\n", vec_type_name));
        self.indent_level += 1;
        self.write_indent();
        self.writeln("ion_alloc_t alloc;");
        self.write_indent();
        self.writeln("void* data;");
        self.write_indent();
        self.writeln("size_t len;");
        self.write_indent();
        self.writeln("size_t capacity;");
        self.write_indent();
        self.writeln("size_t elem_size;");
        self.indent_level -= 1;
        self.writeln(&format!("}} {};", vec_type_name));
        self.writeln("");
    }

    fn generate_slice_struct(&mut self, slice_type_name: &str) {
        // Generate slice struct definition: typedef struct ion_slice_T { T* data; int len; } ion_slice_T;
        // Extract element type from slice type name (ion_slice_int -> int)
        let elem_type_name = slice_type_name.strip_prefix("ion_slice_").unwrap_or("int");

        // Map element type name back to C type
        let elem_c_type = match elem_type_name {
            "int" => "int",
            "u8" | "uint8_t" => "uint8_t",
            _ => elem_type_name, // Assume it's already a valid C type
        };

        self.write(&format!("typedef struct {} {{\n", slice_type_name));
        self.indent_level += 1;
        self.write_indent();
        self.writeln(&format!("{}* data;", elem_c_type));
        self.write_indent();
        self.writeln("int len;");
        self.indent_level -= 1;
        self.writeln(&format!("}} {};", slice_type_name));
        self.writeln("");
    }

    fn generate_tuple_struct(&mut self, tuple_name: &str, elements: &[Type]) {
        if self.generated_types.contains_key(tuple_name) {
            return;
        }
        self.write(&format!("typedef struct {} {{\n", tuple_name));
        self.indent_level += 1;
        for (i, elem) in elements.iter().enumerate() {
            self.write_indent();
            self.writeln(&format!(
                "{};",
                self.c_struct_field_decl(&format!("f{i}"), elem)
            ));
        }
        self.indent_level -= 1;
        self.writeln(&format!("}} {};", tuple_name));
        self.writeln("");
        self.generated_types.insert(tuple_name.to_string(), true);
    }

    fn generate_monomorphized_struct(&mut self, decl: &StructDecl, params: &[Type]) {
        // Generate monomorphized struct: Point<int> -> Point_int
        let monomorphized_name = mangle_type_name(&decl.name, params);

        // Create substitution map: generic param name -> concrete type
        let mut substitutions: HashMap<String, &Type> = HashMap::new();
        for (i, gen_param) in decl.generics.iter().enumerate() {
            if i < params.len() {
                substitutions.insert(gen_param.name.clone(), &params[i]);
            }
        }

        self.write(&format!("typedef struct {} {{\n", monomorphized_name));
        self.indent_level += 1;
        for field in &decl.fields {
            self.write_indent();
            let field_ty = substitute_type_params(&field.ty, &substitutions);
            self.writeln(&format!(
                "{};",
                self.c_struct_field_decl(&field.name, &field_ty)
            ));
        }
        self.indent_level -= 1;
        self.writeln(&format!("}} {};", monomorphized_name));
        self.writeln("");
    }

    fn emit_struct_typedef(&mut self, decl: &StructDecl) {
        self.write(&format!("typedef struct {} {{\n", decl.name));
        self.indent_level += 1;
        for field in &decl.fields {
            self.write_indent();
            self.writeln(&format!(
                "{};",
                self.c_struct_field_decl(&field.name, &field.ty)
            ));
        }
        self.indent_level -= 1;
        self.writeln(&format!("}} {};", decl.name));
        self.writeln("");
    }

    fn emit_struct_decl_body(&mut self, decl: &StructDecl) {
        self.emit_struct_typedef(decl);
        self.generated_types.insert(decl.name.clone(), true);
    }

    fn emit_non_generic_struct_bodies(&mut self, program: &IRProgram) {
        let mut pending: Vec<StructDecl> = program
            .structs
            .iter()
            .filter(|s| s.generics.is_empty())
            .cloned()
            .collect();
        let none: HashSet<String> = HashSet::new();
        while !pending.is_empty() {
            let mut rest = Vec::new();
            let mut progressed = false;
            for decl in pending {
                if decl
                    .fields
                    .iter()
                    .all(|field| type_ready_for_by_value(&field.ty, &self.generated_types, &none))
                {
                    self.emit_struct_decl_body(&decl);
                    progressed = true;
                } else {
                    rest.push(decl);
                }
            }
            if !progressed {
                for decl in rest {
                    self.emit_struct_decl_body(&decl);
                }
                break;
            }
            pending = rest;
        }
    }

    fn emit_enum_instantiations_ready_first(&mut self, mut pending: Vec<(EnumDecl, Vec<Type>)>) {
        let user_structs: HashSet<String> = self
            .struct_map
            .iter()
            .filter(|(_, decl)| decl.generics.is_empty())
            .map(|(name, _)| name.clone())
            .collect();
        while !pending.is_empty() {
            let mut rest = Vec::new();
            let mut progressed = false;
            for (decl, params) in pending {
                let key = mangle_type_name(&decl.name, &params);
                if self.generated_types.contains_key(&key) {
                    continue;
                }
                if params.iter().all(|param| {
                    type_ready_for_by_value(param, &self.generated_types, &user_structs)
                }) {
                    self.generate_monomorphized_enum(&decl, &params);
                    self.generated_types.insert(key, true);
                    progressed = true;
                } else {
                    rest.push((decl, params));
                }
            }
            if !progressed {
                for (decl, params) in rest {
                    let key = mangle_type_name(&decl.name, &params);
                    if !self.generated_types.contains_key(&key) {
                        self.generate_monomorphized_enum(&decl, &params);
                        self.generated_types.insert(key, true);
                    }
                }
                break;
            }
            pending = rest;
        }
    }

    fn emit_monomorphized_enum_forwards(&mut self) {
        let mut names: Vec<String> = self
            .generic_instantiations
            .iter()
            .filter_map(|(key, (base_name, _))| {
                if self.enum_map.contains_key(base_name) || base_name == "Option" {
                    Some(key.clone())
                } else {
                    None
                }
            })
            .collect();
        names.sort();
        names.dedup();
        for name in &names {
            self.writeln(&format!("typedef struct {name} {name};"));
        }
        if !names.is_empty() {
            self.writeln("");
        }
    }

    fn generate_monomorphized_enum(&mut self, decl: &EnumDecl, params: &[Type]) {
        // Generate monomorphized enum: Option<int> -> Option_int
        let monomorphized_name = mangle_type_name(&decl.name, params);

        // Create substitution map: generic param name -> concrete type
        let mut substitutions: HashMap<String, &Type> = HashMap::new();
        for (i, gen_param) in decl.generics.iter().enumerate() {
            if i < params.len() {
                substitutions.insert(gen_param.name.clone(), &params[i]);
            }
        }

        self.write(&format!("typedef struct {} {{\n", monomorphized_name));
        self.indent_level += 1;
        self.write_indent();
        self.writeln("int tag;");
        self.write_indent();
        self.write("union {\n");
        self.indent_level += 1;

        // Generate union members for each variant with payloads
        for (variant_idx, variant) in decl.variants.iter().enumerate() {
            let has_payloads = !variant.payload_types.is_empty() || variant.named_fields.is_some();
            if !has_payloads {
                continue;
            }
            self.write_indent();
            self.write("struct {\n");
            self.indent_level += 1;
            if let Some(named_fields) = &variant.named_fields {
                for (field_name, field_ty) in named_fields {
                    self.write_indent();
                    let substituted_ty = substitute_type_params(field_ty, &substitutions);
                    self.write(&format!(
                        "{} {};",
                        self.type_to_c(&substituted_ty),
                        field_name
                    ));
                    self.writeln("");
                }
            } else {
                for (i, payload_ty) in variant.payload_types.iter().enumerate() {
                    self.write_indent();
                    let substituted_ty = substitute_type_params(payload_ty, &substitutions);
                    self.write(&format!("{} arg{};", self.type_to_c(&substituted_ty), i));
                    self.writeln("");
                }
            }
            self.indent_level -= 1;
            self.write_indent();
            self.write(&format!("}} variant_{};\n", variant_idx));
        }

        self.indent_level -= 1;
        self.write_indent();
        self.writeln("} data;");
        self.indent_level -= 1;
        self.writeln(&format!("}} {};", monomorphized_name));
        self.writeln("");
    }
}

/// True when monomorphized enum payloads can be emitted using only struct forward
/// declarations (pointers / primitives). False when a payload embeds a struct/enum
/// by value and needs the complete typedef first (`Option<Todo>` vs `Option<Box<Node>>`).
fn params_complete_with_struct_forwards(params: &[Type]) -> bool {
    params.iter().all(type_complete_with_struct_forwards)
}

fn type_complete_with_struct_forwards(ty: &Type) -> bool {
    match ty {
        Type::Void
        | Type::Int
        | Type::Bool
        | Type::F32
        | Type::F64
        | Type::I8
        | Type::I16
        | Type::I32
        | Type::I64
        | Type::U8
        | Type::U16
        | Type::U32
        | Type::U64
        | Type::UInt
        | Type::String
        | Type::Str
        | Type::JoinHandle { .. }
        | Type::File
        | Type::Allocator
        | Type::Endpoint { .. }
        | Type::Fn { .. }
        | Type::Slice { .. } => true,
        Type::Box { .. } | Type::Vec { .. } | Type::RawPtr { .. } | Type::Ref { .. } => true,
        Type::Channel { elem_type } | Type::Sender { elem_type } | Type::Receiver { elem_type } => {
            type_complete_with_struct_forwards(elem_type)
        }
        Type::Array { inner, .. } => type_complete_with_struct_forwards(inner),
        Type::Tuple { .. } => false,
        Type::Struct(_) | Type::Enum(_) | Type::Generic { .. } => false,
    }
}

fn type_ready_for_by_value(
    ty: &Type,
    generated: &HashMap<String, bool>,
    user_structs: &HashSet<String>,
) -> bool {
    match ty {
        Type::Void
        | Type::Int
        | Type::Bool
        | Type::F32
        | Type::F64
        | Type::I8
        | Type::I16
        | Type::I32
        | Type::I64
        | Type::U8
        | Type::U16
        | Type::U32
        | Type::U64
        | Type::UInt
        | Type::String
        | Type::Str
        | Type::JoinHandle { .. }
        | Type::File
        | Type::Allocator
        | Type::Endpoint { .. }
        | Type::Fn { .. }
        | Type::Slice { .. }
        | Type::Box { .. }
        | Type::Vec { .. }
        | Type::RawPtr { .. }
        | Type::Ref { .. } => true,
        Type::Channel { elem_type } | Type::Sender { elem_type } | Type::Receiver { elem_type } => {
            type_ready_for_by_value(elem_type, generated, user_structs)
        }
        Type::Array { inner, .. } => match inner.as_ref() {
            Type::Array {
                inner: nested,
                size,
                ..
            } => {
                generated.contains_key(&array_type_name(nested, *size))
                    && type_ready_for_by_value(inner, generated, user_structs)
            }
            Type::Tuple { elements } => {
                generated.contains_key(&tuple_type_name(elements))
                    && type_ready_for_by_value(inner, generated, user_structs)
            }
            _ => type_ready_for_by_value(inner, generated, user_structs),
        },
        Type::Tuple { elements } => {
            generated.contains_key(&tuple_type_name(elements))
                && elements
                    .iter()
                    .all(|elem| type_ready_for_by_value(elem, generated, user_structs))
        }
        Type::Struct(name) | Type::Enum(name) => {
            generated.contains_key(name) || user_structs.contains(name)
        }
        Type::Generic { name, params } => {
            let mangled = mangle_type_name(name, params);
            generated.contains_key(&mangled) || (params.is_empty() && user_structs.contains(name))
        }
    }
}

fn collect_generic_instantiations(
    program: &IRProgram,
    instantiations: &mut std::collections::HashMap<String, (String, Vec<Type>)>,
) {
    instantiations.extend(collect_referenced_types(program).generics);
    close_generic_instantiations(program, instantiations);

    let known = known_type_names(program);
    instantiations.retain(|_, (_, params)| params_are_bound(params, &known));
}

fn close_generic_instantiations(
    program: &IRProgram,
    instantiations: &mut std::collections::HashMap<String, (String, Vec<Type>)>,
) {
    loop {
        let current: Vec<(String, Vec<Type>)> = instantiations.values().cloned().collect();
        let before = instantiations.len();
        for (base_name, params) in current {
            if !type_params_are_concrete(&params) {
                continue;
            }
            if let Some(decl) = program.structs.iter().find(|s| s.name == base_name)
                && !decl.generics.is_empty()
            {
                let owned: Vec<(String, Type)> = decl
                    .generics
                    .iter()
                    .zip(params.iter())
                    .map(|(g, t)| (g.name.clone(), t.clone()))
                    .collect();
                let subst: HashMap<String, &Type> =
                    owned.iter().map(|(n, t)| (n.clone(), t)).collect();
                for field in &decl.fields {
                    collect_generic_from_type(
                        &substitute_type_params(&field.ty, &subst),
                        instantiations,
                    );
                }
            }
            if let Some(decl) = program.enums.iter().find(|e| e.name == base_name)
                && !decl.generics.is_empty()
            {
                let owned: Vec<(String, Type)> = decl
                    .generics
                    .iter()
                    .zip(params.iter())
                    .map(|(g, t)| (g.name.clone(), t.clone()))
                    .collect();
                let subst: HashMap<String, &Type> =
                    owned.iter().map(|(n, t)| (n.clone(), t)).collect();
                for variant in &decl.variants {
                    for ty in &variant.payload_types {
                        collect_generic_from_type(
                            &substitute_type_params(ty, &subst),
                            instantiations,
                        );
                    }
                    if let Some(fields) = &variant.named_fields {
                        for (_, ty) in fields {
                            collect_generic_from_type(
                                &substitute_type_params(ty, &subst),
                                instantiations,
                            );
                        }
                    }
                }
            }
        }
        if instantiations.len() == before {
            break;
        }
    }
}

fn known_type_names(program: &IRProgram) -> HashSet<String> {
    let mut names: HashSet<String> = ["Vec", "Box", "Option", "Result", "SendResult"]
        .into_iter()
        .map(String::from)
        .collect();
    for s in &program.structs {
        names.insert(s.name.clone());
    }
    for e in &program.enums {
        names.insert(e.name.clone());
    }
    names
}

fn params_are_bound(params: &[Type], known: &HashSet<String>) -> bool {
    params.iter().all(|param| type_is_bound(param, known))
}

fn type_is_bound(ty: &Type, known: &HashSet<String>) -> bool {
    match ty {
        Type::Struct(name) | Type::Enum(name) => known.contains(name),
        Type::Generic { name, params } if params.is_empty() => known.contains(name),
        Type::Generic { params, .. } => params.iter().all(|p| type_is_bound(p, known)),
        Type::Vec { elem_type }
        | Type::Box { inner: elem_type }
        | Type::Channel { elem_type }
        | Type::Sender { elem_type }
        | Type::Receiver { elem_type } => type_is_bound(elem_type, known),
        Type::Ref { inner, .. }
        | Type::RawPtr { inner }
        | Type::Array { inner, .. }
        | Type::Slice { inner } => type_is_bound(inner, known),
        Type::Tuple { elements } => elements.iter().all(|e| type_is_bound(e, known)),
        Type::Fn {
            params,
            return_type,
        } => params.iter().all(|p| type_is_bound(p, known)) && type_is_bound(return_type, known),
        _ => true,
    }
}

fn type_params_are_concrete(params: &[Type]) -> bool {
    params.iter().all(|param| match param {
        Type::Struct(_) | Type::Enum(_) => true,
        Type::Generic { params: nested, .. } => type_params_are_concrete(nested),
        Type::Ref { inner, .. }
        | Type::Box { inner }
        | Type::Vec { elem_type: inner }
        | Type::Channel { elem_type: inner }
        | Type::Array { inner, .. }
        | Type::Slice { inner }
        | Type::Sender { elem_type: inner }
        | Type::Receiver { elem_type: inner }
        | Type::RawPtr { inner } => type_params_are_concrete(std::slice::from_ref(inner)),
        Type::Tuple { elements } => type_params_are_concrete(elements),
        Type::Fn {
            params,
            return_type,
        } => {
            type_params_are_concrete(params)
                && type_params_are_concrete(std::slice::from_ref(return_type))
        }
        _ => true,
    })
}

fn collect_generic_from_type(
    ty: &Type,
    instantiations: &mut std::collections::HashMap<String, (String, Vec<Type>)>,
) {
    match ty {
        Type::Generic { name, params } => {
            if type_params_are_concrete(params) {
                if name == "Vec" && params.len() == 1 {
                    let key = mangle_type_name(name, params);
                    instantiations.insert(key, (name.clone(), params.clone()));
                }
                if name != "Box" && name != "Vec" {
                    let key = mangle_type_name(name, params);
                    instantiations.insert(key, (name.clone(), params.clone()));
                }
            }
            for param in params {
                collect_generic_from_type(param, instantiations);
            }
        }
        Type::Ref { inner, .. } => collect_generic_from_type(inner, instantiations),
        Type::Box { inner } => collect_generic_from_type(inner, instantiations),
        Type::Vec { elem_type } => {
            // Also collect Vec types when encountered as Vec { elem_type }
            let key = mangle_type_name("Vec", std::slice::from_ref(elem_type));
            instantiations.insert(key, ("Vec".to_string(), vec![(**elem_type).clone()]));

            // Also collect Option<T> for Vec<T> since Vec::pop and Vec::get return Option<T>
            let elem_type_clone = (**elem_type).clone();
            let option_key = mangle_type_name("Option", std::slice::from_ref(&elem_type_clone));
            instantiations
                .entry(option_key)
                .or_insert_with(|| ("Option".to_string(), vec![elem_type_clone]));

            collect_generic_from_type(elem_type, instantiations);
        }
        Type::Channel { elem_type } => collect_generic_from_type(elem_type, instantiations),
        Type::Array { inner, .. } => collect_generic_from_type(inner, instantiations),
        Type::Slice { inner } => collect_generic_from_type(inner, instantiations),
        Type::Tuple { elements } => {
            for elem in elements {
                collect_generic_from_type(elem, instantiations);
            }
        }
        Type::Fn {
            params,
            return_type,
        } => {
            for param in params {
                collect_generic_from_type(param, instantiations);
            }
            collect_generic_from_type(return_type, instantiations);
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fn_return_type_ret_val_decl() {
        let src = r#"fn make_inc() -> fn(int) -> int {
    return fn(x: int) -> int { return x + 1; };
}
fn main() -> int { return 0; }"#;
        let ir = crate::ir::lower_checked(src);
        let make_inc = ir.functions.iter().find(|f| f.name == "make_inc").unwrap();
        assert!(
            matches!(make_inc.return_type, Some(Type::Fn { .. })),
            "expected fn return type, got {:?}",
            make_inc.return_type
        );
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("int (*ret_val)(int) = 0"),
            "expected fn pointer ret_val decl in:\n{c}"
        );
    }

    #[test]
    fn tuple_return_ret_val_decl() {
        let src = r#"struct Item { done: bool; }
fn pack(items: Vec<Item>) -> (Vec<Item>, int) {
    let mut open: int = 0;
    for item in items {
        if !item.done { open = open + 1; }
    }
    let empty: Vec<Item> = Vec::new();
    return (empty, open);
}
fn main() -> int { return 0; }"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("tuple_Vec_Item_int ret_val = (tuple_Vec_Item_int){0}"),
            "expected tuple ret_val init in:\n{c}"
        );
    }

    #[test]
    fn fn_literal_tuple_return_ret_val_decl() {
        let src = r#"fn main() -> int {
    let get = fn() -> (int, int) { return (1, 2); };
    let t: (int, int) = get();
    return t.0;
}"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("tuple_int_int ret_val = (tuple_int_int){0}"),
            "expected fn literal tuple ret_val init in:\n{c}"
        );
    }

    #[test]
    fn unused_channel_binding_silenced_on_return() {
        let src = r#"fn main() -> int {
    let ch: channel<int>;
    return 0;
}"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("(void)ch;"),
            "expected unused channel silence in:\n{c}"
        );
    }

    #[test]
    fn unused_binding_silenced_on_return() {
        let src = r#"fn main() -> int {
    let x: int = 1;
    return 0;
}"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("(void)x;"),
            "expected unused binding silence in:\n{c}"
        );
    }

    #[test]
    fn enum_literal_uses_compound_init_not_constructor() {
        let src = r#"enum Option {
    Some(int);
    None;
}

fn main() -> int {
    let x: Option = Option::Some(42);
    return 0;
}"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("(Option){ .tag = 0, .data = { .variant_0 = { .arg0 = 42 } } }"),
            "expected compound enum literal in:\n{c}"
        );
        assert!(
            !c.contains("_new("),
            "expected no enum constructor helpers in:\n{c}"
        );
        assert!(
            !c.contains("ION_MAYBE_UNUSED"),
            "expected no ION_MAYBE_UNUSED in:\n{c}"
        );
    }

    #[test]
    fn unused_fn_param_silenced_without_maybe_unused_attribute() {
        let src = r#"fn ignore(x: int) -> int {
    return 0;
}

fn main() -> int {
    return ignore(1);
}"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            !c.contains("ION_MAYBE_UNUSED"),
            "expected no ION_MAYBE_UNUSED in:\n{c}"
        );
        assert!(
            c.contains("(void)x;"),
            "expected unused param silence in:\n{c}"
        );
    }

    #[test]
    fn outer_return_drops_after_inner_if_return() {
        let src = r#"fn main() -> int {
    let (tx, rx): (Sender<int>, Receiver<int>) = channel<int>();
    let mut rx_mut: Receiver<int> = rx;
    if 1 == 1 {
        return 0;
    }
    return 1;
}"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        let outer = c
            .split("ret_val = 1;")
            .nth(1)
            .and_then(|tail| tail.split("goto epilogue;").next())
            .unwrap_or("");
        assert!(
            outer.contains("ion_channel_sender_drop(&(tx))"),
            "expected sender drop in:\n{outer}"
        );
        assert!(
            outer.contains("ion_channel_receiver_drop(&(rx_mut))"),
            "expected outer return to drop sender in:\n{c}"
        );
        assert!(
            outer.contains("ion_channel_receiver_drop(&(rx_mut))"),
            "expected outer return to drop receiver in:\n{c}"
        );
    }

    #[test]
    fn outer_return_drops_after_inner_match_arm_return() {
        let src = r#"enum Opt {
    Some(int);
    None;
}

fn main() -> int {
    let (tx, rx): (Sender<int>, Receiver<int>) = channel<int>();
    let mut rx_mut: Receiver<int> = rx;
    let x: Opt = Opt::Some(1);
    match x {
        Opt::Some(v) => {
            if v == 99 {
                return 0;
            }
        },
        Opt::None => {
            let _pad: int = 0;
        },
    };
    return 1;
}"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        let outer = c
            .split("ret_val = 1;")
            .nth(1)
            .and_then(|tail| tail.split("goto epilogue;").next())
            .unwrap_or("");
        assert!(
            outer.contains("ion_channel_sender_drop(&(tx))"),
            "expected sender drop in:\n{outer}"
        );
        assert!(
            outer.contains("ion_channel_receiver_drop(&(rx_mut))"),
            "expected outer return after match arm to drop sender in:\n{c}"
        );
        assert!(
            outer.contains("ion_channel_receiver_drop(&(rx_mut))"),
            "expected outer return after match arm to drop receiver in:\n{c}"
        );
    }

    #[test]
    fn statement_match_payload_move_neutralizes_scrutinee() {
        let src = r#"enum ReadResult {
    Ok(String);
    Err(int);
}

fn read() -> ReadResult {
    return ReadResult::Ok("hello ion\n");
}

fn main() -> int {
    match read() {
        ReadResult::Ok(raw) => {
            let content: String = raw;
            if content != "hello ion\n" {
                return 1;
            }
            return 80;
        },
        ReadResult::Err(_) => {
            return 2;
        },
    };
}"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        let ok_arm = c
            .split("case 0:")
            .nth(1)
            .and_then(|tail| tail.split("case 1:").next())
            .unwrap_or("");
        assert!(
            ok_arm.contains("match_val_0.data.variant_0.arg0 = NULL"),
            "expected moved-out scrutinee payload to be nulled in:\n{c}"
        );
        assert!(
            ok_arm.contains("if (content) { ion_string_free(content); }"),
            "expected binding drop to free content in:\n{c}"
        );
        assert!(
            !ok_arm.contains("ion_string_free(match_val_0"),
            "expected no enum drop on scrutinee string payload in:\n{c}"
        );
    }

    #[test]
    fn rvalue_match_owned_string_arm_result_marks_payload_moved() {
        let src = r#"enum Option<T> {
    Some(T);
    None;
}

fn unwrap_or_empty(opt: Option<String>) -> String {
    let out: String = match opt {
        Option::Some(s) => {
            s;
        }
        Option::None => {
            String::new();
        }
    };
    return out;
}

fn main() -> int {
    return 0;
}"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        let some_arm = c
            .split("case 0:")
            .nth(1)
            .and_then(|tail| tail.split("case 1:").next())
            .unwrap_or("");
        assert!(
            some_arm.contains("out = s;"),
            "expected arm result assignment in:\n{c}"
        );
        assert!(
            some_arm.contains("match_val_0.data.variant_0.arg0 = NULL"),
            "expected moved-out scrutinee payload to be nulled in:\n{c}"
        );
        assert!(
            !some_arm.contains("ion_string_free(s)"),
            "expected moved payload binding not dropped after rvalue assignment in:\n{some_arm}"
        );
    }

    #[test]
    fn whole_enum_binding_neutralizes_scrutinee_payloads() {
        let src = r#"enum ReadResult {
    Ok(String);
    Err(int);
}

fn read() -> ReadResult {
    return ReadResult::Ok("hello ion\n");
}

fn main() -> int {
    match read() {
        r => {
            return 80;
        },
    };
}"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        let binding_arm = c
            .split("default: { // binding r")
            .nth(1)
            .and_then(|tail| tail.split("goto epilogue;").next())
            .unwrap_or("");
        assert!(
            binding_arm.contains("ReadResult r = match_val_0;"),
            "expected whole-enum binding copy in:\n{c}"
        );
        assert!(
            binding_arm.contains("switch (match_val_0.tag)"),
            "expected scrutinee tag switch for whole-enum move-out in:\n{c}"
        );
        assert!(
            binding_arm.contains("case 0:")
                && binding_arm.contains("match_val_0.data.variant_0.arg0 = NULL"),
            "expected Ok string payload nulled in scrutinee switch in:\n{c}"
        );
        assert!(
            !binding_arm.contains("ion_string_free(match_val_0"),
            "expected no direct scrutinee string drop in:\n{c}"
        );
    }

    #[test]
    fn rvalue_match_divergent_return_unwinds_owned() {
        let src = r#"enum E {
    A(int);
    B(int);
}

fn main() -> int {
    let (tx, rx): (Sender<int>, Receiver<int>) = channel<int>();
    let mut rx_mut: Receiver<int> = rx;
    let s: String = "hi";
    let e: E = E::A(0);
    let n: int = match e {
        E::A(_) => {
            return 1;
        },
        E::B(v) => {
            v;
        },
    };
    return n;
}"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        let divergent_arm = c
            .split("case 0:")
            .nth(1)
            .and_then(|tail| tail.split("break;").next())
            .unwrap_or("");
        assert!(
            divergent_arm.contains("ret_val = 1;"),
            "expected function return lowering in divergent rvalue arm in:\n{c}"
        );
        assert!(
            !divergent_arm.contains("return 1;"),
            "expected goto epilogue, not bare C return, in:\n{c}"
        );
        assert!(
            divergent_arm.contains("ion_channel_sender_drop(&(tx))"),
            "expected channel unwind in divergent rvalue arm in:\n{c}"
        );
        assert!(
            divergent_arm.contains("ion_string_free(s)"),
            "expected String drop in divergent rvalue arm in:\n{c}"
        );
    }

    #[test]
    fn underscore_binding_silenced_once() {
        let src = r#"extern "C" {
    fn write(fd: int, buf: *u8, count: int) -> int;
}
fn log_line() {
    unsafe {
        let _result: int = write(1, "x", 1);
    }
}"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert_eq!(
            c.matches("(void)_result;").count(),
            1,
            "expected single silence for _result in:\n{c}"
        );
    }

    #[test]
    fn vec_push_struct_var_uses_address_of_lvalue() {
        let src = r#"struct Item { done: bool; }
fn main() -> int {
    let mut items: Vec<Item> = Vec::new();
    let item: Item = Item { done: false };
    Vec::push(&mut items, item);
    return 0;
}"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("({ ion_vec_t* _ion_v = (ion_vec_t*)(items); Item _ion_push_val = item; if (!_ion_v || (_ion_v->len >= _ion_v->capacity && ion_vec_reserve_one(_ion_v) != 0)) ion_panic(\"Vec::push failed\"); ((Item*)_ion_v->data)[_ion_v->len] = _ion_push_val; (void)(_ion_v->len++); })"),
            "expected reserve and a typed store of item in:\n{c}"
        );
        assert!(
            !c.contains("ion_vec_push"),
            "push must not call ion_vec_push in:\n{c}"
        );
    }

    #[test]
    fn vec_string_mangles_ion_type_name() {
        let src = r#"enum Option<T> { Some(T); None; }
fn main() -> int {
    let mut names: Vec<String> = Vec::new();
    Vec::push(&mut names, String::from("a"));
    return 0;
}"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("typedef struct Vec_String"),
            "expected Vec_String typedef in:\n{c}"
        );
        assert!(
            !c.contains("Vec_ion_string"),
            "unexpected C-type mangling in:\n{c}"
        );
    }

    #[test]
    fn vec_get_match_uses_per_vector_option_type() {
        let src = r#"enum Option<T> { Some(T); None; }
struct Customer { id: int; name: String; }
fn main() -> int {
    let mut customers: Vec<Customer> = Vec::new();
    let mut names: Vec<String> = Vec::new();
    Vec::push(&mut customers, Customer { id: 1, name: String::from("a") });
    Vec::push(&mut names, String::from("b"));
    match Vec::get(&customers, 0) {
        Option::Some(c) => { if c.id != 1 { return 1; } }
        Option::None => { return 2; }
    };
    match Vec::get(&names, 0) {
        Option::Some(s) => { if String::len(&s) != 1 { return 3; } }
        Option::None => { return 4; }
    };
    return 0;
}"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("Option_Customer match_val_0") && c.contains("Option_String match_val_1"),
            "expected per-vector Option types in:\n{c}"
        );
    }

    #[test]
    fn string_push_str_owned_reads_source_buffer() {
        let src = r#"fn main() -> int {
    let mut s: String = String::from("n=");
    let part: String = String::from("42");
    String::push_str(&mut s, part);
    return 0;
}"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("_ion_push_other") && c.contains("_ion_push_other->data"),
            "expected owned String append via source buffer in:\n{c}"
        );
    }

    #[test]
    fn struct_field_move_neutralizes_source_field() {
        let src = r#"struct Item { done: bool; }
struct Board { items: Vec<Item>; }
fn take(board: Board) -> Vec<Item> {
    return board.items;
}
fn main() -> int {
    let board: Board = Board { items: Vec::new() };
    let taken: Vec<Item> = take(board);
    return 0;
}"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("board.items = NULL"),
            "expected moved-out vec field neutralization in:\n{c}"
        );
    }

    #[test]
    fn call_struct_field_move_defers_field_null() {
        let src = r#"struct Item { done: bool; }
struct Batch { items: Vec<Item>; }
fn take(items: Vec<Item>) -> int { return 0; }
fn main() -> int {
    let batch: Batch = Batch { items: Vec::new() };
    let _x: int = take(batch.items);
    return 0;
}"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("take(batch.items);")
                && c.contains("batch.items = NULL;")
                && c.find("take(batch.items);").unwrap() < c.find("batch.items = NULL;").unwrap(),
            "expected deferred field null after call in:\n{c}"
        );
    }

    #[test]
    fn tuple_vec_int_uses_mangled_tuple_type() {
        let src = r#"struct Item { done: bool; }
fn pair(items: Vec<Item>, n: int) -> (Vec<Item>, int) {
    return (items, n);
}
fn main() -> int {
    let mut items: Vec<Item> = Vec::new();
    let t: (Vec<Item>, int) = pair(items, 1);
    return 0;
}"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("tuple_Vec_Item_int"),
            "expected Vec tuple mangling in:\n{c}"
        );
    }

    #[test]
    fn get_ref_match_arm_binds_borrow_without_dropping_nested_vec() {
        let src = r#"enum Option<T> {
    Some(T);
    None;
}

struct Line {
    price_cents: int;
    qty: int;
}

struct Order {
    customer_id: int;
    lines: Vec<Line>;
}

fn revenue(orders: &Vec<Order>, customer_id: int) -> int {
    let len: int = Vec::len(orders);
    let mut sum: int = 0;
    let mut i: int = 0;
    while i < len {
        match Vec::get_ref(orders, i) {
            Option::Some(order) => {
                if order.customer_id == customer_id {
                    let line_len: int = Vec::len(order.lines);
                    let mut j: int = 0;
                    while j < line_len {
                        match Vec::get_ref(order.lines, j) {
                            Option::Some(line) => {
                                sum = sum + line.price_cents * line.qty;
                            }
                            Option::None => {}
                        };
                        j = j + 1;
                    }
                } else {
                }
            }
            Option::None => {}
        };
        i = i + 1;
    }
    return sum;
}

fn main() -> int {
    let mut orders: Vec<Order> = Vec::new();
    let mut lines: Vec<Line> = Vec::new();
    let line: Line = Line { price_cents: 100, qty: 2 };
    Vec::push(&mut lines, line);
    let order: Order = Order { customer_id: 1, lines: lines };
    Vec::push(&mut orders, order);
    let first: int = revenue(&orders, 1);
    let second: int = revenue(&orders, 1);
    if first != 200 || second != 200 {
        return 1;
    }
    return 0;
}"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("Order* order = match_val_"),
            "expected get_ref arm to bind Order* borrow in:\n{c}"
        );
        assert!(
            !c.contains("Order order = *match_val_"),
            "expected no by-value copy from get_ref borrow in:\n{c}"
        );
        let revenue_fn = c
            .split("int revenue(")
            .nth(1)
            .and_then(|tail| tail.split("int main(").next())
            .unwrap_or("");
        assert!(
            !revenue_fn.contains("ion_vec_free((ion_vec_t*)(order.lines))"),
            "expected no nested Vec drop inside get_ref scan loop in:\n{c}"
        );
    }

    #[test]
    fn multi_file_resolves_module_local_symbols() {
        let mut cg = Codegen::new();
        cg.multi_file_module = Some("fmt".to_string());
        assert_eq!(cg.module_c_symbol("print_int"), "fmt_print_int");
        assert_eq!(cg.module_c_symbol("io::print_int"), "io_print_int");
        cg.extern_functions.insert(
            "write".to_string(),
            vec![
                Type::Int,
                Type::RawPtr {
                    inner: Box::new(Type::U8),
                },
                Type::Int,
            ],
        );
        assert_eq!(cg.module_c_symbol("write"), "write");
    }

    #[test]
    fn box_unwrap_frees_allocation_without_scope_drop() {
        let src = r#"fn main() -> int {
    let boxed: Box<int> = Box::new(42);
    let y: int = Box::unwrap(boxed);
    return y;
}"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("ion_box_free(_box)"),
            "expected unwrap to free the box allocation in:\n{c}"
        );
        assert!(
            !c.contains("(*boxed)"),
            "expected unwrap not to be a bare dereference in:\n{c}"
        );
        assert!(
            !c.contains("ion_box_free(boxed)"),
            "expected moved box not to be freed again at scope exit in:\n{c}"
        );
    }

    #[test]
    fn box_unwrap_unannotated_struct_let_uses_node_not_int() {
        let src = r#"
struct Node {
    a: int;
    b: int;
    c: int;
    d: int;
}

fn main() -> int {
    let boxed = Box::new(Node {
        a: 1,
        b: 2,
        c: 3,
        d: 4,
    });
    let n = Box::unwrap(boxed);
    return n.a + n.b + n.c + n.d;
}
"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("Node* _box"),
            "expected unwrap to use Node* _box in:\n{c}"
        );
        assert!(
            !c.contains("int* _box"),
            "expected unwrap not to lower T as int in:\n{c}"
        );
        assert!(
            !c.contains("int n ="),
            "expected unannotated let n to be Node, not int, in:\n{c}"
        );
    }

    #[test]
    fn enum_unannotated_let_uses_enum_not_int() {
        let src = r#"
enum Flag {
    Off;
    On;
}

fn main() -> int {
    let x = Flag::On;
    match x {
        Flag::On => {
            return 1;
        }
        Flag::Off => {
            return 0;
        }
    }
}
"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("Flag x ="),
            "expected unannotated let x to be Flag, not int, in:\n{c}"
        );
        assert!(
            !c.contains("int x ="),
            "expected unannotated enum let not to lower as int in:\n{c}"
        );
    }

    #[test]
    fn generic_enum_unannotated_let_uses_option_int_not_int() {
        let src = r#"
enum Option<T> {
    Some(T);
    None;
}

fn main() -> int {
    let x = Option::Some(42);
    match x {
        Option::Some(v) => {
            return v;
        }
        Option::None => {
            return 0;
        }
    }
}
"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("Option_int x ="),
            "expected unannotated let x to be Option_int, not int, in:\n{c}"
        );
        assert!(
            !c.contains("    int x ="),
            "expected unannotated Option::Some let not to lower as int in:\n{c}"
        );
    }

    #[test]
    fn multi_file_imported_string_param_wraps_literal() {
        let src = r#"
fn println(s: String) -> int {
    return 0;
}
fn main() -> int {
    println("hi");
    return 0;
}
"#;
        let mut ir = crate::ir::lower_checked(src);
        ir.functions.retain(|f| f.name == "main");
        let mut types = crate::tc::TypeInfo::default();
        types
            .function_params
            .insert("println".to_string(), vec![Type::String]);
        types
            .function_params
            .insert("io::println".to_string(), vec![Type::String]);
        types
            .function_params
            .insert("io_println".to_string(), vec![Type::String]);
        let mut cg = Codegen::new();
        cg.set_type_info(&types);
        let c = cg.generate_module_source(&ir, "app", "test.ion", &[], "app");
        assert!(
            c.contains("ion_string_from_literal"),
            "expected imported String param to wrap literal in:\n{c}"
        );
    }

    #[test]
    fn fn_pointer_enum_lit_arg_uses_mangled_type() {
        let src = r#"
enum Result<T, E> {
    Ok(T);
    Err(E);
}
fn main() -> int {
    let f: fn(Result<int, int>) -> int = fn(r: Result<int, int>) -> int {
        match r {
            Result::Ok(v) => {
                return v;
            }
            Result::Err(_) => {
                return 0;
            }
        }
    };
    return f(Result::Ok(7));
}
"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        assert!(
            c.contains("(Result_int_int){"),
            "expected mangled Result literal at fn-pointer call, got:\n{c}"
        );
        assert!(
            !c.contains("(Result){"),
            "bare Result type name at fn-pointer call:\n{c}"
        );
    }

    #[test]
    fn loop_break_label_is_empty_statement() {
        let src = r#"fn main() -> int {
    let mut i: int = 0;
    while i < 2 {
        if i == 1 { break; }
        i = i + 1;
    }
    let end: int = i;
    return end;
}"#;
        let ir = crate::ir::lower_checked(src);
        let mut cg = Codegen::new();
        let c = cg.generate(&ir, "test.ion");
        let label = c
            .lines()
            .find(|line| line.contains("loop_break_") && line.contains(':'))
            .unwrap_or("");
        assert!(
            label.trim().ends_with(": ;"),
            "break label must be an empty statement so the next declaration is valid C, got:\n{c}"
        );
    }
}
