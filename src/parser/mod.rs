use crate::ast::*;
use crate::lexer::{Token, TokenKind};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        expected: String,
        got: TokenKind,
        span: Span,
    },
    UnexpectedEOF,
    Message(String),
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Program, ParseError> {
        let mut imports = Vec::new();
        let mut structs = Vec::new();
        let mut enums = Vec::new();
        let mut type_aliases = Vec::new();
        let mut functions = Vec::new();
        let mut extern_blocks = Vec::new();

        while !self.is_at_end() {
            if matches!(self.peek().kind, TokenKind::EOF) {
                break;
            }

            // Check for pub modifier
            let is_pub = matches!(self.peek().kind, TokenKind::Pub);
            if is_pub {
                self.advance(); // consume pub
            }

            match self.peek().kind {
                TokenKind::Import => {
                    if is_pub {
                        return Err(ParseError::UnexpectedToken {
                            expected: "struct, enum, or fn after pub".to_string(),
                            got: TokenKind::Import,
                            span: Span::from_token(self.peek()),
                        });
                    }
                    imports.push(self.parse_import()?);
                }
                TokenKind::Extern => {
                    if is_pub {
                        return Err(ParseError::UnexpectedToken {
                            expected: "struct, enum, or fn after pub".to_string(),
                            got: TokenKind::Extern,
                            span: Span::from_token(self.peek()),
                        });
                    }
                    extern_blocks.push(self.parse_extern_block()?);
                }
                TokenKind::Struct => {
                    let mut decl = self.parse_struct_decl()?;
                    decl.pub_ = is_pub;
                    structs.push(decl);
                }
                TokenKind::Enum => {
                    let mut decl = self.parse_enum_decl()?;
                    decl.pub_ = is_pub;
                    enums.push(decl);
                }
                TokenKind::Type => {
                    let mut decl = self.parse_type_alias()?;
                    decl.pub_ = is_pub;
                    type_aliases.push(decl);
                }
                TokenKind::Fn => {
                    let mut decl = self.parse_function()?;
                    decl.pub_ = is_pub;
                    functions.push(decl);
                }
                ref other => {
                    return Err(ParseError::UnexpectedToken {
                        expected: "import, extern, struct, enum, type, or fn".to_string(),
                        got: other.clone(),
                        span: Span::from_token(self.peek()),
                    });
                }
            }
        }

        Ok(Program {
            imports,
            structs,
            enums,
            type_aliases,
            functions,
            extern_blocks,
        })
    }

    fn parse_import(&mut self) -> Result<ImportStmt, ParseError> {
        let import_span = Span::from_token(self.expect(TokenKind::Import)?);

        // Expect string literal for path
        let path_token_idx = self.current;
        let path = if let TokenKind::StringLit(ref path_str) = self.tokens[path_token_idx].kind {
            path_str.clone()
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: "string literal".to_string(),
                got: self.tokens[path_token_idx].kind.clone(),
                span: Span::from_token(&self.tokens[path_token_idx]),
            });
        };
        self.advance(); // consume string literal

        // Expect 'as' keyword
        let as_token_idx = self.current;
        if !matches!(self.tokens[as_token_idx].kind, TokenKind::As) {
            return Err(ParseError::UnexpectedToken {
                expected: "as".to_string(),
                got: self.tokens[as_token_idx].kind.clone(),
                span: Span::from_token(&self.tokens[as_token_idx]),
            });
        }
        self.advance(); // consume 'as'

        // Expect identifier for alias
        let alias_token_idx = self.current;
        let alias = if let TokenKind::Ident(ref alias_str) = self.tokens[alias_token_idx].kind {
            alias_str.clone()
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: "identifier".to_string(),
                got: self.tokens[alias_token_idx].kind.clone(),
                span: Span::from_token(&self.tokens[alias_token_idx]),
            });
        };
        self.advance(); // consume identifier

        self.expect(TokenKind::Semicolon)?;

        let span = import_span.merge(&Span::from_token(self.previous()));

        Ok(ImportStmt { path, alias, span })
    }

    fn parse_extern_block(&mut self) -> Result<ExternBlock, ParseError> {
        let extern_span = Span::from_token(self.expect(TokenKind::Extern)?);

        // Expect string literal for linkage (e.g., "C")
        let linkage_token_idx = self.current;
        let linkage =
            if let TokenKind::StringLit(ref linkage_str) = self.tokens[linkage_token_idx].kind {
                linkage_str.clone()
            } else {
                return Err(ParseError::UnexpectedToken {
                    expected: "string literal".to_string(),
                    got: self.tokens[linkage_token_idx].kind.clone(),
                    span: Span::from_token(&self.tokens[linkage_token_idx]),
                });
            };
        self.advance(); // consume string literal

        self.expect(TokenKind::LBrace)?;

        let mut functions = Vec::new();
        while !self.is_at_end() && !matches!(self.peek().kind, TokenKind::RBrace) {
            functions.push(self.parse_extern_function()?);
        }

        self.expect(TokenKind::RBrace)?;

        let span = extern_span.merge(&Span::from_token(self.previous()));

        Ok(ExternBlock {
            linkage,
            functions,
            span,
        })
    }

    fn parse_extern_function(&mut self) -> Result<ExternFnDecl, ParseError> {
        let fn_span = Span::from_token(self.expect(TokenKind::Fn)?);

        let name_token_idx = self.current;
        let name = if let TokenKind::Ident(ref name_str) = self.tokens[name_token_idx].kind {
            name_str.clone()
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: "function name".to_string(),
                got: self.tokens[name_token_idx].kind.clone(),
                span: Span::from_token(&self.tokens[name_token_idx]),
            });
        };
        self.advance(); // consume identifier

        self.expect(TokenKind::LParen)?;

        // Parse parameters, checking for variadic function (...)
        let mut params = Vec::new();
        let mut variadic = false;

        if !self.is_at_end() && !matches!(self.peek().kind, TokenKind::RParen) {
            // Check if first token is ellipsis (variadic with no named params)
            if matches!(self.peek().kind, TokenKind::Ellipsis) {
                self.advance(); // consume ...
                variadic = true;
            } else {
                // Parse normal parameters
                loop {
                    // Check if this is the ellipsis (variadic marker)
                    if matches!(self.peek().kind, TokenKind::Ellipsis) {
                        self.advance(); // consume ...
                        variadic = true;
                        break;
                    }

                    let name_idx = self.current;
                    let name = if let TokenKind::Ident(ref ident_name) = self.tokens[name_idx].kind
                    {
                        ident_name.clone()
                    } else {
                        return Err(ParseError::UnexpectedToken {
                            expected: "parameter name".to_string(),
                            got: self.tokens[name_idx].kind.clone(),
                            span: Span::from_token(&self.tokens[name_idx]),
                        });
                    };
                    self.current += 1; // consume identifier

                    self.expect(TokenKind::Colon)?;
                    let ty = self.parse_type()?;

                    params.push(Param { name, ty });

                    if !self.is_at_end() && matches!(self.peek().kind, TokenKind::Comma) {
                        self.advance(); // consume ,
                    } else {
                        break;
                    }
                }
            }
        }

        self.expect(TokenKind::RParen)?;

        let return_type = if !self.is_at_end() && matches!(self.peek().kind, TokenKind::Arrow) {
            self.advance(); // consume ->
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(TokenKind::Semicolon)?;

        let span = fn_span.merge(&Span::from_token(self.previous()));

        Ok(ExternFnDecl {
            name,
            params,
            return_type,
            variadic,
            span,
        })
    }

    fn parse_function(&mut self) -> Result<FnDecl, ParseError> {
        let fn_token_span = Span::from_token(self.expect(TokenKind::Fn)?);

        // Extract name using current index to avoid borrow conflicts
        let name_token_idx = self.current;
        let name = if let TokenKind::Ident(ref ident_name) = self.tokens[name_token_idx].kind {
            ident_name.clone()
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: "function name".to_string(),
                got: self.tokens[name_token_idx].kind.clone(),
                span: Span::from_token(&self.tokens[name_token_idx]),
            });
        };
        self.advance(); // consume identifier

        // Parse generic parameters
        let generics = self.parse_generic_params()?;

        self.expect(TokenKind::LParen)?;

        let params = self.parse_params()?;

        self.expect(TokenKind::RParen)?;

        let return_type =
            if !self.is_at_end() && matches!(self.tokens[self.current].kind, TokenKind::Arrow) {
                self.advance(); // consume ->
                Some(self.parse_type()?)
            } else {
                None
            };

        let body = self.parse_block()?;

        let end_span = body
            .statements
            .last()
            .map(|s| match s {
                Stmt::Let(s) => s.span,
                Stmt::Return(s) => s.span,
                Stmt::Expr(s) => s.expr.span(),
                Stmt::Defer(s) => s.span,
                Stmt::Spawn(s) => s.span,
                Stmt::If(s) => s.span,
                Stmt::While(s) => s.span,
                Stmt::For(s) => s.span,
                Stmt::UnsafeBlock(s) => s.span,
            })
            .unwrap_or_else(|| Span::from_token(self.previous()));
        let span = fn_token_span.merge(&end_span);

        Ok(FnDecl {
            pub_: false, // Will be set by caller if pub modifier was present
            name,
            generics,
            params,
            return_type,
            body,
            span,
        })
    }

    fn parse_params(&mut self) -> Result<Vec<Param>, ParseError> {
        let mut params = Vec::new();

        let rparen_idx = self.current;
        if !self.is_at_end() && matches!(self.tokens[rparen_idx].kind, TokenKind::RParen) {
            return Ok(params);
        }

        loop {
            let name_idx = self.current;
            let name = if let TokenKind::Ident(ref ident_name) = self.tokens[name_idx].kind {
                ident_name.clone()
            } else {
                return Err(ParseError::UnexpectedToken {
                    expected: "parameter name".to_string(),
                    got: self.tokens[name_idx].kind.clone(),
                    span: Span::from_token(&self.tokens[name_idx]),
                });
            };
            self.current += 1; // consume identifier

            self.expect(TokenKind::Colon)?;
            let ty = self.parse_type()?;

            params.push(Param { name, ty });

            if !self.is_at_end() && matches!(self.peek().kind, TokenKind::Comma) {
                self.advance(); // consume ,
            } else {
                break;
            }
        }

        Ok(params)
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        // Check for tuple types: (Type1, Type2, ...) (before other types)
        let tuple_idx = self.current;
        let is_tuple =
            !self.is_at_end() && matches!(self.tokens[tuple_idx].kind, TokenKind::LParen);

        if is_tuple {
            self.current += 1; // consume (
            let mut elements = Vec::new();
            loop {
                if !self.is_at_end() && matches!(self.tokens[self.current].kind, TokenKind::RParen)
                {
                    break;
                }
                elements.push(self.parse_type()?);
                if !self.is_at_end() && matches!(self.tokens[self.current].kind, TokenKind::Comma) {
                    self.current += 1; // consume ,
                } else {
                    break;
                }
            }
            self.expect(TokenKind::RParen)?; // consume )
            return Ok(Type::Tuple { elements });
        }

        // Check for slice types: []T (before other types)
        let slice_idx = self.current;
        let is_slice =
            !self.is_at_end() && matches!(self.tokens[slice_idx].kind, TokenKind::LBracket);

        if is_slice {
            // Check if it's []T (slice) or [T; N] (array)
            self.current += 1; // consume [

            // Check if next token is ] (slice) or something else (array)
            if !self.is_at_end() && matches!(self.tokens[self.current].kind, TokenKind::RBracket) {
                // It's a slice: []T
                self.current += 1; // consume ]
                let inner_type = self.parse_type()?;
                return Ok(Type::Slice {
                    inner: Box::new(inner_type),
                });
            } else {
                // It's an array: [T; N]
                let inner_type = self.parse_type()?;
                self.expect(TokenKind::Semicolon)?;

                // Parse size
                let size_token = self.peek();
                let size = if let TokenKind::Integer(size_val) = &size_token.kind {
                    let val = *size_val;
                    self.advance();
                    val as usize
                } else {
                    return Err(ParseError::UnexpectedToken {
                        expected: "integer literal for array size".to_string(),
                        got: size_token.kind.clone(),
                        span: Span::from_token(size_token),
                    });
                };

                self.expect(TokenKind::RBracket)?;
                return Ok(Type::Array {
                    inner: Box::new(inner_type),
                    size,
                });
            }
        }

        // Check for raw pointer types: *T (before references)
        let star_idx = self.current;
        let is_raw_ptr = !self.is_at_end() && matches!(self.tokens[star_idx].kind, TokenKind::Star);

        if is_raw_ptr {
            self.current += 1; // consume *

            // Parse the inner type
            let inner_type = self.parse_type()?;

            Ok(Type::RawPtr {
                inner: Box::new(inner_type),
            })
        } else {
            // Check for reference types: &T or &mut T
            let ref_idx = self.current;
            let is_ref =
                !self.is_at_end() && matches!(self.tokens[ref_idx].kind, TokenKind::Ampersand);

            if is_ref {
                self.current += 1; // consume &

                // Check for &mut
                let mut_idx = self.current;
                let is_mut =
                    !self.is_at_end() && matches!(self.tokens[mut_idx].kind, TokenKind::Mut);
                if is_mut {
                    self.current += 1; // consume mut
                }

                // Parse the inner type
                let inner_type = self.parse_type()?;

                Ok(Type::Ref {
                    inner: Box::new(inner_type),
                    mutable: is_mut,
                })
            } else {
                // Parse base type
                let type_token = self.peek();
                match &type_token.kind {
                    TokenKind::Int => {
                        self.advance();
                        Ok(Type::Int)
                    }
                    TokenKind::Bool => {
                        self.advance();
                        Ok(Type::Bool)
                    }
                    TokenKind::F32 => {
                        self.advance();
                        Ok(Type::F32)
                    }
                    TokenKind::F64 => {
                        self.advance();
                        Ok(Type::F64)
                    }
                    TokenKind::I8 => {
                        self.advance();
                        Ok(Type::I8)
                    }
                    TokenKind::I16 => {
                        self.advance();
                        Ok(Type::I16)
                    }
                    TokenKind::I32 => {
                        self.advance();
                        Ok(Type::I32)
                    }
                    TokenKind::I64 => {
                        self.advance();
                        Ok(Type::I64)
                    }
                    TokenKind::U16 => {
                        self.advance();
                        Ok(Type::U16)
                    }
                    TokenKind::U32 => {
                        self.advance();
                        Ok(Type::U32)
                    }
                    TokenKind::U64 => {
                        self.advance();
                        Ok(Type::U64)
                    }
                    TokenKind::UInt => {
                        self.advance();
                        Ok(Type::UInt)
                    }
                    TokenKind::Ident(name) if name == "u8" => {
                        self.advance();
                        Ok(Type::U8)
                    }
                    TokenKind::String => {
                        self.advance();
                        Ok(Type::String)
                    }
                    TokenKind::Box => {
                        self.advance(); // consume 'Box'
                        self.expect(TokenKind::Less)?;
                        let inner = self.parse_type()?;
                        self.expect(TokenKind::Greater)?;
                        Ok(Type::Box {
                            inner: Box::new(inner),
                        })
                    }
                    TokenKind::Vec => {
                        self.advance(); // consume 'Vec'
                        self.expect(TokenKind::Less)?;
                        let inner = self.parse_type()?;
                        self.expect(TokenKind::Greater)?;
                        Ok(Type::Vec {
                            elem_type: Box::new(inner),
                        })
                    }
                    TokenKind::Channel => {
                        // Parse channel<T> types (deprecated, use Sender/Receiver)
                        self.advance(); // consume 'channel'
                        self.expect(TokenKind::Less)?;
                        let inner = self.parse_type()?;
                        self.expect(TokenKind::Greater)?;
                        Ok(Type::Channel {
                            elem_type: Box::new(inner),
                        })
                    }
                    TokenKind::Ident(name) => {
                        let type_name = name.clone();
                        self.advance();
                        // Check for built-in generic types first
                        if type_name == "Sender" || type_name == "Receiver" {
                            self.expect(TokenKind::Less)?;
                            let inner = self.parse_type()?;
                            self.expect(TokenKind::Greater)?;
                            if type_name == "Sender" {
                                return Ok(Type::Sender {
                                    elem_type: Box::new(inner),
                                });
                            } else {
                                return Ok(Type::Receiver {
                                    elem_type: Box::new(inner),
                                });
                            }
                        }
                        // Check if this is a generic type (e.g., Vec<int>, Option<Result<int, bool>>)
                        if !self.is_at_end() && matches!(self.peek().kind, TokenKind::Less) {
                            self.expect(TokenKind::Less)?;
                            let mut params = Vec::new();
                            loop {
                                params.push(self.parse_type()?);
                                if !self.is_at_end() && matches!(self.peek().kind, TokenKind::Comma)
                                {
                                    self.advance(); // consume ,
                                } else {
                                    break;
                                }
                            }
                            self.expect(TokenKind::Greater)?;
                            Ok(Type::Generic {
                                name: type_name,
                                params,
                            })
                        } else {
                            // Could be struct or enum - we'll resolve this in type checker
                            // For now, assume struct (enum will be handled by type checker)
                            Ok(Type::Struct(type_name))
                        }
                    }
                    _ => Err(ParseError::UnexpectedToken {
                        expected: "type".to_string(),
                        got: type_token.kind.clone(),
                        span: Span::from_token(type_token),
                    }),
                }
            }
        }
    }

    fn parse_generic_params(&mut self) -> Result<Vec<String>, ParseError> {
        if !self.is_at_end() && matches!(self.peek().kind, TokenKind::Less) {
            self.advance(); // consume <
            let mut params = Vec::new();

            loop {
                let name_idx = self.current;
                let name = if let TokenKind::Ident(ref ident_name) = self.tokens[name_idx].kind {
                    ident_name.clone()
                } else {
                    return Err(ParseError::UnexpectedToken {
                        expected: "type parameter name".to_string(),
                        got: self.tokens[name_idx].kind.clone(),
                        span: Span::from_token(&self.tokens[name_idx]),
                    });
                };
                self.current += 1; // consume identifier
                params.push(name);

                if !self.is_at_end() && matches!(self.peek().kind, TokenKind::Comma) {
                    self.advance(); // consume ,
                } else {
                    break;
                }
            }

            self.expect(TokenKind::Greater)?; // consume >
            Ok(params)
        } else {
            Ok(Vec::new())
        }
    }

    fn parse_struct_decl(&mut self) -> Result<StructDecl, ParseError> {
        let struct_span = Span::from_token(self.expect(TokenKind::Struct)?);

        // Name
        let name_idx = self.current;
        let name = if let TokenKind::Ident(ref ident_name) = self.tokens[name_idx].kind {
            ident_name.clone()
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: "struct name".to_string(),
                got: self.tokens[name_idx].kind.clone(),
                span: Span::from_token(&self.tokens[name_idx]),
            });
        };
        self.current += 1; // consume identifier

        // Parse generic parameters
        let generics = self.parse_generic_params()?;

        self.expect(TokenKind::LBrace)?;

        let mut fields = Vec::new();
        while !self.is_at_end() && !matches!(self.peek().kind, TokenKind::RBrace) {
            let field_name_idx = self.current;
            let field_name =
                if let TokenKind::Ident(ref ident_name) = self.tokens[field_name_idx].kind {
                    ident_name.clone()
                } else {
                    return Err(ParseError::UnexpectedToken {
                        expected: "field name".to_string(),
                        got: self.tokens[field_name_idx].kind.clone(),
                        span: Span::from_token(&self.tokens[field_name_idx]),
                    });
                };
            self.current += 1; // consume identifier

            self.expect(TokenKind::Colon)?;
            let ty = self.parse_type()?;

            // Field ends with semicolon
            let semi_tok = self.expect(TokenKind::Semicolon)?;

            let field_span = Span::from_token(semi_tok);
            fields.push(StructField {
                name: field_name,
                ty,
                span: field_span,
            });
        }

        self.expect(TokenKind::RBrace)?;

        let end_span = fields.last().map(|f| f.span).unwrap_or(struct_span);
        let span = struct_span.merge(&end_span);

        Ok(StructDecl {
            pub_: false, // Will be set by caller if pub modifier was present
            name,
            generics,
            fields,
            span,
        })
    }

    fn parse_enum_decl(&mut self) -> Result<EnumDecl, ParseError> {
        let enum_span = Span::from_token(self.expect(TokenKind::Enum)?);

        // Name
        let name_idx = self.current;
        let name = if let TokenKind::Ident(ref ident_name) = self.tokens[name_idx].kind {
            ident_name.clone()
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: "enum name".to_string(),
                got: self.tokens[name_idx].kind.clone(),
                span: Span::from_token(&self.tokens[name_idx]),
            });
        };
        self.current += 1; // consume identifier

        // Parse generic parameters
        let generics = self.parse_generic_params()?;

        self.expect(TokenKind::LBrace)?;

        let mut variants = Vec::new();
        while !self.is_at_end() && !matches!(self.peek().kind, TokenKind::RBrace) {
            let variant_name_idx = self.current;
            let variant_name =
                if let TokenKind::Ident(ref ident_name) = self.tokens[variant_name_idx].kind {
                    ident_name.clone()
                } else {
                    return Err(ParseError::UnexpectedToken {
                        expected: "variant name".to_string(),
                        got: self.tokens[variant_name_idx].kind.clone(),
                        span: Span::from_token(&self.tokens[variant_name_idx]),
                    });
                };
            self.current += 1; // consume identifier

            // Check for tuple variant (Type1, Type2) or struct variant { field: Type }
            let (payload_types, named_fields) =
                if !self.is_at_end() && matches!(self.peek().kind, TokenKind::LParen) {
                    // Tuple variant: Variant(Type1, Type2);
                    self.advance(); // consume (
                    let mut types = Vec::new();
                    loop {
                        if !self.is_at_end() && matches!(self.peek().kind, TokenKind::RParen) {
                            break;
                        }
                        types.push(self.parse_type()?);
                        if !self.is_at_end() && matches!(self.peek().kind, TokenKind::Comma) {
                            self.advance(); // consume ,
                        } else {
                            break;
                        }
                    }
                    self.expect(TokenKind::RParen)?; // consume )
                    (types, None)
                } else if !self.is_at_end() && matches!(self.peek().kind, TokenKind::LBrace) {
                    // Struct variant: Variant { field: Type };
                    self.advance(); // consume {
                    let mut fields = Vec::new();
                    loop {
                        if !self.is_at_end() && matches!(self.peek().kind, TokenKind::RBrace) {
                            break;
                        }
                        let field_name_idx = self.current;
                        let field_name = if let TokenKind::Ident(ref ident_name) =
                            self.tokens[field_name_idx].kind
                        {
                            ident_name.clone()
                        } else {
                            return Err(ParseError::UnexpectedToken {
                                expected: "field name".to_string(),
                                got: self.tokens[field_name_idx].kind.clone(),
                                span: Span::from_token(&self.tokens[field_name_idx]),
                            });
                        };
                        self.current += 1; // consume identifier
                        self.expect(TokenKind::Colon)?;
                        let field_type = self.parse_type()?;
                        fields.push((field_name, field_type));
                        if !self.is_at_end() && matches!(self.peek().kind, TokenKind::Comma) {
                            self.advance(); // consume ,
                        } else {
                            break;
                        }
                    }
                    self.expect(TokenKind::RBrace)?; // consume }
                    (Vec::new(), Some(fields))
                } else {
                    // Unit variant: Variant;
                    (Vec::new(), None)
                };

            let variant_span = Span::from_token(&self.tokens[variant_name_idx]);
            variants.push(EnumVariant {
                name: variant_name,
                payload_types,
                named_fields,
                span: variant_span,
            });

            // Variant ends with semicolon
            self.expect(TokenKind::Semicolon)?;
        }

        self.expect(TokenKind::RBrace)?;

        let end_span = variants.last().map(|v| v.span).unwrap_or(enum_span);
        let span = enum_span.merge(&end_span);

        Ok(EnumDecl {
            pub_: false, // Will be set by caller if pub modifier was present
            name,
            generics,
            variants,
            span,
        })
    }

    fn parse_type_alias(&mut self) -> Result<TypeAliasDecl, ParseError> {
        let type_span = Span::from_token(self.expect(TokenKind::Type)?);

        // Name
        let name_idx = self.current;
        let name = if let TokenKind::Ident(ref ident_name) = self.tokens[name_idx].kind {
            ident_name.clone()
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: "type alias name".to_string(),
                got: self.tokens[name_idx].kind.clone(),
                span: Span::from_token(&self.tokens[name_idx]),
            });
        };
        self.current += 1; // consume identifier

        // Parse generic parameters
        let generics = self.parse_generic_params()?;

        // Expect =
        self.expect(TokenKind::Equals)?;

        // Parse target type
        let target = self.parse_type()?;

        // Expect semicolon
        self.expect(TokenKind::Semicolon)?;

        let span = type_span.merge(&Span::from_token(self.previous()));

        Ok(TypeAliasDecl {
            pub_: false, // Will be set by caller if pub modifier was present
            name,
            generics,
            target,
            span,
        })
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        self.expect(TokenKind::LBrace)?;

        let mut statements = Vec::new();

        loop {
            if self.is_at_end() || matches!(self.tokens[self.current].kind, TokenKind::RBrace) {
                break;
            }
            statements.push(self.parse_stmt()?);
        }

        self.expect(TokenKind::RBrace)?;

        Ok(Block { statements })
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        match self.peek().kind {
            TokenKind::Let => {
                let stmt = self.parse_let_stmt()?;
                Ok(Stmt::Let(stmt))
            }
            TokenKind::If => {
                let stmt = self.parse_if_stmt()?;
                Ok(Stmt::If(stmt))
            }
            TokenKind::Return => {
                let stmt = self.parse_return_stmt()?;
                Ok(Stmt::Return(stmt))
            }
            TokenKind::Defer => {
                let stmt = self.parse_defer_stmt()?;
                Ok(Stmt::Defer(stmt))
            }
            TokenKind::Spawn => {
                let stmt = self.parse_spawn_stmt()?;
                Ok(Stmt::Spawn(stmt))
            }
            TokenKind::While => {
                let stmt = self.parse_while_stmt()?;
                Ok(Stmt::While(stmt))
            }
            TokenKind::For => {
                let stmt = self.parse_for_stmt()?;
                Ok(Stmt::For(stmt))
            }
            TokenKind::Unsafe => {
                let stmt = self.parse_unsafe_block()?;
                Ok(Stmt::UnsafeBlock(stmt))
            }
            _ => {
                let expr = self.parse_expr()?;
                self.expect(TokenKind::Semicolon)?;
                Ok(Stmt::Expr(ExprStmt { expr }))
            }
        }
    }

    fn parse_while_stmt(&mut self) -> Result<WhileStmt, ParseError> {
        let while_token_span = Span::from_token(self.expect(TokenKind::While)?);

        // Condition expression
        let cond = self.parse_expr()?;

        // Body block
        let body = self.parse_block()?;

        let end_span = body
            .statements
            .last()
            .map(|s| match s {
                Stmt::Let(s) => s.span,
                Stmt::Return(s) => s.span,
                Stmt::Expr(s) => s.expr.span(),
                Stmt::Defer(s) => s.span,
                Stmt::Spawn(s) => s.span,
                Stmt::If(s) => s.span,
                Stmt::While(s) => s.span,
                Stmt::For(s) => s.span,
                Stmt::UnsafeBlock(s) => s.span,
            })
            .unwrap_or(while_token_span);
        let span = while_token_span.merge(&end_span);

        Ok(WhileStmt { cond, body, span })
    }

    fn parse_for_stmt(&mut self) -> Result<ForStmt, ParseError> {
        let for_token_span = Span::from_token(self.expect(TokenKind::For)?);

        // Parse variable name
        let var_name_idx = self.current;
        let var_name = if let TokenKind::Ident(ref ident_name) = self.tokens[var_name_idx].kind {
            ident_name.clone()
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: "variable name".to_string(),
                got: self.tokens[var_name_idx].kind.clone(),
                span: Span::from_token(&self.tokens[var_name_idx]),
            });
        };
        self.current += 1; // consume identifier

        // Expect "in" keyword (we'll check for it as an identifier for now)
        let in_idx = self.current;
        if let TokenKind::Ident(ref ident_name) = self.tokens[in_idx].kind {
            if ident_name != "in" {
                return Err(ParseError::UnexpectedToken {
                    expected: "\"in\" keyword".to_string(),
                    got: self.tokens[in_idx].kind.clone(),
                    span: Span::from_token(&self.tokens[in_idx]),
                });
            }
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: "\"in\" keyword".to_string(),
                got: self.tokens[in_idx].kind.clone(),
                span: Span::from_token(&self.tokens[in_idx]),
            });
        }
        self.current += 1; // consume "in"

        // Parse iterable expression
        let iterable = self.parse_expr()?;

        // Parse loop body
        let body = self.parse_block()?;

        let end_span = body
            .statements
            .last()
            .map(|s| match s {
                Stmt::Let(s) => s.span,
                Stmt::Return(s) => s.span,
                Stmt::Expr(s) => s.expr.span(),
                Stmt::Defer(s) => s.span,
                Stmt::Spawn(s) => s.span,
                Stmt::If(s) => s.span,
                Stmt::While(s) => s.span,
                Stmt::For(s) => s.span,
                Stmt::UnsafeBlock(s) => s.span,
            })
            .unwrap_or(for_token_span);
        let span = for_token_span.merge(&end_span);

        Ok(ForStmt {
            var_name,
            iterable,
            body,
            span,
        })
    }

    fn parse_match_expr(&mut self) -> Result<MatchExpr, ParseError> {
        let match_token_span = Span::from_token(self.expect(TokenKind::Match)?);

        // Match expression
        let expr = self.parse_expr()?;

        self.expect(TokenKind::LBrace)?;

        let mut arms = Vec::new();
        while !self.is_at_end() && !matches!(self.peek().kind, TokenKind::RBrace) {
            let pattern = self.parse_pattern()?;
            self.expect(TokenKind::Arrow)?; // =>
            let body = self.parse_block()?;
            let arm_span = pattern.span().merge(
                &body
                    .statements
                    .last()
                    .map(|s| match s {
                        Stmt::Let(s) => s.span,
                        Stmt::Return(s) => s.span,
                        Stmt::Expr(s) => s.expr.span(),
                        Stmt::Defer(s) => s.span,
                        Stmt::Spawn(s) => s.span,
                        Stmt::If(s) => s.span,
                        Stmt::While(s) => s.span,
                        Stmt::For(s) => s.span,
                        Stmt::UnsafeBlock(s) => s.span,
                    })
                    .unwrap_or(pattern.span()),
            );
            arms.push(MatchArm {
                pattern,
                body,
                span: arm_span,
            });
            // Optional comma between arms
            if !self.is_at_end() && matches!(self.peek().kind, TokenKind::Comma) {
                self.advance();
            }
        }

        self.expect(TokenKind::RBrace)?;

        let end_span = arms.last().map(|a| a.span).unwrap_or(match_token_span);
        let span = match_token_span.merge(&end_span);

        Ok(MatchExpr {
            expr: Box::new(expr),
            arms,
            span,
        })
    }

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        let pattern_span = Span::from_token(self.peek());

        // Check for wildcard first (before checking if it's an identifier)
        if let TokenKind::Ident(ref ident_name) = self.peek().kind
            && ident_name == "_"
        {
            self.advance();
            return Ok(Pattern::Wildcard { span: pattern_span });
        }

        match self.peek().kind {
            TokenKind::Ident(_) => {
                // Could be variant pattern or binding
                // Use lookahead BEFORE advancing to check for :: (same as expression parser)
                let name_idx = self.current;
                let name = if let TokenKind::Ident(ref ident_name) = self.tokens[name_idx].kind {
                    ident_name.clone()
                } else {
                    unreachable!()
                };

                // Check for :: using lookahead (check current+1 and current+2, since current is the identifier)
                // Must have at least 3 tokens ahead: identifier, first :, second :
                let has_enough_tokens = self.current + 2 < self.tokens.len();
                let next_is_colon = if self.current + 1 < self.tokens.len() {
                    matches!(self.tokens[self.current + 1].kind, TokenKind::Colon)
                } else {
                    false
                };
                let next_next_is_colon = if self.current + 2 < self.tokens.len() {
                    matches!(self.tokens[self.current + 2].kind, TokenKind::Colon)
                } else {
                    false
                };
                let next_is_colon_colon = has_enough_tokens && next_is_colon && next_next_is_colon;

                if next_is_colon_colon {
                    // This is EnumName::VariantName - advance past identifier and both colons
                    self.advance(); // consume identifier
                    self.advance(); // consume first :
                    self.advance(); // consume second :
                    let variant_name_idx = self.current;
                    let variant_name = if let TokenKind::Ident(ref ident_name) =
                        self.tokens[variant_name_idx].kind
                    {
                        ident_name.clone()
                    } else {
                        return Err(ParseError::UnexpectedToken {
                            expected: "variant name".to_string(),
                            got: self.tokens[variant_name_idx].kind.clone(),
                            span: Span::from_token(&self.tokens[variant_name_idx]),
                        });
                    };
                    self.advance();

                    // Parse sub-patterns if present (tuple or struct variant)
                    let (sub_patterns, named_fields) = if !self.is_at_end()
                        && matches!(self.peek().kind, TokenKind::LParen)
                    {
                        // Tuple variant pattern: Enum::Variant(pattern1, pattern2)
                        self.advance(); // consume (
                        let mut patterns = Vec::new();
                        loop {
                            if !self.is_at_end() && matches!(self.peek().kind, TokenKind::RParen) {
                                break;
                            }
                            patterns.push(self.parse_pattern()?);
                            if !self.is_at_end() && matches!(self.peek().kind, TokenKind::Comma) {
                                self.advance(); // consume ,
                            } else {
                                break;
                            }
                        }
                        self.expect(TokenKind::RParen)?; // consume )
                        (patterns, None)
                    } else if !self.is_at_end() && matches!(self.peek().kind, TokenKind::LBrace) {
                        // Struct variant pattern: Enum::Variant { field: pattern }
                        self.advance(); // consume {
                        let mut fields = Vec::new();
                        loop {
                            if !self.is_at_end() && matches!(self.peek().kind, TokenKind::RBrace) {
                                break;
                            }
                            let field_name_idx = self.current;
                            let field_name = if let TokenKind::Ident(ref ident_name) =
                                self.tokens[field_name_idx].kind
                            {
                                ident_name.clone()
                            } else {
                                return Err(ParseError::UnexpectedToken {
                                    expected: "field name".to_string(),
                                    got: self.tokens[field_name_idx].kind.clone(),
                                    span: Span::from_token(&self.tokens[field_name_idx]),
                                });
                            };
                            self.current += 1; // consume identifier
                            self.expect(TokenKind::Colon)?;
                            let field_pattern = self.parse_pattern()?;
                            fields.push((field_name, field_pattern));
                            if !self.is_at_end() && matches!(self.peek().kind, TokenKind::Comma) {
                                self.advance(); // consume ,
                            } else {
                                break;
                            }
                        }
                        self.expect(TokenKind::RBrace)?; // consume }
                        (Vec::new(), Some(fields))
                    } else {
                        // Unit variant: Enum::Variant
                        (Vec::new(), None)
                    };

                    let end_span = if let Some(ref fields) = named_fields {
                        fields.last().map(|(_, p)| p.span()).unwrap_or(pattern_span)
                    } else {
                        sub_patterns
                            .last()
                            .map(|p| p.span())
                            .unwrap_or(pattern_span)
                    };
                    let variant_span = pattern_span.merge(&end_span);
                    Ok(Pattern::Variant {
                        enum_name: name,
                        variant: variant_name,
                        sub_patterns,
                        named_fields,
                        span: variant_span,
                    })
                } else {
                    // Just a binding
                    self.advance(); // consume identifier
                    Ok(Pattern::Binding {
                        name,
                        span: pattern_span,
                    })
                }
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: "pattern".to_string(),
                got: self.peek().kind.clone(),
                span: pattern_span,
            }),
        }
    }

    fn parse_if_stmt(&mut self) -> Result<IfStmt, ParseError> {
        let if_token_span = Span::from_token(self.expect(TokenKind::If)?);

        // Condition expression
        let cond = self.parse_expr()?;

        // Then block
        let then_block = self.parse_block()?;

        // Optional else block
        let else_block = if !self.is_at_end() && matches!(self.peek().kind, TokenKind::Else) {
            self.advance(); // consume else
            // For minimal subset, we only support `else { ... }` (no else-if chain)
            Some(self.parse_block()?)
        } else {
            None
        };

        // Span from `if` to end of last block
        let end_span = if let Some(ref else_blk) = else_block {
            else_blk
                .statements
                .last()
                .map(|s| match s {
                    Stmt::Let(s) => s.span,
                    Stmt::Return(s) => s.span,
                    Stmt::Expr(s) => s.expr.span(),
                    Stmt::Defer(s) => s.span,
                    Stmt::Spawn(s) => s.span,
                    Stmt::If(s) => s.span,
                    Stmt::While(s) => s.span,
                    Stmt::For(s) => s.span,
                    Stmt::UnsafeBlock(s) => s.span,
                })
                .unwrap_or(if_token_span)
        } else {
            then_block
                .statements
                .last()
                .map(|s| match s {
                    Stmt::Let(s) => s.span,
                    Stmt::Return(s) => s.span,
                    Stmt::Expr(s) => s.expr.span(),
                    Stmt::Defer(s) => s.span,
                    Stmt::Spawn(s) => s.span,
                    Stmt::If(s) => s.span,
                    Stmt::While(s) => s.span,
                    Stmt::For(s) => s.span,
                    Stmt::UnsafeBlock(s) => s.span,
                })
                .unwrap_or(if_token_span)
        };
        let span = if_token_span.merge(&end_span);

        Ok(IfStmt {
            cond,
            then_block,
            else_block,
            span,
        })
    }

    fn parse_let_stmt(&mut self) -> Result<LetStmt, ParseError> {
        let let_token_span = Span::from_token(self.expect(TokenKind::Let)?);

        // Check for 'mut' keyword
        let is_mut =
            if !self.is_at_end() && matches!(self.tokens[self.current].kind, TokenKind::Mut) {
                self.current += 1; // consume 'mut'
                true
            } else {
                false
            };

        // Check for tuple destructuring: let (a, b, c) = ...
        let patterns = if !self.is_at_end() && matches!(self.peek().kind, TokenKind::LParen) {
            self.advance(); // consume (
            let mut patterns = Vec::new();
            loop {
                if !self.is_at_end() && matches!(self.peek().kind, TokenKind::RParen) {
                    break;
                }
                patterns.push(self.parse_pattern()?);
                if !self.is_at_end() && matches!(self.peek().kind, TokenKind::Comma) {
                    self.advance(); // consume ,
                } else {
                    break;
                }
            }
            self.expect(TokenKind::RParen)?; // consume )
            Some(patterns)
        } else {
            None
        };

        let name = if patterns.is_none() {
            let name_idx = self.current;
            let name_val = if let TokenKind::Ident(ref ident_name) = self.tokens[name_idx].kind {
                ident_name.clone()
            } else {
                return Err(ParseError::UnexpectedToken {
                    expected: "variable name or tuple pattern".to_string(),
                    got: self.tokens[name_idx].kind.clone(),
                    span: Span::from_token(&self.tokens[name_idx]),
                });
            };
            self.current += 1; // consume identifier manually
            name_val
        } else {
            String::new() // Not used when patterns is Some
        };

        let colon_idx = self.current;
        let type_ann =
            if !self.is_at_end() && matches!(self.tokens[colon_idx].kind, TokenKind::Colon) {
                self.current += 1; // consume :
                Some(self.parse_type()?)
            } else {
                None
            };

        let equals_idx = self.current;
        let init = if !self.is_at_end() && matches!(self.tokens[equals_idx].kind, TokenKind::Equals)
        {
            self.current += 1; // consume =
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.expect(TokenKind::Semicolon)?;

        let span = let_token_span.merge(&Span::from_token(self.previous()));

        Ok(LetStmt {
            name,
            patterns,
            mutable: is_mut,
            type_ann,
            init,
            span,
        })
    }

    fn parse_return_stmt(&mut self) -> Result<ReturnStmt, ParseError> {
        let return_token_span = Span::from_token(self.expect(TokenKind::Return)?);

        let semicolon_idx = self.current;
        let value = if !self.is_at_end()
            && !matches!(self.tokens[semicolon_idx].kind, TokenKind::Semicolon)
        {
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.expect(TokenKind::Semicolon)?;

        let span = return_token_span.merge(&Span::from_token(self.previous()));

        Ok(ReturnStmt { value, span })
    }

    fn parse_defer_stmt(&mut self) -> Result<DeferStmt, ParseError> {
        let defer_token_span = Span::from_token(self.expect(TokenKind::Defer)?);

        let expr = self.parse_expr()?;

        self.expect(TokenKind::Semicolon)?;

        let span = defer_token_span.merge(&expr.span());

        Ok(DeferStmt { expr, span })
    }

    fn parse_spawn_stmt(&mut self) -> Result<SpawnStmt, ParseError> {
        let spawn_token_span = Span::from_token(self.expect(TokenKind::Spawn)?);

        // Grammar: spawn_stmt = "spawn" , block , ";" ;
        let body = self.parse_block()?;
        self.expect(TokenKind::Semicolon)?;

        // Span from 'spawn' to end of block
        let end_span = body
            .statements
            .last()
            .map(|s| match s {
                Stmt::Let(s) => s.span,
                Stmt::Return(s) => s.span,
                Stmt::Expr(s) => s.expr.span(),
                Stmt::Defer(s) => s.span,
                Stmt::Spawn(s) => s.span,
                Stmt::If(s) => s.span,
                Stmt::While(s) => s.span,
                Stmt::For(s) => s.span,
                Stmt::UnsafeBlock(s) => s.span,
            })
            .unwrap_or(spawn_token_span);
        let span = spawn_token_span.merge(&end_span);

        Ok(SpawnStmt { body, span })
    }

    fn parse_unsafe_block(&mut self) -> Result<UnsafeBlockStmt, ParseError> {
        let unsafe_token_span = Span::from_token(self.expect(TokenKind::Unsafe)?);

        let body = self.parse_block()?;

        let end_span = body
            .statements
            .last()
            .map(|s| match s {
                Stmt::Let(s) => s.span,
                Stmt::Return(s) => s.span,
                Stmt::Expr(s) => s.expr.span(),
                Stmt::Defer(s) => s.span,
                Stmt::Spawn(s) => s.span,
                Stmt::If(s) => s.span,
                Stmt::While(s) => s.span,
                Stmt::For(s) => s.span,
                Stmt::UnsafeBlock(s) => s.span,
            })
            .unwrap_or(unsafe_token_span);
        let span = unsafe_token_span.merge(&end_span);

        Ok(UnsafeBlockStmt { body, span })
    }

    // Expression parsing with operator precedence
    // Precedence (lowest to highest):
    // 1. <, >, <=, >=, ==, != (comparison)
    // 2. +, - (additive)
    // 3. *, / (multiplicative)
    // 4. &, &mut (address-of/borrow) - unary operators
    // 5. literals, variables, parenthesized expressions

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr, ParseError> {
        // Try to parse an assignment: target = value
        // First parse the left-hand side (which could be a variable or indexed expression)
        let lhs = self.parse_logical_or()?;

        // Check if next token is '='
        let equals_idx = self.current;
        if self.is_at_end() {
            return Ok(lhs);
        }

        if matches!(self.tokens[equals_idx].kind, TokenKind::Equals) {
            // This is an assignment
            let start_span = lhs.span();
            self.advance(); // consume '='

            // Parse the right-hand side (which is also an assignment expression, allowing chaining)
            let rhs = self.parse_assignment()?;
            let end_span = rhs.span();
            let span = start_span.merge(&end_span);

            // Validate that lhs is a valid assignment target (Var or Index)
            match &lhs {
                Expr::Var(_) | Expr::Index(_) => Ok(Expr::Assign(AssignExpr {
                    target: Box::new(lhs),
                    value: Box::new(rhs),
                    span,
                })),
                _ => Err(ParseError::Message(
                    "Assignment target must be a variable or indexed expression".to_string(),
                )),
            }
        } else {
            Ok(lhs)
        }
    }

    fn parse_logical_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_logical_and()?;

        loop {
            let op_idx = self.current;
            if self.is_at_end() {
                break;
            }

            if matches!(self.tokens[op_idx].kind, TokenKind::OrOr) {
                self.current += 1; // consume ||
                let right = self.parse_logical_and()?;
                let span = expr.span().merge(&right.span());
                expr = Expr::BinOp(BinOpExpr {
                    op: BinOp::Or,
                    left: Box::new(expr),
                    right: Box::new(right),
                    span,
                });
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_comparison()?;

        loop {
            let op_idx = self.current;
            if self.is_at_end() {
                break;
            }

            if matches!(self.tokens[op_idx].kind, TokenKind::AndAnd) {
                self.current += 1; // consume &&
                let right = self.parse_comparison()?;
                let span = expr.span().merge(&right.span());
                expr = Expr::BinOp(BinOpExpr {
                    op: BinOp::And,
                    left: Box::new(expr),
                    right: Box::new(right),
                    span,
                });
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_equality()?;

        loop {
            let op_idx = self.current;
            if self.is_at_end() {
                break;
            }

            let op = match self.tokens[op_idx].kind {
                TokenKind::Less => Some(BinOp::Lt),
                TokenKind::Greater => Some(BinOp::Gt),
                TokenKind::LessEqual => Some(BinOp::Le),
                TokenKind::GreaterEqual => Some(BinOp::Ge),
                _ => None,
            };

            if let Some(bin_op) = op {
                self.current += 1; // consume operator
                let right = self.parse_equality()?;
                let span = expr.span().merge(&right.span());
                expr = Expr::BinOp(BinOpExpr {
                    op: bin_op,
                    left: Box::new(expr),
                    right: Box::new(right),
                    span,
                });
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_bitwise_or()?;

        loop {
            let op_idx = self.current;
            if self.is_at_end() {
                break;
            }

            let op = match self.tokens[op_idx].kind {
                TokenKind::EqualsEquals => Some(BinOp::Eq),
                TokenKind::NotEquals => Some(BinOp::Ne),
                _ => None,
            };

            if let Some(bin_op) = op {
                self.current += 1; // consume operator
                let right = self.parse_bitwise_or()?;
                let span = expr.span().merge(&right.span());
                expr = Expr::BinOp(BinOpExpr {
                    op: bin_op,
                    left: Box::new(expr),
                    right: Box::new(right),
                    span,
                });
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_bitwise_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_bitwise_xor()?;

        loop {
            let op_idx = self.current;
            if self.is_at_end() {
                break;
            }

            if matches!(self.tokens[op_idx].kind, TokenKind::Pipe) {
                self.current += 1; // consume |
                let right = self.parse_bitwise_xor()?;
                let span = expr.span().merge(&right.span());
                expr = Expr::BinOp(BinOpExpr {
                    op: BinOp::BitOr,
                    left: Box::new(expr),
                    right: Box::new(right),
                    span,
                });
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_bitwise_xor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_bitwise_and()?;

        loop {
            let op_idx = self.current;
            if self.is_at_end() {
                break;
            }

            if matches!(self.tokens[op_idx].kind, TokenKind::Caret) {
                self.current += 1; // consume ^
                let right = self.parse_bitwise_and()?;
                let span = expr.span().merge(&right.span());
                expr = Expr::BinOp(BinOpExpr {
                    op: BinOp::BitXor,
                    left: Box::new(expr),
                    right: Box::new(right),
                    span,
                });
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_bitwise_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_shift()?;

        loop {
            let op_idx = self.current;
            if self.is_at_end() {
                break;
            }

            // Disambiguate & (bitwise AND) from & (reference operator)
            // & is bitwise AND if it's between two expressions (binary operator)
            // & is reference if it's before an identifier/expression (unary operator)
            // Since we're in parse_bitwise_and, we've already parsed the left expression,
            // so if we see & here, it must be bitwise AND (binary operator)
            if matches!(self.tokens[op_idx].kind, TokenKind::Ampersand) {
                // In this context, & is always bitwise AND because we're parsing a binary expression
                // The left side is already parsed, so this is a binary operator
                self.current += 1; // consume &
                let right = self.parse_shift()?;
                let span = expr.span().merge(&right.span());
                expr = Expr::BinOp(BinOpExpr {
                    op: BinOp::BitAnd,
                    left: Box::new(expr),
                    right: Box::new(right),
                    span,
                });
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_shift(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_additive()?;

        loop {
            let op_idx = self.current;
            if self.is_at_end() {
                break;
            }

            let op = match self.tokens[op_idx].kind {
                TokenKind::ShiftLeft => Some(BinOp::ShiftLeft),
                TokenKind::ShiftRight => Some(BinOp::ShiftRight),
                _ => None,
            };

            if let Some(bin_op) = op {
                self.current += 1; // consume operator
                let right = self.parse_additive()?;
                let span = expr.span().merge(&right.span());
                expr = Expr::BinOp(BinOpExpr {
                    op: bin_op,
                    left: Box::new(expr),
                    right: Box::new(right),
                    span,
                });
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        // Check for unary operators: !, &, &mut, -
        let unary_idx = self.current;
        let is_not = !self.is_at_end() && matches!(self.tokens[unary_idx].kind, TokenKind::Not);
        let is_ampersand =
            !self.is_at_end() && matches!(self.tokens[unary_idx].kind, TokenKind::Ampersand);
        let is_minus = !self.is_at_end() && matches!(self.tokens[unary_idx].kind, TokenKind::Minus);

        if is_not {
            let start_span = Span::from_token(&self.tokens[unary_idx]);
            self.current += 1; // consume !
            let operand = self.parse_unary()?; // Right-associative
            let end_span = operand.span();
            let span = start_span.merge(&end_span);
            let expr = Expr::UnOp(UnOpExpr {
                op: UnOp::Not,
                operand: Box::new(operand),
                span,
            });
            self.parse_field_access_chain(expr)
        } else if is_ampersand {
            let start_span = Span::from_token(&self.tokens[unary_idx]);
            self.current += 1; // consume &

            // Check for &mut
            let mut_idx = self.current;
            let is_mut = !self.is_at_end() && matches!(self.tokens[mut_idx].kind, TokenKind::Mut);
            if is_mut {
                self.current += 1; // consume mut
            }

            // Parse the operand
            let inner = self.parse_unary()?;
            let end_span = inner.span();
            let span = start_span.merge(&end_span);

            let expr = Expr::Ref(RefExpr {
                mutable: is_mut,
                inner: Box::new(inner),
                span,
            });
            self.parse_field_access_chain(expr)
        } else if is_minus {
            // Unary minus: -expr
            let start_span = Span::from_token(&self.tokens[unary_idx]);
            self.current += 1; // consume -
            let operand = self.parse_unary()?; // Right-associative
            let end_span = operand.span();
            let span = start_span.merge(&end_span);
            let expr = Expr::UnOp(UnOpExpr {
                op: UnOp::Neg,
                operand: Box::new(operand),
                span,
            });
            self.parse_field_access_chain(expr)
        } else {
            let expr = self.parse_primary()?;
            let expr = self.parse_field_access_chain(expr)?;
            self.parse_cast_chain(expr)
        }
    }

    fn parse_cast_chain(&mut self, mut expr: Expr) -> Result<Expr, ParseError> {
        loop {
            let as_idx = self.current;
            if self.is_at_end() {
                break;
            }

            // Check for 'as' keyword
            if matches!(self.tokens[as_idx].kind, TokenKind::As) {
                let start_span = expr.span();
                self.advance(); // consume 'as'

                // Parse target type
                let target_type = self.parse_type()?;
                let end_span = Span::from_token(self.previous());
                let span = start_span.merge(&end_span);

                expr = Expr::Cast(CastExpr {
                    expr: Box::new(expr),
                    target_type,
                    span,
                });
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_field_access_chain(&mut self, mut expr: Expr) -> Result<Expr, ParseError> {
        loop {
            // Check for indexing: expr[expr]
            let bracket_idx = self.current;
            if !self.is_at_end() && matches!(self.tokens[bracket_idx].kind, TokenKind::LBracket) {
                let start_span = expr.span();
                self.advance(); // consume [
                let index_expr = self.parse_expr()?;
                self.expect(TokenKind::RBracket)?; // consume ]
                let end_span = Span::from_token(self.previous());
                let span = start_span.merge(&end_span);

                expr = Expr::Index(IndexExpr {
                    target: Box::new(expr),
                    index: Box::new(index_expr),
                    span,
                });
                continue;
            }

            // Check for field access: expr.field
            let dot_idx = self.current;
            if self.is_at_end() {
                break;
            }

            // Stop parsing if we encounter tokens that indicate the end of an expression
            // Use explicit match to ensure proper pattern matching
            match self.tokens[dot_idx].kind {
                // Statement-starting keywords - must stop immediately
                TokenKind::If
                | TokenKind::While
                | TokenKind::Let
                | TokenKind::Return
                | TokenKind::Else
                | TokenKind::Defer
                | TokenKind::Spawn
                | TokenKind::Unsafe => {
                    break;
                }
                // Other expression-ending tokens
                TokenKind::LBrace
                | TokenKind::RBrace
                | TokenKind::Semicolon
                | TokenKind::Comma
                | TokenKind::RParen
                | TokenKind::RBracket
                | TokenKind::AndAnd
                | TokenKind::OrOr
                | TokenKind::EqualsEquals
                | TokenKind::NotEquals
                | TokenKind::Less
                | TokenKind::Greater
                | TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::Ampersand  // Stop for bitwise AND (handled by parse_bitwise_and)
                | TokenKind::Pipe       // Stop for bitwise OR (handled by parse_bitwise_or)
                | TokenKind::Caret      // Stop for bitwise XOR (handled by parse_bitwise_xor)
                | TokenKind::ShiftLeft  // Stop for shift (handled by parse_shift)
                | TokenKind::ShiftRight // Stop for shift (handled by parse_shift)
                | TokenKind::Arrow => {
                    break;
                }
                // Only proceed if it's a dot
                TokenKind::Dot => {
                    // Consume '.'
                    let dot_token = self.advance();
                    let start_span = Span::from_token(dot_token);

                    // Expect field identifier - check again after consuming dot
                    let field_idx = self.current;
                    if self.is_at_end() {
                        return Err(ParseError::UnexpectedEOF);
                    }

                    // Additional safety check: if the next token is a statement keyword, error
                    if matches!(
                        &self.tokens[field_idx].kind,
                        TokenKind::If
                            | TokenKind::While
                            | TokenKind::Let
                            | TokenKind::Return
                            | TokenKind::Else
                            | TokenKind::Defer
                            | TokenKind::Spawn
                            | TokenKind::Unsafe
                    ) {
                        return Err(ParseError::UnexpectedToken {
                            expected: "field name".to_string(),
                            got: self.tokens[field_idx].kind.clone(),
                            span: Span::from_token(&self.tokens[field_idx]),
                        });
                    }

                    let field_name =
                        if let TokenKind::Ident(ref ident_name) = self.tokens[field_idx].kind {
                            ident_name.clone()
                        } else {
                            return Err(ParseError::UnexpectedToken {
                                expected: "field name".to_string(),
                                got: self.tokens[field_idx].kind.clone(),
                                span: Span::from_token(&self.tokens[field_idx]),
                            });
                        };
                    self.current += 1; // consume identifier

                    // Check if this is a method call (field_name followed by '(')
                    let is_method_call = if !self.is_at_end() {
                        matches!(self.peek().kind, TokenKind::LParen)
                    } else {
                        false
                    };

                    if is_method_call {
                        // This is a method call: expr.method(args...)
                        // Parse arguments
                        self.expect(TokenKind::LParen)?;
                        let mut args = Vec::new();
                        loop {
                            if !self.is_at_end() && matches!(self.peek().kind, TokenKind::RParen) {
                                break;
                            }
                            args.push(self.parse_expr()?);
                            if !self.is_at_end() && matches!(self.peek().kind, TokenKind::Comma) {
                                self.advance(); // consume ,
                            } else {
                                break;
                            }
                        }
                        self.expect(TokenKind::RParen)?;

                        let end_span = args.last().map(|a| a.span()).unwrap_or(start_span);
                        let span = start_span.merge(&end_span);

                        expr = Expr::MethodCall(MethodCallExpr {
                            receiver: Box::new(expr),
                            method: field_name,
                            args,
                            span,
                        });
                        // Continue the loop to allow chaining: x.method1().method2()
                        continue;
                    } else {
                        // This is a regular field access: expr.field
                        let end_span = expr.span();
                        let span = start_span.merge(&end_span);

                        expr = Expr::FieldAccess(FieldAccessExpr {
                            base: Box::new(expr),
                            field: field_name,
                            span,
                        });
                        // Continue the loop to check for more field accesses
                        continue;
                    }
                }
                // Any other token - end of field access chain
                _ => {
                    break;
                }
            }
        }

        Ok(expr)
    }

    fn parse_additive(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_multiplicative()?;

        loop {
            let op_idx = self.current;
            if self.is_at_end()
                || !matches!(self.tokens[op_idx].kind, TokenKind::Plus | TokenKind::Minus)
            {
                break;
            }
            let op = match self.tokens[op_idx].kind {
                TokenKind::Plus => BinOp::Add,
                TokenKind::Minus => BinOp::Sub,
                _ => unreachable!(),
            };
            self.current += 1; // consume operator
            let right = self.parse_multiplicative()?;
            let span = expr.span().merge(&right.span());
            expr = Expr::BinOp(BinOpExpr {
                op,
                left: Box::new(expr),
                right: Box::new(right),
                span,
            });
        }

        Ok(expr)
    }

    fn parse_multiplicative(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_unary()?;

        loop {
            let op_idx = self.current;
            if self.is_at_end()
                || !matches!(self.tokens[op_idx].kind, TokenKind::Star | TokenKind::Slash)
            {
                break;
            }
            let op = match self.tokens[op_idx].kind {
                TokenKind::Star => BinOp::Mul,
                TokenKind::Slash => BinOp::Div,
                _ => unreachable!(),
            };
            self.current += 1; // consume operator
            let right = self.parse_unary()?;
            let span = expr.span().merge(&right.span());
            expr = Expr::BinOp(BinOpExpr {
                op,
                left: Box::new(expr),
                right: Box::new(right),
                span,
            });
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        let primary_idx = self.current;
        let span = Span::from_token(&self.tokens[primary_idx]);
        let kind = self.tokens[primary_idx].kind.clone();

        // Check if this is a keyword followed by :: (e.g., Box::new, Vec::new, String::new)
        let next_is_colon_colon = if self.current + 2 < self.tokens.len() {
            matches!(self.tokens[self.current + 1].kind, TokenKind::Colon)
                && matches!(self.tokens[self.current + 2].kind, TokenKind::Colon)
        } else {
            false
        };

        // If it's a keyword followed by ::, treat it as a function call
        if next_is_colon_colon {
            let keyword_name = match &kind {
                TokenKind::Box => "Box",
                TokenKind::Vec => "Vec",
                TokenKind::String => "String",
                _ => "",
            };

            if !keyword_name.is_empty() {
                self.advance(); // consume keyword
                self.advance(); // consume first :
                self.advance(); // consume second :

                // Parse function name
                let func_name_idx = self.current;
                let func_name = if let TokenKind::Ident(ref name) = self.tokens[func_name_idx].kind
                {
                    name.clone()
                } else {
                    return Err(ParseError::UnexpectedToken {
                        expected: "function name".to_string(),
                        got: self.tokens[func_name_idx].kind.clone(),
                        span: Span::from_token(&self.tokens[func_name_idx]),
                    });
                };
                self.advance(); // consume function name

                // Parse arguments
                self.expect(TokenKind::LParen)?;
                let mut args = Vec::new();
                loop {
                    if !self.is_at_end() && matches!(self.peek().kind, TokenKind::RParen) {
                        break;
                    }
                    args.push(self.parse_expr()?);
                    if !self.is_at_end() && matches!(self.peek().kind, TokenKind::Comma) {
                        self.advance(); // consume ,
                    } else {
                        break;
                    }
                }
                self.expect(TokenKind::RParen)?;

                let callee = format!("{}::{}", keyword_name, func_name);
                let end_span = args.last().map(|a| a.span()).unwrap_or(span);
                let call_span = span.merge(&end_span);

                return Ok(Expr::Call(CallExpr {
                    callee,
                    args,
                    span: call_span,
                }));
            }
        }

        match kind {
            TokenKind::Integer(value) => {
                self.advance();
                Ok(Expr::Lit(LitExpr { value, span }))
            }
            TokenKind::True => {
                self.advance();
                Ok(Expr::BoolLiteral(BoolLiteralExpr { value: true, span }))
            }
            TokenKind::False => {
                self.advance();
                Ok(Expr::BoolLiteral(BoolLiteralExpr { value: false, span }))
            }
            TokenKind::FloatLiteral(value) => {
                self.advance();
                Ok(Expr::FloatLiteral(FloatLiteralExpr { value, span }))
            }
            TokenKind::StringLit(value) => {
                self.advance();
                Ok(Expr::StringLit(StringLitExpr { value, span }))
            }
            TokenKind::Match => {
                let match_expr = self.parse_match_expr()?;
                Ok(Expr::Match(match_expr))
            }
            TokenKind::Channel => {
                // channel<T>() is a function call, not a type
                // Check if followed by < (generic) and then ( (function call)
                let next_is_less = if self.current + 1 < self.tokens.len() {
                    matches!(self.tokens[self.current + 1].kind, TokenKind::Less)
                } else {
                    false
                };

                if next_is_less {
                    // Parse channel<T>() as a function call
                    self.advance(); // consume 'channel'
                    self.expect(TokenKind::Less)?; // consume <
                    let _elem_type = self.parse_type()?; // parse T
                    self.expect(TokenKind::Greater)?; // consume >

                    // Now check for function call parentheses
                    if !self.is_at_end() && matches!(self.peek().kind, TokenKind::LParen) {
                        self.advance(); // consume (
                        let mut args = Vec::new();
                        loop {
                            if !self.is_at_end() && matches!(self.peek().kind, TokenKind::RParen) {
                                break;
                            }
                            args.push(self.parse_expr()?);
                            if !self.is_at_end() && matches!(self.peek().kind, TokenKind::Comma) {
                                self.advance(); // consume ,
                            } else {
                                break;
                            }
                        }
                        self.expect(TokenKind::RParen)?; // consume )

                        let end_span = args.last().map(|a| a.span()).unwrap_or(span);
                        let call_span = span.merge(&end_span);

                        Ok(Expr::Call(CallExpr {
                            callee: "channel".to_string(),
                            args,
                            span: call_span,
                        }))
                    } else {
                        // channel<T> without () - this is a type, not an expression
                        Err(ParseError::UnexpectedToken {
                            expected: "function call parentheses ()".to_string(),
                            got: self.peek().kind.clone(),
                            span: Span::from_token(self.peek()),
                        })
                    }
                } else {
                    // channel without < - this is a type, not an expression
                    Err(ParseError::UnexpectedToken {
                        expected: "type parameter <T>".to_string(),
                        got: self.peek().kind.clone(),
                        span: Span::from_token(self.peek()),
                    })
                }
            }
            TokenKind::Ident(name) => {
                // Look ahead to see what follows
                let next_is_lbrace = if self.current + 1 < self.tokens.len() {
                    matches!(self.tokens[self.current + 1].kind, TokenKind::LBrace)
                } else {
                    false
                };
                let next_is_colon_colon = if self.current + 2 < self.tokens.len() {
                    matches!(self.tokens[self.current + 1].kind, TokenKind::Colon)
                        && matches!(self.tokens[self.current + 2].kind, TokenKind::Colon)
                } else {
                    false
                };
                let next_is_lparen = if self.current + 1 < self.tokens.len() {
                    matches!(self.tokens[self.current + 1].kind, TokenKind::LParen)
                } else {
                    false
                };

                // Heuristic: avoid treating `if x { ... }` or `match x { ... }` as a struct literal.
                let prev_is_if = if self.current > 0 {
                    matches!(self.tokens[self.current - 1].kind, TokenKind::If)
                } else {
                    false
                };
                let prev_is_match = if self.current > 0 {
                    matches!(self.tokens[self.current - 1].kind, TokenKind::Match)
                } else {
                    false
                };

                // Check if this is a qualified function call: mod::func(...)
                // We prefer enum literals over qualified calls, so we only treat as qualified call
                // if it's a known built-in type (Box, Vec, String) or if we can determine it's a module.
                // For now, we'll prefer enum literals and let the type checker handle the ambiguity.
                // Pattern: Ident : : Ident (
                // Token indices: current=Ident, current+1=:, current+2=:, current+3=Ident, current+4=(
                let is_qualified_call =
                    if next_is_colon_colon && self.current + 4 < self.tokens.len() {
                        // Check if current+3 is Ident and current+4 is LParen
                        let has_ident_and_paren =
                            matches!(self.tokens[self.current + 3].kind, TokenKind::Ident(_))
                                && matches!(self.tokens[self.current + 4].kind, TokenKind::LParen);

                        // Only treat as qualified call if it's a known built-in type that uses :: syntax
                        // (Box, Vec, String) - these are never enums
                        has_ident_and_paren && matches!(name.as_str(), "Box" | "Vec" | "String")
                    } else {
                        false
                    };

                if next_is_colon_colon && !is_qualified_call {
                    // Enum literal: EnumName::VariantName(...)
                    let enum_name = name;
                    self.advance(); // consume enum name
                    self.advance(); // consume first :
                    self.advance(); // consume second :

                    let variant_name_idx = self.current;
                    let variant_name = if let TokenKind::Ident(ref ident_name) =
                        self.tokens[variant_name_idx].kind
                    {
                        ident_name.clone()
                    } else {
                        return Err(ParseError::UnexpectedToken {
                            expected: "variant name".to_string(),
                            got: self.tokens[variant_name_idx].kind.clone(),
                            span: Span::from_token(&self.tokens[variant_name_idx]),
                        });
                    };
                    self.advance(); // consume variant name

                    // Check for tuple variant Enum::Variant(expr1, expr2) or struct variant Enum::Variant { field: expr }
                    let (args, named_fields) = if !self.is_at_end()
                        && matches!(self.peek().kind, TokenKind::LParen)
                    {
                        // Tuple variant: Enum::Variant(expr1, expr2)
                        self.advance(); // consume (
                        let mut args = Vec::new();
                        loop {
                            if !self.is_at_end() && matches!(self.peek().kind, TokenKind::RParen) {
                                break;
                            }
                            args.push(self.parse_expr()?);
                            if !self.is_at_end() && matches!(self.peek().kind, TokenKind::Comma) {
                                self.advance(); // consume ,
                            } else {
                                break;
                            }
                        }
                        self.expect(TokenKind::RParen)?; // consume )
                        (args, None)
                    } else if !self.is_at_end() && matches!(self.peek().kind, TokenKind::LBrace) {
                        // Struct variant: Enum::Variant { field: expr }
                        self.advance(); // consume {
                        let mut fields = Vec::new();
                        loop {
                            if !self.is_at_end() && matches!(self.peek().kind, TokenKind::RBrace) {
                                break;
                            }
                            let field_name_idx = self.current;
                            let field_name = if let TokenKind::Ident(ref ident_name) =
                                self.tokens[field_name_idx].kind
                            {
                                ident_name.clone()
                            } else {
                                return Err(ParseError::UnexpectedToken {
                                    expected: "field name".to_string(),
                                    got: self.tokens[field_name_idx].kind.clone(),
                                    span: Span::from_token(&self.tokens[field_name_idx]),
                                });
                            };
                            self.current += 1; // consume identifier
                            self.expect(TokenKind::Colon)?;
                            let field_value = self.parse_expr()?;
                            fields.push((field_name, field_value));
                            if !self.is_at_end() && matches!(self.peek().kind, TokenKind::Comma) {
                                self.advance(); // consume ,
                            } else {
                                break;
                            }
                        }
                        self.expect(TokenKind::RBrace)?; // consume }
                        (Vec::new(), Some(fields))
                    } else {
                        // Unit variant: Enum::Variant
                        (Vec::new(), None)
                    };

                    let end_span = if let Some(ref fields) = named_fields {
                        fields.last().map(|(_, e)| e.span()).unwrap_or(span)
                    } else {
                        args.last().map(|a| a.span()).unwrap_or(span)
                    };
                    let lit_span = span.merge(&end_span);

                    Ok(Expr::EnumLit(EnumLitExpr {
                        enum_name,
                        variant: variant_name,
                        args,
                        named_fields,
                        span: lit_span,
                    }))
                } else if next_is_lparen || is_qualified_call {
                    // Function call - check if it's qualified (mod::func)
                    let callee = if is_qualified_call || next_is_colon_colon {
                        // Qualified function call: mod::func(...)
                        let module_name = name;
                        self.advance(); // consume module name
                        self.advance(); // consume first :
                        self.advance(); // consume second :
                        let func_name_idx = self.current;
                        let func_name = if let TokenKind::Ident(ref func_name_str) =
                            self.tokens[func_name_idx].kind
                        {
                            func_name_str.clone()
                        } else {
                            return Err(ParseError::UnexpectedToken {
                                expected: "function name".to_string(),
                                got: self.tokens[func_name_idx].kind.clone(),
                                span: Span::from_token(&self.tokens[func_name_idx]),
                            });
                        };
                        self.advance(); // consume function name
                        format!("{}::{}", module_name, func_name)
                    } else {
                        // Simple function call
                        self.advance(); // consume function name
                        name
                    };
                    self.expect(TokenKind::LParen)?;

                    let mut args = Vec::new();
                    loop {
                        if !self.is_at_end() && matches!(self.peek().kind, TokenKind::RParen) {
                            break;
                        }
                        args.push(self.parse_expr()?);
                        if !self.is_at_end() && matches!(self.peek().kind, TokenKind::Comma) {
                            self.advance(); // consume ,
                        } else {
                            break;
                        }
                    }
                    self.expect(TokenKind::RParen)?;

                    let end_span = args.last().map(|a| a.span()).unwrap_or(span);
                    let call_span = span.merge(&end_span);

                    Ok(Expr::Call(CallExpr {
                        callee,
                        args,
                        span: call_span,
                    }))
                } else if next_is_lbrace && !prev_is_if && !prev_is_match {
                    // Lookahead: struct literal body must start with Ident or }
                    // If the token after { is a keyword (like `if`, `let`, `return`), it's likely a block.
                    let token_after_brace_idx = self.current + 2; // current=Ident, current+1={, current+2=token after {
                    let is_likely_block = if token_after_brace_idx < self.tokens.len() {
                        matches!(
                            self.tokens[token_after_brace_idx].kind,
                            TokenKind::If
                                | TokenKind::While
                                | TokenKind::Let
                                | TokenKind::Return
                                | TokenKind::Else
                                | TokenKind::Defer
                                | TokenKind::Spawn
                                | TokenKind::Unsafe
                        )
                    } else {
                        false
                    };

                    // Only treat as struct literal if it's NOT likely a block
                    // (i.e., the token after { is Ident or RBrace)
                    let is_struct_literal = !is_likely_block
                        && token_after_brace_idx < self.tokens.len()
                        && matches!(
                            self.tokens[token_after_brace_idx].kind,
                            TokenKind::Ident(_) | TokenKind::RBrace
                        );

                    if !is_struct_literal {
                        // This is likely a variable followed by a block, not a struct literal
                        self.advance(); // consume identifier
                        return Ok(Expr::Var(VarExpr { name, span }));
                    }

                    // Struct literal
                    self.advance(); // consume type name
                    self.expect(TokenKind::LBrace)?;

                    let mut fields = Vec::new();
                    while !self.is_at_end() && !matches!(self.peek().kind, TokenKind::RBrace) {
                        let field_name_idx = self.current;
                        let field_name = if let TokenKind::Ident(ref ident_name) =
                            self.tokens[field_name_idx].kind
                        {
                            ident_name.clone()
                        } else {
                            return Err(ParseError::UnexpectedToken {
                                expected: "field name".to_string(),
                                got: self.tokens[field_name_idx].kind.clone(),
                                span: Span::from_token(&self.tokens[field_name_idx]),
                            });
                        };
                        self.current += 1; // consume identifier

                        self.expect(TokenKind::Colon)?;
                        let value_expr = self.parse_expr()?;

                        let field_span = value_expr.span();
                        fields.push(StructLitField {
                            name: field_name,
                            value: value_expr,
                            span: field_span,
                        });

                        // Optional comma between fields
                        if !self.is_at_end() && matches!(self.peek().kind, TokenKind::Comma) {
                            self.advance();
                        }
                    }

                    self.expect(TokenKind::RBrace)?;

                    let end_span = fields.last().map(|f| f.span).unwrap_or(span);
                    let lit_span = span.merge(&end_span);

                    Ok(Expr::StructLit(StructLitExpr {
                        type_name: name,
                        fields,
                        span: lit_span,
                    }))
                } else {
                    // Variable - check if it's qualified (mod::item)
                    let var_name = if next_is_colon_colon {
                        // Qualified variable: mod::item
                        let module_name = name;
                        self.advance(); // consume module name
                        self.advance(); // consume first :
                        self.advance(); // consume second :
                        let item_name_idx = self.current;
                        let item_name = if let TokenKind::Ident(ref item_name_str) =
                            self.tokens[item_name_idx].kind
                        {
                            item_name_str.clone()
                        } else {
                            return Err(ParseError::UnexpectedToken {
                                expected: "item name".to_string(),
                                got: self.tokens[item_name_idx].kind.clone(),
                                span: Span::from_token(&self.tokens[item_name_idx]),
                            });
                        };
                        self.advance(); // consume item name
                        format!("{}::{}", module_name, item_name)
                    } else {
                        // Simple variable
                        self.advance(); // consume identifier
                        name
                    };
                    Ok(Expr::Var(VarExpr {
                        name: var_name,
                        span,
                    }))
                }
            }
            TokenKind::Send => {
                // send(ch, value)
                let send_token = self.advance();
                let start_span = Span::from_token(send_token);
                self.expect(TokenKind::LParen)?;
                let channel = self.parse_expr()?;
                self.expect(TokenKind::Comma)?;
                let value = self.parse_expr()?;
                self.expect(TokenKind::RParen)?;
                let end_span = value.span();
                let span = start_span.merge(&end_span);
                Ok(Expr::Send(SendExpr {
                    channel: Box::new(channel),
                    value: Box::new(value),
                    span,
                }))
            }
            TokenKind::Recv => {
                // recv(ch)
                let recv_token = self.advance();
                let start_span = Span::from_token(recv_token);
                self.expect(TokenKind::LParen)?;
                let channel = self.parse_expr()?;
                self.expect(TokenKind::RParen)?;
                let end_span = channel.span();
                let span = start_span.merge(&end_span);
                Ok(Expr::Recv(RecvExpr {
                    channel: Box::new(channel),
                    span,
                }))
            }
            TokenKind::LParen => {
                self.advance(); // consume (
                let expr = self.parse_expr()?;
                self.expect(TokenKind::RParen)?;
                Ok(expr)
            }
            TokenKind::LBracket => {
                // Array literal: [expr, expr, ...] or [expr; count]
                let start_span = span;
                self.advance(); // consume [

                // Check if it's an empty array
                if !self.is_at_end() && matches!(self.peek().kind, TokenKind::RBracket) {
                    self.advance(); // consume ]
                    let end_span = Span::from_token(self.previous());
                    return Ok(Expr::ArrayLiteral(ArrayLiteralExpr {
                        elements: Vec::new(),
                        repeat: None,
                        span: start_span.merge(&end_span),
                    }));
                }

                // Parse first expression
                let first_expr = self.parse_expr()?;

                // Check if it's [expr; count] syntax
                if !self.is_at_end() && matches!(self.peek().kind, TokenKind::Semicolon) {
                    self.advance(); // consume ;

                    // Parse count (must be integer literal)
                    let count_token = self.peek();
                    let count = if let TokenKind::Integer(count_val) = &count_token.kind {
                        let val = *count_val;
                        if val < 0 {
                            return Err(ParseError::Message(format!(
                                "Array repeat count must be non-negative, got {} at line {}, column {}",
                                val, count_token.span.line, count_token.span.column
                            )));
                        }
                        self.advance(); // consume integer
                        val as usize
                    } else {
                        return Err(ParseError::UnexpectedToken {
                            expected: "integer literal for array repeat count".to_string(),
                            got: count_token.kind.clone(),
                            span: Span::from_token(count_token),
                        });
                    };

                    self.expect(TokenKind::RBracket)?; // consume ]
                    let end_span = Span::from_token(self.previous());
                    Ok(Expr::ArrayLiteral(ArrayLiteralExpr {
                        elements: Vec::new(),
                        repeat: Some((Box::new(first_expr), count)),
                        span: start_span.merge(&end_span),
                    }))
                } else {
                    // Regular array literal: [expr, expr, ...]
                    let mut elements = vec![first_expr];
                    loop {
                        if !self.is_at_end() && matches!(self.peek().kind, TokenKind::Comma) {
                            self.advance(); // consume ,
                        } else {
                            break;
                        }
                        elements.push(self.parse_expr()?);
                    }

                    self.expect(TokenKind::RBracket)?; // consume ]
                    let end_span = Span::from_token(self.previous());
                    Ok(Expr::ArrayLiteral(ArrayLiteralExpr {
                        elements,
                        repeat: None,
                        span: start_span.merge(&end_span),
                    }))
                }
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: "expression".to_string(),
                got: kind,
                span,
            }),
        }
    }

    // Helper methods

    fn is_at_end(&self) -> bool {
        matches!(self.peek().kind, TokenKind::EOF)
    }

    fn peek(&self) -> &Token {
        if self.current < self.tokens.len() {
            &self.tokens[self.current]
        } else {
            &self.tokens[self.tokens.len() - 1] // Return last token if at end
        }
    }

    fn previous(&self) -> &Token {
        if self.current > 0 {
            &self.tokens[self.current - 1]
        } else {
            &self.tokens[0]
        }
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn expect(&mut self, kind: TokenKind) -> Result<&Token, ParseError> {
        let expected_kind_str = format!("{:?}", kind);
        let current_idx = self.current;
        if !self.is_at_end() && self.tokens[current_idx].kind == kind {
            Ok(self.advance())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: expected_kind_str,
                got: self.tokens[current_idx].kind.clone(),
                span: Span::from_token(&self.tokens[current_idx]),
            })
        }
    }
}

// Helper trait for getting span from expressions
trait HasSpan {
    fn span(&self) -> Span;
}

impl HasSpan for Expr {
    fn span(&self) -> Span {
        match self {
            Expr::Lit(e) => e.span,
            Expr::BoolLiteral(e) => e.span,
            Expr::FloatLiteral(e) => e.span,
            Expr::Var(e) => e.span,
            Expr::BinOp(e) => e.span,
            Expr::UnOp(e) => e.span,
            Expr::Ref(e) => e.span,
            Expr::Send(e) => e.span,
            Expr::Recv(e) => e.span,
            Expr::StructLit(e) => e.span,
            Expr::FieldAccess(e) => e.span,
            Expr::EnumLit(e) => e.span,
            Expr::Match(e) => e.span,
            Expr::Call(e) => e.span,
            Expr::MethodCall(e) => e.span,
            Expr::StringLit(e) => e.span,
            Expr::ArrayLiteral(e) => e.span,
            Expr::Index(e) => e.span,
            Expr::Cast(e) => e.span,
            Expr::Assign(e) => e.span,
        }
    }
}
