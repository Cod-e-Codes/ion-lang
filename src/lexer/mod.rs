#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Keywords
    Fn,
    Let,
    Struct,
    Enum,
    If,
    Else,
    Return,
    Channel,
    Send,
    Recv,
    Spawn,
    Defer,
    Int,
    Bool,
    F32,
    F64,
    I8,
    I16,
    I32,
    I64,
    U16,
    U32,
    U64,
    UInt,
    Mut,
    While,
    For,
    Match,
    Box,
    Vec,
    String,
    Pub,
    Import,
    Extern,
    Unsafe,
    Type,
    As,
    True,
    False,

    // Operators
    Plus,         // +
    Minus,        // -
    Star,         // *
    Slash,        // /
    Percent,      // %
    Dot,          // .
    Ellipsis,     // ...
    Equals,       // = (assignment)
    EqualsEquals, // == (equality)
    NotEquals,    // !=
    Arrow,        // ->
    Ampersand,    // &
    AndAnd,       // &&
    OrOr,         // ||
    Pipe,         // | (bitwise OR)
    Caret,        // ^ (bitwise XOR)
    ShiftLeft,    // <<
    ShiftRight,   // >>
    Not,          // !
    Less,         // <
    Greater,      // >
    LessEqual,    // <=
    GreaterEqual, // >=

    // Delimiters
    LParen,    // (
    RParen,    // )
    LBrace,    // {
    RBrace,    // }
    LBracket,  // [
    RBracket,  // ]
    Semicolon, // ;
    Colon,     // :
    Comma,     // ,

    // Literals
    Integer(i64),
    FloatLiteral(f64),
    StringLit(String),

    // Identifiers
    Ident(String),

    // Special
    EOF,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
}

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect(),
            position: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();

        while !self.is_at_end() {
            self.skip_whitespace();
            if self.is_at_end() {
                break;
            }

            let start = self.position;
            let line = self.line;
            let col = self.column;

            let kind = match self.peek() {
                Some('+') => {
                    self.advance();
                    TokenKind::Plus
                }
                Some('-') => {
                    self.advance();
                    if self.peek() == Some('>') {
                        self.advance();
                        TokenKind::Arrow
                    } else {
                        TokenKind::Minus
                    }
                }
                Some('*') => {
                    self.advance();
                    TokenKind::Star
                }
                Some('.') => {
                    // Check for ellipsis (...)
                    if self.peek_next() == Some('.')
                        && self.input.get(self.position + 2) == Some(&'.')
                    {
                        self.advance(); // consume first .
                        self.advance(); // consume second .
                        self.advance(); // consume third .
                        TokenKind::Ellipsis
                    } else if let Some(next_char) = self.peek_next() {
                        // Check if next character is a digit - if so, parse as float literal starting with .
                        if next_char.is_ascii_digit() {
                            // Parse float literal starting with decimal point (e.g., .5)
                            self.advance(); // consume the .
                            self.read_float_from_dot()?
                        } else {
                            // Just a dot operator
                            self.advance();
                            TokenKind::Dot
                        }
                    } else {
                        // Just a dot operator
                        self.advance();
                        TokenKind::Dot
                    }
                }
                Some('/') => {
                    self.advance();
                    if self.peek() == Some('/') {
                        // Line comment - skip to end of line
                        while self.peek() != Some('\n') && !self.is_at_end() {
                            self.advance();
                        }
                        continue; // Skip adding this as a token
                    } else {
                        TokenKind::Slash
                    }
                }
                Some('%') => {
                    self.advance();
                    TokenKind::Percent
                }
                Some('=') => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        TokenKind::EqualsEquals // ==
                    } else if self.peek() == Some('>') {
                        self.advance();
                        TokenKind::Arrow
                    } else {
                        TokenKind::Equals // = (assignment)
                    }
                }
                Some('!') => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        TokenKind::NotEquals
                    } else {
                        TokenKind::Not
                    }
                }
                Some('&') => {
                    self.advance();
                    if self.peek() == Some('&') {
                        self.advance();
                        TokenKind::AndAnd
                    } else {
                        TokenKind::Ampersand
                    }
                }
                Some('|') => {
                    self.advance();
                    if self.peek() == Some('|') {
                        self.advance();
                        TokenKind::OrOr
                    } else {
                        TokenKind::Pipe
                    }
                }
                Some('^') => {
                    self.advance();
                    TokenKind::Caret
                }
                Some('<') => {
                    self.advance();
                    if self.peek() == Some('<') {
                        self.advance();
                        TokenKind::ShiftLeft
                    } else if self.peek() == Some('=') {
                        self.advance();
                        TokenKind::LessEqual
                    } else {
                        TokenKind::Less
                    }
                }
                Some('>') => {
                    self.advance();
                    if self.peek() == Some('>') {
                        self.advance();
                        TokenKind::ShiftRight
                    } else if self.peek() == Some('=') {
                        self.advance();
                        TokenKind::GreaterEqual
                    } else {
                        TokenKind::Greater
                    }
                }
                Some('(') => {
                    self.advance();
                    TokenKind::LParen
                }
                Some(')') => {
                    self.advance();
                    TokenKind::RParen
                }
                Some('{') => {
                    self.advance();
                    TokenKind::LBrace
                }
                Some('}') => {
                    self.advance();
                    TokenKind::RBrace
                }
                Some('[') => {
                    self.advance();
                    TokenKind::LBracket
                }
                Some(']') => {
                    self.advance();
                    TokenKind::RBracket
                }
                Some(';') => {
                    self.advance();
                    TokenKind::Semicolon
                }
                Some(':') => {
                    self.advance();
                    TokenKind::Colon
                }
                Some(',') => {
                    self.advance();
                    TokenKind::Comma
                }
                Some('"') => TokenKind::StringLit(self.read_string()?),
                Some(c) if c.is_ascii_digit() => self.read_number()?,
                Some(c) if c.is_ascii_alphabetic() || c == '_' => self.read_identifier_or_keyword(),
                Some(c) => {
                    return Err(format!(
                        "Unexpected character '{}' at line {}, column {}",
                        c, self.line, self.column
                    ));
                }
                None => {
                    // End of input - should be handled by is_at_end check, but just in case
                    break;
                }
            };

            let end = self.position;
            tokens.push(Token {
                kind,
                span: Span {
                    start,
                    end,
                    line,
                    column: col,
                },
            });
        }

        tokens.push(Token {
            kind: TokenKind::EOF,
            span: Span {
                start: self.position,
                end: self.position,
                line: self.line,
                column: self.column,
            },
        });

        Ok(tokens)
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.input.len()
    }

    fn peek(&self) -> Option<char> {
        self.input.get(self.position).copied()
    }

    fn peek_next(&self) -> Option<char> {
        self.input.get(self.position + 1).copied()
    }

    fn advance(&mut self) -> Option<char> {
        if let Some(c) = self.peek() {
            self.position += 1;
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            Some(c)
        } else {
            None
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn read_number(&mut self) -> Result<TokenKind, String> {
        let start_pos = self.position;
        let mut integer_part = 0i64;
        let mut has_decimal = false;
        let mut fractional_part = 0.0;
        let mut fractional_digits = 0;
        let mut has_exponent = false;
        let mut exponent_sign = 1i32;
        let mut exponent = 0i32;

        // Read integer part (or nothing if starting with .)
        while let Some(c) = self.peek() {
            if let Some(digit) = c.to_digit(10) {
                integer_part = integer_part
                    .checked_mul(10)
                    .and_then(|v| v.checked_add(digit as i64))
                    .ok_or_else(|| {
                        format!(
                            "Integer overflow at line {}, column {}",
                            self.line, self.column
                        )
                    })?;
                self.advance();
            } else if c == '.' {
                // Decimal point
                self.advance();
                has_decimal = true;
                break;
            } else if c == 'e' || c == 'E' {
                // Scientific notation (no decimal point)
                self.advance();
                has_exponent = true;
                break;
            } else {
                break;
            }
        }

        // Read fractional part
        if has_decimal {
            while let Some(c) = self.peek() {
                if let Some(digit) = c.to_digit(10) {
                    fractional_part = fractional_part * 10.0 + digit as f64;
                    fractional_digits += 1;
                    self.advance();
                } else if c == 'e' || c == 'E' {
                    // Scientific notation
                    self.advance();
                    has_exponent = true;
                    break;
                } else {
                    // End of fractional part (e.g., "3." - valid float literal)
                    break;
                }
            }
            // Normalize fractional part
            if fractional_digits > 0 {
                fractional_part /= 10.0_f64.powi(fractional_digits);
            }
            // Note: Even if fractional_digits == 0, this is still a float (e.g., "3.")
        }

        // Read exponent
        if has_exponent {
            // Check for sign
            if let Some(c) = self.peek() {
                if c == '+' {
                    self.advance();
                } else if c == '-' {
                    exponent_sign = -1;
                    self.advance();
                }
            }

            // Read exponent digits
            while let Some(c) = self.peek() {
                if let Some(digit) = c.to_digit(10) {
                    exponent = exponent
                        .checked_mul(10)
                        .and_then(|v| v.checked_add(digit as i32))
                        .ok_or_else(|| {
                            format!(
                                "Exponent overflow at line {}, column {}",
                                self.line, self.column
                            )
                        })?;
                    self.advance();
                } else {
                    break;
                }
            }
        }

        if self.position == start_pos {
            Err(format!(
                "Expected digit at line {}, column {}",
                self.line, self.column
            ))
        } else if has_decimal || has_exponent {
            // It's a float
            let mut float_value = integer_part as f64 + fractional_part;
            if has_exponent {
                float_value *= 10.0_f64.powi(exponent * exponent_sign);
            }
            Ok(TokenKind::FloatLiteral(float_value))
        } else {
            // It's an integer
            Ok(TokenKind::Integer(integer_part))
        }
    }

    fn read_float_from_dot(&mut self) -> Result<TokenKind, String> {
        // We've already consumed the '.', now read the fractional part
        let mut fractional_part = 0.0;
        let mut fractional_digits = 0;
        let mut has_exponent = false;
        let mut exponent_sign = 1i32;
        let mut exponent = 0i32;

        // Read fractional part
        while let Some(c) = self.peek() {
            if let Some(digit) = c.to_digit(10) {
                fractional_part = fractional_part * 10.0 + digit as f64;
                fractional_digits += 1;
                self.advance();
            } else if c == 'e' || c == 'E' {
                // Scientific notation
                self.advance();
                has_exponent = true;
                break;
            } else {
                break;
            }
        }

        // Normalize fractional part
        if fractional_digits > 0 {
            fractional_part /= 10.0_f64.powi(fractional_digits);
        }

        // Read exponent if present
        if has_exponent {
            // Check for sign
            if let Some(c) = self.peek() {
                if c == '+' {
                    self.advance();
                } else if c == '-' {
                    exponent_sign = -1;
                    self.advance();
                }
            }

            // Read exponent digits
            while let Some(c) = self.peek() {
                if let Some(digit) = c.to_digit(10) {
                    exponent = exponent
                        .checked_mul(10)
                        .and_then(|v| v.checked_add(digit as i32))
                        .ok_or_else(|| {
                            format!(
                                "Exponent overflow at line {}, column {}",
                                self.line, self.column
                            )
                        })?;
                    self.advance();
                } else {
                    break;
                }
            }
        }

        if fractional_digits == 0 && !has_exponent {
            Err(format!(
                "Expected digit after decimal point at line {}, column {}",
                self.line, self.column
            ))
        } else {
            let mut float_value = fractional_part;
            if has_exponent {
                float_value *= 10.0_f64.powi(exponent * exponent_sign);
            }
            Ok(TokenKind::FloatLiteral(float_value))
        }
    }

    fn read_string(&mut self) -> Result<String, String> {
        // Consume opening quote
        self.advance();
        let mut result = String::new();

        while !self.is_at_end() {
            match self.peek() {
                Some('"') => {
                    // Closing quote
                    self.advance();
                    return Ok(result);
                }
                Some('\\') => {
                    // Escape sequence
                    self.advance(); // consume backslash
                    match self.peek() {
                        Some('"') => {
                            result.push('"');
                            self.advance();
                        }
                        Some('\\') => {
                            result.push('\\');
                            self.advance();
                        }
                        Some('n') => {
                            result.push('\n');
                            self.advance();
                        }
                        Some('t') => {
                            result.push('\t');
                            self.advance();
                        }
                        Some('r') => {
                            result.push('\r');
                            self.advance();
                        }
                        Some('0') => {
                            result.push('\0');
                            self.advance();
                        }
                        Some(c) => {
                            return Err(format!(
                                "Invalid escape sequence '\\{}' at line {}, column {}",
                                c, self.line, self.column
                            ));
                        }
                        None => {
                            return Err(format!(
                                "Unterminated escape sequence at line {}, column {}",
                                self.line, self.column
                            ));
                        }
                    }
                }
                Some('\n') => {
                    return Err(format!(
                        "Unterminated string literal at line {}, column {}",
                        self.line, self.column
                    ));
                }
                Some(c) => {
                    result.push(c);
                    self.advance();
                }
                None => {
                    return Err(format!(
                        "Unterminated string literal at line {}, column {}",
                        self.line, self.column
                    ));
                }
            }
        }

        Err(format!(
            "Unterminated string literal at line {}, column {}",
            self.line, self.column
        ))
    }

    fn read_identifier_or_keyword(&mut self) -> TokenKind {
        let start = self.position;
        while let Some(c) = self.peek() {
            if c.is_ascii_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let text: String = self.input[start..self.position].iter().collect();

        match text.as_str() {
            "fn" => TokenKind::Fn,
            "let" => TokenKind::Let,
            "struct" => TokenKind::Struct,
            "enum" => TokenKind::Enum,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "return" => TokenKind::Return,
            "channel" => TokenKind::Channel,
            "send" => TokenKind::Send,
            "recv" => TokenKind::Recv,
            "spawn" => TokenKind::Spawn,
            "defer" => TokenKind::Defer,
            "int" => TokenKind::Int,
            "bool" => TokenKind::Bool,
            "f32" => TokenKind::F32,
            "f64" => TokenKind::F64,
            "i8" => TokenKind::I8,
            "i16" => TokenKind::I16,
            "i32" => TokenKind::I32,
            "i64" => TokenKind::I64,
            "u16" => TokenKind::U16,
            "u32" => TokenKind::U32,
            "u64" => TokenKind::U64,
            "uint" => TokenKind::UInt,
            "mut" => TokenKind::Mut,
            "while" => TokenKind::While,
            "for" => TokenKind::For,
            "match" => TokenKind::Match,
            "Box" => TokenKind::Box,
            "Vec" => TokenKind::Vec,
            "String" => TokenKind::String,
            "pub" => TokenKind::Pub,
            "import" => TokenKind::Import,
            "extern" => TokenKind::Extern,
            "unsafe" => TokenKind::Unsafe,
            "type" => TokenKind::Type,
            "as" => TokenKind::As,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            _ => TokenKind::Ident(text),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("fn let return int");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Fn);
        assert_eq!(tokens[1].kind, TokenKind::Let);
        assert_eq!(tokens[2].kind, TokenKind::Return);
        assert_eq!(tokens[3].kind, TokenKind::Int);
    }

    #[test]
    fn test_operators() {
        let mut lexer = Lexer::new("+ - * / = ->");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Plus);
        assert_eq!(tokens[1].kind, TokenKind::Minus);
        assert_eq!(tokens[2].kind, TokenKind::Star);
        assert_eq!(tokens[3].kind, TokenKind::Slash);
        assert_eq!(tokens[4].kind, TokenKind::Equals);
        assert_eq!(tokens[5].kind, TokenKind::Arrow);
    }

    #[test]
    fn test_integer_literals() {
        let mut lexer = Lexer::new("0 42 100");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Integer(0));
        assert_eq!(tokens[1].kind, TokenKind::Integer(42));
        assert_eq!(tokens[2].kind, TokenKind::Integer(100));
    }

    #[test]
    fn test_identifiers() {
        let mut lexer = Lexer::new("main x y");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Ident("main".to_string()));
        assert_eq!(tokens[1].kind, TokenKind::Ident("x".to_string()));
        assert_eq!(tokens[2].kind, TokenKind::Ident("y".to_string()));
    }

    #[test]
    fn test_delimiters() {
        let mut lexer = Lexer::new("() {} ; :");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::LParen);
        assert_eq!(tokens[1].kind, TokenKind::RParen);
        assert_eq!(tokens[2].kind, TokenKind::LBrace);
        assert_eq!(tokens[3].kind, TokenKind::RBrace);
        assert_eq!(tokens[4].kind, TokenKind::Semicolon);
        assert_eq!(tokens[5].kind, TokenKind::Colon);
    }

    #[test]
    fn test_comments() {
        let mut lexer = Lexer::new("fn main // comment\nlet x");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Fn);
        assert_eq!(tokens[1].kind, TokenKind::Ident("main".to_string()));
        assert_eq!(tokens[2].kind, TokenKind::Let);
        assert_eq!(tokens[3].kind, TokenKind::Ident("x".to_string()));
    }
}
