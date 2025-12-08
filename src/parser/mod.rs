//! Parser for RetroLang
//!
//! Recursive descent parser that builds an AST from tokens.

use crate::ast::*;
use crate::lexer::{Span, SpannedToken, Token};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Unexpected token: expected {expected}, found {found} at position {pos}")]
    UnexpectedToken {
        expected: String,
        found: String,
        pos: usize,
    },

    #[error("Unexpected end of input")]
    UnexpectedEof,

    #[error("Invalid type at position {0}")]
    InvalidType(usize),

    #[error("Invalid expression at position {0}")]
    InvalidExpr(usize),

    #[error("Expected statement at position {0}")]
    ExpectedStmt(usize),
}

pub struct Parser {
    tokens: Vec<SpannedToken>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<SpannedToken>) -> Self {
        // Filter out newlines for easier parsing (we don't need them)
        let tokens: Vec<_> = tokens
            .into_iter()
            .filter(|t| !matches!(t.token, Token::Newline))
            .collect();
        Self { tokens, pos: 0 }
    }

    /// Parse a complete program
    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut items = Vec::new();

        while !self.is_at_end() {
            items.push(self.parse_item()?);
        }

        Ok(Program { items })
    }

    /// Parse a top-level item
    fn parse_item(&mut self) -> Result<Item, ParseError> {
        match self.peek_token()? {
            Token::Const => Ok(Item::Const(self.parse_const_decl()?)),
            Token::Var => Ok(Item::Var(self.parse_var_decl()?)),
            Token::Proc => Ok(Item::Proc(self.parse_proc_decl()?)),
            Token::Func => Ok(Item::Func(self.parse_func_decl()?)),
            _ => Err(ParseError::UnexpectedToken {
                expected: "const, var, proc, or func".to_string(),
                found: format!("{}", self.peek_token()?),
                pos: self.current_pos(),
            }),
        }
    }

    /// Parse: const NAME: TYPE = VALUE;
    fn parse_const_decl(&mut self) -> Result<ConstDecl, ParseError> {
        let start = self.current_pos();
        self.expect(Token::Const)?;
        let name = self.parse_ident()?;
        self.expect(Token::Colon)?;
        let ty = self.parse_type()?;
        self.expect(Token::Eq)?;
        let value = self.parse_expr()?;
        self.expect(Token::Semi)?;

        Ok(ConstDecl {
            name,
            ty,
            value,
            span: Span::new(start, self.current_pos()),
        })
    }

    /// Parse: var NAME: TYPE [= VALUE];
    fn parse_var_decl(&mut self) -> Result<VarDecl, ParseError> {
        let start = self.current_pos();
        self.expect(Token::Var)?;
        let name = self.parse_ident()?;
        self.expect(Token::Colon)?;
        let ty = self.parse_type()?;

        let init = if self.check(&Token::Eq) {
            self.advance();
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.expect(Token::Semi)?;

        Ok(VarDecl {
            name,
            ty,
            init,
            span: Span::new(start, self.current_pos()),
        })
    }

    /// Parse: proc NAME(params) ... end;
    fn parse_proc_decl(&mut self) -> Result<ProcDecl, ParseError> {
        let start = self.current_pos();
        self.expect(Token::Proc)?;
        let name = self.parse_ident()?;
        let params = self.parse_params()?;

        // Parse local variables
        let mut locals = Vec::new();
        while self.check(&Token::Var) {
            locals.push(self.parse_var_decl()?);
        }

        // Parse body statements
        let body = self.parse_stmt_list()?;

        self.expect(Token::End)?;
        self.expect(Token::Semi)?;

        Ok(ProcDecl {
            name,
            params,
            locals,
            body,
            span: Span::new(start, self.current_pos()),
        })
    }

    /// Parse: func NAME(params): TYPE ... end;
    fn parse_func_decl(&mut self) -> Result<FuncDecl, ParseError> {
        let start = self.current_pos();
        self.expect(Token::Func)?;
        let name = self.parse_ident()?;
        let params = self.parse_params()?;
        self.expect(Token::Colon)?;
        let return_type = self.parse_type()?;

        // Parse local variables
        let mut locals = Vec::new();
        while self.check(&Token::Var) {
            locals.push(self.parse_var_decl()?);
        }

        // Parse body statements
        let body = self.parse_stmt_list()?;

        self.expect(Token::End)?;
        self.expect(Token::Semi)?;

        Ok(FuncDecl {
            name,
            params,
            return_type,
            locals,
            body,
            span: Span::new(start, self.current_pos()),
        })
    }

    /// Parse parameter list: (name: type, name: type, ...)
    fn parse_params(&mut self) -> Result<Vec<Param>, ParseError> {
        self.expect(Token::LParen)?;
        let mut params = Vec::new();

        if !self.check(&Token::RParen) {
            loop {
                let start = self.current_pos();
                let name = self.parse_ident()?;
                self.expect(Token::Colon)?;
                let ty = self.parse_type()?;
                params.push(Param {
                    name,
                    ty,
                    span: Span::new(start, self.current_pos()),
                });

                if !self.check(&Token::Comma) {
                    break;
                }
                self.advance();
            }
        }

        self.expect(Token::RParen)?;
        Ok(params)
    }

    /// Parse a type
    fn parse_type(&mut self) -> Result<Type, ParseError> {
        // Check for pointer prefix
        if self.check(&Token::Caret) {
            self.advance();
            let inner = self.parse_type()?;
            return Ok(Type::Pointer(Box::new(inner)));
        }

        let base = match self.advance_token()? {
            Token::TypeByte => Type::Byte,
            Token::TypeInt => Type::Int,
            Token::TypeBool => Type::Bool,
            Token::TypeChar => Type::Char,
            _ => return Err(ParseError::InvalidType(self.current_pos())),
        };

        // Check for array suffix
        if self.check(&Token::LBracket) {
            self.advance();
            let size = match self.advance_token()? {
                Token::IntLit(n) => n as usize,
                Token::HexLit(n) => n as usize,
                _ => return Err(ParseError::InvalidType(self.current_pos())),
            };
            self.expect(Token::RBracket)?;
            return Ok(Type::Array(Box::new(base), size));
        }

        Ok(base)
    }

    /// Parse a list of statements until 'end', 'else', or 'elsif'
    fn parse_stmt_list(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();

        while !self.is_at_end() {
            match self.peek_token()? {
                Token::End | Token::Else | Token::Elsif => break,
                _ => stmts.push(self.parse_stmt()?),
            }
        }

        Ok(stmts)
    }

    /// Parse a single statement
    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        let start = self.current_pos();

        match self.peek_token()? {
            Token::If => self.parse_if_stmt(),
            Token::While => self.parse_while_stmt(),
            Token::For => self.parse_for_stmt(),
            Token::Break => {
                self.advance();
                self.expect(Token::Semi)?;
                Ok(Stmt::Break(Span::new(start, self.current_pos())))
            }
            Token::Continue => {
                self.advance();
                self.expect(Token::Semi)?;
                Ok(Stmt::Continue(Span::new(start, self.current_pos())))
            }
            Token::Return => {
                self.advance();
                let expr = if !self.check(&Token::Semi) {
                    Some(self.parse_expr()?)
                } else {
                    None
                };
                self.expect(Token::Semi)?;
                Ok(Stmt::Return(expr, Span::new(start, self.current_pos())))
            }
            Token::Asm => self.parse_asm_stmt(),
            Token::Ident(_) => self.parse_assign_or_call(),
            Token::Mem | Token::Memw => self.parse_mem_assign(),
            _ => Err(ParseError::ExpectedStmt(start)),
        }
    }

    /// Parse if statement
    fn parse_if_stmt(&mut self) -> Result<Stmt, ParseError> {
        let start = self.current_pos();
        self.expect(Token::If)?;
        let cond = self.parse_expr()?;
        self.expect(Token::Then)?;
        let then_body = self.parse_stmt_list()?;

        let mut elsif_clauses = Vec::new();
        while self.check(&Token::Elsif) {
            self.advance();
            let elsif_cond = self.parse_expr()?;
            self.expect(Token::Then)?;
            let elsif_body = self.parse_stmt_list()?;
            elsif_clauses.push((elsif_cond, elsif_body));
        }

        let else_body = if self.check(&Token::Else) {
            self.advance();
            Some(self.parse_stmt_list()?)
        } else {
            None
        };

        self.expect(Token::End)?;
        self.expect(Token::Semi)?;

        Ok(Stmt::If {
            cond,
            then_body,
            elsif_clauses,
            else_body,
            span: Span::new(start, self.current_pos()),
        })
    }

    /// Parse while statement
    fn parse_while_stmt(&mut self) -> Result<Stmt, ParseError> {
        let start = self.current_pos();
        self.expect(Token::While)?;
        let cond = self.parse_expr()?;
        self.expect(Token::Do)?;
        let body = self.parse_stmt_list()?;
        self.expect(Token::End)?;
        self.expect(Token::Semi)?;

        Ok(Stmt::While {
            cond,
            body,
            span: Span::new(start, self.current_pos()),
        })
    }

    /// Parse for statement
    fn parse_for_stmt(&mut self) -> Result<Stmt, ParseError> {
        let start = self.current_pos();
        self.expect(Token::For)?;
        let var = self.parse_ident()?;
        self.expect(Token::Assign)?;
        let start_expr = self.parse_expr()?;

        let downto = if self.check(&Token::To) {
            self.advance();
            false
        } else if self.check(&Token::Downto) {
            self.advance();
            true
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: "to or downto".to_string(),
                found: format!("{}", self.peek_token()?),
                pos: self.current_pos(),
            });
        };

        let end_expr = self.parse_expr()?;
        self.expect(Token::Do)?;
        let body = self.parse_stmt_list()?;
        self.expect(Token::End)?;
        self.expect(Token::Semi)?;

        Ok(Stmt::For {
            var,
            start: start_expr,
            end: end_expr,
            downto,
            body,
            span: Span::new(start, self.current_pos()),
        })
    }

    /// Parse inline assembly
    fn parse_asm_stmt(&mut self) -> Result<Stmt, ParseError> {
        let start = self.current_pos();
        self.expect(Token::Asm)?;

        // For now, just collect everything until 'end' as a string
        // A more sophisticated version would handle {var} substitutions
        let mut asm_text = String::new();

        while !self.check(&Token::End) && !self.is_at_end() {
            let tok = self.advance_token()?;
            asm_text.push_str(&format!("{} ", tok));
        }

        self.expect(Token::End)?;
        self.expect(Token::Semi)?;

        Ok(Stmt::Asm(asm_text.trim().to_string(), Span::new(start, self.current_pos())))
    }

    /// Parse assignment or procedure call
    fn parse_assign_or_call(&mut self) -> Result<Stmt, ParseError> {
        let start = self.current_pos();
        let name = self.parse_ident()?;

        // Check for array index
        if self.check(&Token::LBracket) {
            self.advance();
            let index = self.parse_expr()?;
            self.expect(Token::RBracket)?;
            self.expect(Token::Assign)?;
            let value = self.parse_expr()?;
            self.expect(Token::Semi)?;
            return Ok(Stmt::Assign(
                LValue::Index(name, index, Span::new(start, self.current_pos())),
                value,
                Span::new(start, self.current_pos()),
            ));
        }

        // Check for procedure call
        if self.check(&Token::LParen) {
            let args = self.parse_args()?;
            self.expect(Token::Semi)?;
            return Ok(Stmt::Call(name, args, Span::new(start, self.current_pos())));
        }

        // Must be simple assignment
        self.expect(Token::Assign)?;
        let value = self.parse_expr()?;
        self.expect(Token::Semi)?;

        Ok(Stmt::Assign(
            LValue::Var(name, Span::new(start, self.current_pos())),
            value,
            Span::new(start, self.current_pos()),
        ))
    }

    /// Parse memory assignment: mem[addr] := value;
    fn parse_mem_assign(&mut self) -> Result<Stmt, ParseError> {
        let start = self.current_pos();
        let is_word = self.check(&Token::Memw);
        self.advance(); // consume mem or memw
        self.expect(Token::LBracket)?;
        let addr = self.parse_expr()?;
        self.expect(Token::RBracket)?;
        self.expect(Token::Assign)?;
        let value = self.parse_expr()?;
        self.expect(Token::Semi)?;

        let lvalue = if is_word {
            LValue::Memw(addr, Span::new(start, self.current_pos()))
        } else {
            LValue::Mem(addr, Span::new(start, self.current_pos()))
        };

        Ok(Stmt::Assign(lvalue, value, Span::new(start, self.current_pos())))
    }

    /// Parse argument list: (expr, expr, ...)
    fn parse_args(&mut self) -> Result<Vec<Expr>, ParseError> {
        self.expect(Token::LParen)?;
        let mut args = Vec::new();

        if !self.check(&Token::RParen) {
            loop {
                args.push(self.parse_expr()?);
                if !self.check(&Token::Comma) {
                    break;
                }
                self.advance();
            }
        }

        self.expect(Token::RParen)?;
        Ok(args)
    }

    /// Parse expression using precedence climbing
    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_expr_prec(0)
    }

    fn parse_expr_prec(&mut self, min_prec: u8) -> Result<Expr, ParseError> {
        let mut left = self.parse_unary()?;

        while let Some(op) = self.peek_binop() {
            let prec = op.precedence();
            if prec < min_prec {
                break;
            }

            self.advance(); // consume operator
            let right = self.parse_expr_prec(prec + 1)?;
            let span = Span::new(left.span().start, right.span().end);
            left = Expr::Binary(Box::new(left), op, Box::new(right), span);
        }

        Ok(left)
    }

    fn peek_binop(&self) -> Option<BinOp> {
        if self.is_at_end() {
            return None;
        }
        match &self.tokens[self.pos].token {
            Token::Plus => Some(BinOp::Add),
            Token::Minus => Some(BinOp::Sub),
            Token::Star => Some(BinOp::Mul),
            Token::Slash => Some(BinOp::Div),
            Token::Percent => Some(BinOp::Mod),
            Token::Eq => Some(BinOp::Eq),
            Token::Neq => Some(BinOp::Neq),
            Token::Lt => Some(BinOp::Lt),
            Token::Gt => Some(BinOp::Gt),
            Token::Le => Some(BinOp::Le),
            Token::Ge => Some(BinOp::Ge),
            Token::And => Some(BinOp::And),
            Token::Or => Some(BinOp::Or),
            Token::Ampersand => Some(BinOp::BitAnd),
            Token::Pipe => Some(BinOp::BitOr),
            Token::Caret => Some(BinOp::BitXor),
            Token::Shl => Some(BinOp::Shl),
            Token::Shr => Some(BinOp::Shr),
            _ => None,
        }
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        let start = self.current_pos();

        if self.check(&Token::Minus) {
            self.advance();
            let inner = self.parse_unary()?;
            let span = Span::new(start, inner.span().end);
            return Ok(Expr::Unary(UnaryOp::Neg, Box::new(inner), span));
        }

        if self.check(&Token::Not) {
            self.advance();
            let inner = self.parse_unary()?;
            let span = Span::new(start, inner.span().end);
            return Ok(Expr::Unary(UnaryOp::Not, Box::new(inner), span));
        }

        if self.check(&Token::Tilde) {
            self.advance();
            let inner = self.parse_unary()?;
            let span = Span::new(start, inner.span().end);
            return Ok(Expr::Unary(UnaryOp::BitNot, Box::new(inner), span));
        }

        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.check(&Token::LBracket) {
                self.advance();
                let index = self.parse_expr()?;
                self.expect(Token::RBracket)?;
                let span = Span::new(expr.span().start, self.current_pos());
                expr = Expr::Index(Box::new(expr), Box::new(index), span);
            } else if self.check(&Token::Caret) {
                self.advance();
                let span = Span::new(expr.span().start, self.current_pos());
                expr = Expr::Deref(Box::new(expr), span);
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        let start = self.current_pos();

        match self.peek_token()? {
            Token::IntLit(n) => {
                let n = *n;
                self.advance();
                Ok(Expr::IntLit(n, Span::new(start, self.current_pos())))
            }
            Token::HexLit(n) | Token::BinLit(n) => {
                let n = *n;
                self.advance();
                Ok(Expr::IntLit(n, Span::new(start, self.current_pos())))
            }
            Token::CharLit(c) => {
                let c = *c;
                self.advance();
                Ok(Expr::CharLit(c, Span::new(start, self.current_pos())))
            }
            Token::StringLit(s) => {
                let s = s.clone();
                self.advance();
                Ok(Expr::StringLit(s, Span::new(start, self.current_pos())))
            }
            Token::True => {
                self.advance();
                Ok(Expr::BoolLit(true, Span::new(start, self.current_pos())))
            }
            Token::False => {
                self.advance();
                Ok(Expr::BoolLit(false, Span::new(start, self.current_pos())))
            }
            Token::At => {
                self.advance();
                let name = self.parse_ident()?;
                Ok(Expr::AddrOf(name, Span::new(start, self.current_pos())))
            }
            Token::Mem => {
                self.advance();
                self.expect(Token::LBracket)?;
                let addr = self.parse_expr()?;
                self.expect(Token::RBracket)?;
                Ok(Expr::Mem(Box::new(addr), Span::new(start, self.current_pos())))
            }
            Token::Memw => {
                self.advance();
                self.expect(Token::LBracket)?;
                let addr = self.parse_expr()?;
                self.expect(Token::RBracket)?;
                Ok(Expr::Memw(Box::new(addr), Span::new(start, self.current_pos())))
            }
            Token::LParen => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            Token::Ident(_) => {
                let name = self.parse_ident()?;
                // Check for function call
                if self.check(&Token::LParen) {
                    let args = self.parse_args()?;
                    Ok(Expr::Call(name, args, Span::new(start, self.current_pos())))
                } else {
                    Ok(Expr::Var(name, Span::new(start, self.current_pos())))
                }
            }
            _ => Err(ParseError::InvalidExpr(start)),
        }
    }

    // === Helper methods ===

    fn parse_ident(&mut self) -> Result<String, ParseError> {
        match self.advance_token()? {
            Token::Ident(s) => Ok(s),
            tok => Err(ParseError::UnexpectedToken {
                expected: "identifier".to_string(),
                found: format!("{}", tok),
                pos: self.current_pos(),
            }),
        }
    }

    fn expect(&mut self, expected: Token) -> Result<(), ParseError> {
        let tok = self.advance_token()?;
        if std::mem::discriminant(&tok) == std::mem::discriminant(&expected) {
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: format!("{}", expected),
                found: format!("{}", tok),
                pos: self.current_pos(),
            })
        }
    }

    fn check(&self, expected: &Token) -> bool {
        if self.is_at_end() {
            return false;
        }
        std::mem::discriminant(&self.tokens[self.pos].token) == std::mem::discriminant(expected)
    }

    fn advance(&mut self) {
        if !self.is_at_end() {
            self.pos += 1;
        }
    }

    fn advance_token(&mut self) -> Result<Token, ParseError> {
        if self.is_at_end() {
            return Err(ParseError::UnexpectedEof);
        }
        let tok = self.tokens[self.pos].token.clone();
        self.pos += 1;
        Ok(tok)
    }

    fn peek_token(&self) -> Result<&Token, ParseError> {
        if self.is_at_end() {
            return Err(ParseError::UnexpectedEof);
        }
        Ok(&self.tokens[self.pos].token)
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn current_pos(&self) -> usize {
        if self.pos < self.tokens.len() {
            self.tokens[self.pos].span.start
        } else if !self.tokens.is_empty() {
            self.tokens.last().unwrap().span.end
        } else {
            0
        }
    }
}

/// Convenience function to parse source code
pub fn parse(source: &str) -> Result<Program, ParseError> {
    let tokens = crate::lexer::tokenize(source);
    let mut parser = Parser::new(tokens);
    parser.parse_program()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_const() {
        let program = parse("const X: int = 42;").unwrap();
        assert_eq!(program.items.len(), 1);
        match &program.items[0] {
            Item::Const(c) => {
                assert_eq!(c.name, "X");
                assert_eq!(c.ty, Type::Int);
            }
            _ => panic!("Expected const"),
        }
    }

    #[test]
    fn test_parse_var() {
        let program = parse("var x: byte = 0xFF;").unwrap();
        assert_eq!(program.items.len(), 1);
        match &program.items[0] {
            Item::Var(v) => {
                assert_eq!(v.name, "x");
                assert_eq!(v.ty, Type::Byte);
                assert!(v.init.is_some());
            }
            _ => panic!("Expected var"),
        }
    }

    #[test]
    fn test_parse_proc() {
        let src = r#"
            proc hello()
                var x: int;
                x := 42;
            end;
        "#;
        let program = parse(src).unwrap();
        assert_eq!(program.items.len(), 1);
        match &program.items[0] {
            Item::Proc(p) => {
                assert_eq!(p.name, "hello");
                assert_eq!(p.locals.len(), 1);
                assert_eq!(p.body.len(), 1);
            }
            _ => panic!("Expected proc"),
        }
    }

    #[test]
    fn test_parse_func() {
        let src = r#"
            func add(a: int, b: int): int
                return a + b;
            end;
        "#;
        let program = parse(src).unwrap();
        match &program.items[0] {
            Item::Func(f) => {
                assert_eq!(f.name, "add");
                assert_eq!(f.params.len(), 2);
                assert_eq!(f.return_type, Type::Int);
            }
            _ => panic!("Expected func"),
        }
    }

    #[test]
    fn test_parse_if() {
        let src = r#"
            proc test()
                if x > 0 then
                    y := 1;
                elsif x < 0 then
                    y := -1;
                else
                    y := 0;
                end;
            end;
        "#;
        let program = parse(src).unwrap();
        assert!(matches!(&program.items[0], Item::Proc(_)));
    }

    #[test]
    fn test_parse_while() {
        let src = r#"
            proc test()
                while x > 0 do
                    x := x - 1;
                end;
            end;
        "#;
        let program = parse(src).unwrap();
        assert!(matches!(&program.items[0], Item::Proc(_)));
    }

    #[test]
    fn test_parse_for() {
        let src = r#"
            proc test()
                for i := 0 to 10 do
                    x := x + 1;
                end;
            end;
        "#;
        let program = parse(src).unwrap();
        assert!(matches!(&program.items[0], Item::Proc(_)));
    }

    #[test]
    fn test_parse_expr_precedence() {
        let src = "const X: int = 1 + 2 * 3;";
        let program = parse(src).unwrap();
        match &program.items[0] {
            Item::Const(c) => {
                // Should be 1 + (2 * 3), not (1 + 2) * 3
                match &c.value {
                    Expr::Binary(_, BinOp::Add, right, _) => {
                        assert!(matches!(right.as_ref(), Expr::Binary(_, BinOp::Mul, _, _)));
                    }
                    _ => panic!("Expected binary add"),
                }
            }
            _ => panic!("Expected const"),
        }
    }
}
