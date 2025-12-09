//! Z80 Code Generator for RetroLang
//!
//! Generates Z80 assembly from the AST.

use crate::ast::*;
use std::collections::HashMap;
use std::fmt::Write;

/// Serial driver selection
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SerialDriver {
    /// MC6850 ACIA (ports $80/$81) - used by kz80_forth, kz80_pascal, etc.
    Acia,
    /// Intel 8251 USART (ports $00/$01) - used by Grant's BASIC, EFEX, etc.
    Intel8251,
}

/// Symbol information for code generation
#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub ty: Type,
    pub kind: SymbolKind,
}

#[derive(Debug, Clone)]
pub enum SymbolKind {
    /// Global variable at fixed address
    Global { addr: u16 },
    /// Local variable at stack offset (relative to IX)
    Local { offset: i16 },
    /// Function parameter at stack offset
    Param { offset: i16 },
    /// Constant value
    Const { value: i32 },
}

/// Code generator state
pub struct CodeGen {
    /// Output assembly
    output: String,

    /// Global symbol table
    globals: HashMap<String, Symbol>,

    /// Current function's local symbols
    locals: HashMap<String, Symbol>,

    /// Next available global address
    next_global: u16,

    /// Current stack frame size
    frame_size: u16,

    /// Label counter for generating unique labels
    label_counter: u32,

    /// Current loop labels for break/continue
    loop_stack: Vec<(String, String)>, // (continue_label, break_label)

    /// String literals to emit in data section
    strings: Vec<(String, String)>, // (label, content)

    /// Serial driver to use for I/O
    serial_driver: SerialDriver,
}

impl CodeGen {
    pub fn new(serial_driver: SerialDriver) -> Self {
        Self {
            output: String::new(),
            globals: HashMap::new(),
            locals: HashMap::new(),
            next_global: 0x8000, // Start of RAM
            frame_size: 0,
            label_counter: 0,
            loop_stack: Vec::new(),
            strings: Vec::new(),
            serial_driver,
        }
    }

    /// Generate a unique label
    fn new_label(&mut self, prefix: &str) -> String {
        self.label_counter += 1;
        format!(".{}_{}", prefix, self.label_counter)
    }

    /// Emit a line of assembly
    fn emit(&mut self, line: &str) {
        writeln!(self.output, "{}", line).unwrap();
    }

    /// Format an IX offset with proper sign (ix+n or ix-n)
    fn ix_offset(&self, offset: i16) -> String {
        if offset >= 0 {
            format!("ix+{}", offset)
        } else {
            format!("ix{}", offset)  // negative already includes minus sign
        }
    }

    /// Emit a labeled line
    fn emit_label(&mut self, label: &str) {
        writeln!(self.output, "{}:", label).unwrap();
    }

    /// Compile a complete program
    pub fn compile(&mut self, program: &Program) -> String {
        // Emit header
        self.emit("; RetroLang compiled output");
        self.emit("; Target: Z80 / RetroShield");
        self.emit("");
        self.emit("    org $0000");
        self.emit("");

        // Jump to main, then halt
        self.emit("    call _main");
        self.emit("_halt:");
        self.emit("    halt");
        self.emit("    jp _halt");
        self.emit("");

        // Include runtime
        self.emit("; === Runtime ===");
        self.emit_runtime();
        self.emit("");

        // First pass: collect globals and constants
        for item in &program.items {
            match item {
                Item::Const(c) => self.compile_const_decl(c),
                Item::Var(v) => self.compile_global_var(v),
                _ => {}
            }
        }

        // Second pass: compile functions
        self.emit("; === Code ===");
        for item in &program.items {
            match item {
                Item::Proc(p) => self.compile_proc(p),
                Item::Func(f) => self.compile_func(f),
                _ => {}
            }
        }

        // Emit string literals
        if !self.strings.is_empty() {
            self.emit("");
            self.emit("; === String literals ===");
            for (label, content) in &self.strings.clone() {
                self.emit_label(label);
                // Emit as bytes with null terminator
                let escaped = content
                    .bytes()
                    .map(|b| format!("${:02X}", b))
                    .collect::<Vec<_>>()
                    .join(", ");
                self.emit(&format!("    db {}, 0", escaped));
            }
        }

        // Emit global variables (BSS section)
        self.emit("");
        self.emit("; === Variables ===");
        for (name, sym) in &self.globals.clone() {
            if let SymbolKind::Global { addr } = sym.kind {
                self.emit(&format!("_{}: equ ${:04X}  ; {}", name, addr, sym.ty.size()));
            }
        }

        self.output.clone()
    }

    fn compile_const_decl(&mut self, decl: &ConstDecl) {
        // Evaluate constant expression at compile time
        if let Some(value) = self.eval_const(&decl.value) {
            self.globals.insert(
                decl.name.clone(),
                Symbol {
                    name: decl.name.clone(),
                    ty: decl.ty.clone(),
                    kind: SymbolKind::Const { value },
                },
            );
        }
    }

    fn compile_global_var(&mut self, decl: &VarDecl) {
        let size = decl.ty.size();
        let addr = self.next_global;
        self.next_global += size as u16;

        self.globals.insert(
            decl.name.clone(),
            Symbol {
                name: decl.name.clone(),
                ty: decl.ty.clone(),
                kind: SymbolKind::Global { addr },
            },
        );
    }

    fn compile_proc(&mut self, proc: &ProcDecl) {
        self.emit("");
        self.emit(&format!("; proc {}", proc.name));
        self.emit_label(&format!("_{}", proc.name));

        // Set up stack frame
        self.setup_frame(&proc.params, &proc.locals);

        // Compile body
        for stmt in &proc.body {
            self.compile_stmt(stmt);
        }

        // Clean up and return
        self.cleanup_frame();
        self.emit("    ret");

        self.locals.clear();
    }

    fn compile_func(&mut self, func: &FuncDecl) {
        self.emit("");
        self.emit(&format!("; func {}", func.name));
        self.emit_label(&format!("_{}", func.name));

        // Set up stack frame
        self.setup_frame(&func.params, &func.locals);

        // Compile body
        for stmt in &func.body {
            self.compile_stmt(stmt);
        }

        // Default return (in case no explicit return)
        self.emit("    ld hl, 0");
        self.cleanup_frame();
        self.emit("    ret");

        self.locals.clear();
    }

    fn setup_frame(&mut self, params: &[Param], locals: &[VarDecl]) {
        self.emit("    push ix");
        self.emit("    ld ix, 0");
        self.emit("    add ix, sp");

        // Calculate frame size for locals
        self.frame_size = 0;
        for local in locals {
            self.frame_size += local.ty.size() as u16;
        }

        if self.frame_size > 0 {
            self.emit(&format!("    ld hl, -{}", self.frame_size));
            self.emit("    add hl, sp");
            self.emit("    ld sp, hl");
        }

        // Map parameters (above return address and saved IX)
        // Stack layout: [params...] [ret addr] [saved IX] <- IX points here
        let mut param_offset: i16 = 4; // Skip saved IX (2) and return address (2)
        for param in params.iter().rev() {
            self.locals.insert(
                param.name.clone(),
                Symbol {
                    name: param.name.clone(),
                    ty: param.ty.clone(),
                    kind: SymbolKind::Param { offset: param_offset },
                },
            );
            param_offset += param.ty.size() as i16;
        }

        // Map locals (below IX)
        let mut local_offset: i16 = 0;
        for local in locals {
            local_offset -= local.ty.size() as i16;
            self.locals.insert(
                local.name.clone(),
                Symbol {
                    name: local.name.clone(),
                    ty: local.ty.clone(),
                    kind: SymbolKind::Local { offset: local_offset },
                },
            );
        }
    }

    fn cleanup_frame(&mut self) {
        if self.frame_size > 0 {
            self.emit("    ld sp, ix");
        }
        self.emit("    pop ix");
    }

    fn compile_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Assign(lvalue, expr, _) => {
                self.compile_assign(lvalue, expr);
            }
            Stmt::If { cond, then_body, elsif_clauses, else_body, .. } => {
                self.compile_if(cond, then_body, elsif_clauses, else_body);
            }
            Stmt::While { cond, body, .. } => {
                self.compile_while(cond, body);
            }
            Stmt::For { var, start, end, downto, body, .. } => {
                self.compile_for(var, start, end, *downto, body);
            }
            Stmt::Break(_) => {
                if let Some((_, break_label)) = self.loop_stack.last() {
                    self.emit(&format!("    jp {}", break_label));
                }
            }
            Stmt::Continue(_) => {
                if let Some((continue_label, _)) = self.loop_stack.last() {
                    self.emit(&format!("    jp {}", continue_label));
                }
            }
            Stmt::Return(expr, _) => {
                if let Some(e) = expr {
                    self.compile_expr(e); // Result in HL
                }
                self.cleanup_frame();
                self.emit("    ret");
            }
            Stmt::Call(name, args, _) => {
                self.compile_call(name, args);
            }
            Stmt::Asm(text, _) => {
                // Emit inline assembly directly
                for line in text.lines() {
                    self.emit(&format!("    {}", line.trim()));
                }
            }
        }
    }

    fn compile_assign(&mut self, lvalue: &LValue, expr: &Expr) {
        // Compile expression, result in HL (or A for bytes)
        self.compile_expr(expr);

        match lvalue {
            LValue::Var(name, _) => {
                if let Some(sym) = self.lookup(name) {
                    match &sym.kind {
                        SymbolKind::Global { addr } => {
                            if sym.ty.is_word() {
                                self.emit(&format!("    ld (${:04X}), hl", addr));
                            } else {
                                self.emit("    ld a, l");
                                self.emit(&format!("    ld (${:04X}), a", addr));
                            }
                        }
                        SymbolKind::Local { offset } | SymbolKind::Param { offset } => {
                            if sym.ty.is_word() {
                                self.emit(&format!("    ld ({}), l", self.ix_offset(*offset)));
                                self.emit(&format!("    ld ({}), h", self.ix_offset(*offset + 1)));
                            } else {
                                self.emit("    ld a, l");
                                self.emit(&format!("    ld ({}), a", self.ix_offset(*offset)));
                            }
                        }
                        SymbolKind::Const { .. } => {
                            // Error: can't assign to const
                        }
                    }
                }
            }
            LValue::Index(name, index, _) => {
                // Save value
                self.emit("    push hl");
                // Compile index
                self.compile_expr(index);
                // Calculate address
                if let Some(sym) = self.lookup(name) {
                    if let SymbolKind::Global { addr } = sym.kind {
                        if let Type::Array(elem, _) = &sym.ty {
                            let elem_size = elem.size();
                            if elem_size > 1 {
                                self.emit(&format!("    ld de, {}", elem_size));
                                self.emit("    call _mul16"); // HL = HL * DE
                            }
                            self.emit(&format!("    ld de, ${:04X}", addr));
                            self.emit("    add hl, de");
                            self.emit("    ex de, hl"); // DE = address
                            self.emit("    pop hl");    // HL = value
                            if elem.is_word() {
                                self.emit("    ld a, l");
                                self.emit("    ld (de), a");
                                self.emit("    inc de");
                                self.emit("    ld a, h");
                                self.emit("    ld (de), a");
                            } else {
                                self.emit("    ld a, l");
                                self.emit("    ld (de), a");
                            }
                        }
                    }
                }
            }
            LValue::Mem(addr, _) => {
                self.emit("    push hl");
                self.compile_expr(addr);
                self.emit("    ex de, hl"); // DE = address
                self.emit("    pop hl");    // HL = value
                self.emit("    ld a, l");
                self.emit("    ld (de), a");
            }
            LValue::Memw(addr, _) => {
                self.emit("    push hl");
                self.compile_expr(addr);
                self.emit("    ex de, hl");
                self.emit("    pop hl");
                self.emit("    ld a, l");
                self.emit("    ld (de), a");
                self.emit("    inc de");
                self.emit("    ld a, h");
                self.emit("    ld (de), a");
            }
            LValue::Deref(ptr_expr, _) => {
                self.emit("    push hl");
                self.compile_expr(ptr_expr);
                self.emit("    ex de, hl");
                self.emit("    pop hl");
                self.emit("    ld a, l");
                self.emit("    ld (de), a");
            }
        }
    }

    fn compile_if(
        &mut self,
        cond: &Expr,
        then_body: &[Stmt],
        elsif_clauses: &[(Expr, Vec<Stmt>)],
        else_body: &Option<Vec<Stmt>>,
    ) {
        let end_label = self.new_label("endif");

        // Compile condition
        self.compile_expr(cond);
        self.emit("    ld a, l");
        self.emit("    or a");

        if elsif_clauses.is_empty() && else_body.is_none() {
            // Simple if
            self.emit(&format!("    jp z, {}", end_label));
            for stmt in then_body {
                self.compile_stmt(stmt);
            }
        } else {
            let else_label = self.new_label("else");
            self.emit(&format!("    jp z, {}", else_label));

            for stmt in then_body {
                self.compile_stmt(stmt);
            }
            self.emit(&format!("    jp {}", end_label));

            self.emit_label(&else_label);

            // Elsif clauses
            for (i, (elsif_cond, elsif_body)) in elsif_clauses.iter().enumerate() {
                self.compile_expr(elsif_cond);
                self.emit("    ld a, l");
                self.emit("    or a");

                let next_label = if i + 1 < elsif_clauses.len() || else_body.is_some() {
                    self.new_label("elsif")
                } else {
                    end_label.clone()
                };

                self.emit(&format!("    jp z, {}", next_label));

                for stmt in elsif_body {
                    self.compile_stmt(stmt);
                }
                self.emit(&format!("    jp {}", end_label));

                if i + 1 < elsif_clauses.len() || else_body.is_some() {
                    self.emit_label(&next_label);
                }
            }

            // Else clause
            if let Some(else_stmts) = else_body {
                for stmt in else_stmts {
                    self.compile_stmt(stmt);
                }
            }
        }

        self.emit_label(&end_label);
    }

    fn compile_while(&mut self, cond: &Expr, body: &[Stmt]) {
        let loop_label = self.new_label("while");
        let end_label = self.new_label("endwhile");

        self.loop_stack.push((loop_label.clone(), end_label.clone()));

        self.emit_label(&loop_label);
        self.compile_expr(cond);
        self.emit("    ld a, l");
        self.emit("    or a");
        self.emit(&format!("    jp z, {}", end_label));

        for stmt in body {
            self.compile_stmt(stmt);
        }

        self.emit(&format!("    jp {}", loop_label));
        self.emit_label(&end_label);

        self.loop_stack.pop();
    }

    fn compile_for(&mut self, var: &str, start: &Expr, end: &Expr, downto: bool, body: &[Stmt]) {
        let loop_label = self.new_label("for");
        let continue_label = self.new_label("forcont");
        let end_label = self.new_label("endfor");

        // Initialize loop variable
        self.compile_expr(start);
        if let Some(sym) = self.lookup(var) {
            self.store_to_symbol(&sym);
        }

        self.loop_stack.push((continue_label.clone(), end_label.clone()));

        self.emit_label(&loop_label);

        // Check condition
        self.compile_expr(end);
        self.emit("    push hl");
        if let Some(sym) = self.lookup(var) {
            self.load_from_symbol(&sym);
        }
        self.emit("    pop de");

        if downto {
            // var >= end
            self.emit("    or a");
            self.emit("    sbc hl, de");
            self.emit(&format!("    jp c, {}", end_label));
        } else {
            // var <= end
            self.emit("    ex de, hl");
            self.emit("    or a");
            self.emit("    sbc hl, de");
            self.emit(&format!("    jp c, {}", end_label));
        }

        // Body
        for stmt in body {
            self.compile_stmt(stmt);
        }

        // Increment/decrement
        self.emit_label(&continue_label);
        if let Some(sym) = self.lookup(var) {
            self.load_from_symbol(&sym);
            if downto {
                self.emit("    dec hl");
            } else {
                self.emit("    inc hl");
            }
            self.store_to_symbol(&sym);
        }

        self.emit(&format!("    jp {}", loop_label));
        self.emit_label(&end_label);

        self.loop_stack.pop();
    }

    fn compile_call(&mut self, name: &str, args: &[Expr]) {
        // Push arguments right to left
        for arg in args.iter().rev() {
            self.compile_expr(arg);
            self.emit("    push hl");
        }

        self.emit(&format!("    call _{}", name));

        // Clean up stack without clobbering return value in HL
        if !args.is_empty() {
            let bytes = args.len() * 2; // All args are 2 bytes on stack
            self.emit("    ex de, hl");  // Save return value to DE
            self.emit(&format!("    ld hl, {}", bytes));
            self.emit("    add hl, sp");
            self.emit("    ld sp, hl");
            self.emit("    ex de, hl");  // Restore return value to HL
        }
    }

    fn compile_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::IntLit(n, _) => {
                self.emit(&format!("    ld hl, {}", n));
            }
            Expr::BoolLit(b, _) => {
                self.emit(&format!("    ld hl, {}", if *b { 1 } else { 0 }));
            }
            Expr::CharLit(c, _) => {
                self.emit(&format!("    ld hl, {}", c));
            }
            Expr::StringLit(s, _) => {
                // Use global label for strings (underscore prefix, no dot)
                self.label_counter += 1;
                let label = format!("_str_{}", self.label_counter);
                self.strings.push((label.clone(), s.clone()));
                self.emit(&format!("    ld hl, {}", label));
            }
            Expr::Var(name, _) => {
                if let Some(sym) = self.lookup(name) {
                    self.load_from_symbol(&sym);
                }
            }
            Expr::Index(arr, idx, _) => {
                // Compile index
                self.compile_expr(idx);
                // Get array base and calculate address
                if let Expr::Var(name, _) = arr.as_ref() {
                    if let Some(sym) = self.lookup(name) {
                        if let SymbolKind::Global { addr } = sym.kind {
                            if let Type::Array(elem, _) = &sym.ty {
                                let elem_size = elem.size();
                                if elem_size > 1 {
                                    self.emit(&format!("    ld de, {}", elem_size));
                                    self.emit("    call _mul16");
                                }
                                self.emit(&format!("    ld de, ${:04X}", addr));
                                self.emit("    add hl, de");
                                if elem.is_word() {
                                    self.emit("    ld e, (hl)");
                                    self.emit("    inc hl");
                                    self.emit("    ld d, (hl)");
                                    self.emit("    ex de, hl");
                                } else {
                                    self.emit("    ld l, (hl)");
                                    self.emit("    ld h, 0");
                                }
                            }
                        }
                    }
                }
            }
            Expr::AddrOf(name, _) => {
                if let Some(sym) = self.lookup(name) {
                    match sym.kind {
                        SymbolKind::Global { addr } => {
                            self.emit(&format!("    ld hl, ${:04X}", addr));
                        }
                        SymbolKind::Local { offset } | SymbolKind::Param { offset } => {
                            self.emit("    push ix");
                            self.emit("    pop hl");
                            self.emit(&format!("    ld de, {}", offset));
                            self.emit("    add hl, de");
                        }
                        _ => {}
                    }
                }
            }
            Expr::Deref(ptr, _) => {
                self.compile_expr(ptr);
                self.emit("    ld a, (hl)");
                self.emit("    ld l, a");
                self.emit("    ld h, 0");
            }
            Expr::Mem(addr, _) => {
                self.compile_expr(addr);
                self.emit("    ld a, (hl)");
                self.emit("    ld l, a");
                self.emit("    ld h, 0");
            }
            Expr::Memw(addr, _) => {
                self.compile_expr(addr);
                self.emit("    ld e, (hl)");
                self.emit("    inc hl");
                self.emit("    ld d, (hl)");
                self.emit("    ex de, hl");
            }
            Expr::Binary(left, op, right, _) => {
                self.compile_binary(left, *op, right);
            }
            Expr::Unary(op, inner, _) => {
                self.compile_expr(inner);
                match op {
                    UnaryOp::Neg => {
                        self.emit("    xor a");
                        self.emit("    sub l");
                        self.emit("    ld l, a");
                        self.emit("    sbc a, a");
                        self.emit("    sub h");
                        self.emit("    ld h, a");
                    }
                    UnaryOp::Not => {
                        let skip = self.new_label("skip");
                        self.emit("    ld a, l");
                        self.emit("    or h");
                        self.emit("    ld hl, 0");
                        self.emit(&format!("    jr nz, {}", skip));
                        self.emit("    inc l");
                        self.emit_label(&skip);
                    }
                    UnaryOp::BitNot => {
                        self.emit("    ld a, l");
                        self.emit("    cpl");
                        self.emit("    ld l, a");
                        self.emit("    ld a, h");
                        self.emit("    cpl");
                        self.emit("    ld h, a");
                    }
                }
            }
            Expr::Call(name, args, _) => {
                self.compile_call(name, args);
                // Result is already in HL
            }
        }
    }

    fn compile_binary(&mut self, left: &Expr, op: BinOp, right: &Expr) {
        // Compile right first, push, then left
        self.compile_expr(right);
        self.emit("    push hl");
        self.compile_expr(left);
        self.emit("    pop de"); // DE = right, HL = left

        match op {
            BinOp::Add => {
                self.emit("    add hl, de");
            }
            BinOp::Sub => {
                self.emit("    or a");
                self.emit("    sbc hl, de");
            }
            BinOp::Mul => {
                self.emit("    call _mul16");
            }
            BinOp::Div => {
                self.emit("    call _div16");
            }
            BinOp::Mod => {
                self.emit("    call _mod16");
            }
            BinOp::Eq => {
                let skip = self.new_label("skip");
                self.emit("    or a");
                self.emit("    sbc hl, de");
                self.emit("    ld hl, 0");
                self.emit(&format!("    jr nz, {}", skip));
                self.emit("    inc l");
                self.emit_label(&skip);
            }
            BinOp::Neq => {
                let skip = self.new_label("skip");
                self.emit("    or a");
                self.emit("    sbc hl, de");
                self.emit("    ld a, h");
                self.emit("    or l");
                self.emit("    ld hl, 0");
                self.emit(&format!("    jr z, {}", skip));
                self.emit("    inc l");
                self.emit_label(&skip);
            }
            BinOp::Lt => {
                let skip = self.new_label("skip");
                self.emit("    call _cmp16");
                self.emit("    ld hl, 0");
                self.emit(&format!("    jr nc, {}", skip));
                self.emit("    inc l");
                self.emit_label(&skip);
            }
            BinOp::Gt => {
                let skip = self.new_label("skip");
                self.emit("    ex de, hl");
                self.emit("    call _cmp16");
                self.emit("    ld hl, 0");
                self.emit(&format!("    jr nc, {}", skip));
                self.emit("    inc l");
                self.emit_label(&skip);
            }
            BinOp::Le => {
                let skip = self.new_label("skip");
                self.emit("    ex de, hl");
                self.emit("    call _cmp16");
                self.emit("    ld hl, 1");
                self.emit(&format!("    jr nc, {}", skip));
                self.emit("    dec l");
                self.emit_label(&skip);
            }
            BinOp::Ge => {
                let skip = self.new_label("skip");
                self.emit("    call _cmp16");
                self.emit("    ld hl, 1");
                self.emit(&format!("    jr nc, {}", skip));
                self.emit("    dec l");
                self.emit_label(&skip);
            }
            BinOp::And => {
                let false_label = self.new_label("and_false");
                let end_label = self.new_label("and_end");
                self.emit("    ld a, h");
                self.emit("    or l");
                self.emit(&format!("    jr z, {}", false_label));
                self.emit("    ld a, d");
                self.emit("    or e");
                self.emit(&format!("    jr z, {}", false_label));
                self.emit("    ld hl, 1");
                self.emit(&format!("    jr {}", end_label));
                self.emit_label(&false_label);
                self.emit("    ld hl, 0");
                self.emit_label(&end_label);
            }
            BinOp::Or => {
                let skip = self.new_label("skip");
                self.emit("    ld a, h");
                self.emit("    or l");
                self.emit("    or d");
                self.emit("    or e");
                self.emit("    ld hl, 0");
                self.emit(&format!("    jr z, {}", skip));
                self.emit("    inc l");
                self.emit_label(&skip);
            }
            BinOp::BitAnd => {
                self.emit("    ld a, l");
                self.emit("    and e");
                self.emit("    ld l, a");
                self.emit("    ld a, h");
                self.emit("    and d");
                self.emit("    ld h, a");
            }
            BinOp::BitOr => {
                self.emit("    ld a, l");
                self.emit("    or e");
                self.emit("    ld l, a");
                self.emit("    ld a, h");
                self.emit("    or d");
                self.emit("    ld h, a");
            }
            BinOp::BitXor => {
                self.emit("    ld a, l");
                self.emit("    xor e");
                self.emit("    ld l, a");
                self.emit("    ld a, h");
                self.emit("    xor d");
                self.emit("    ld h, a");
            }
            BinOp::Shl => {
                // HL << DE (use E as count)
                let loop_label = self.new_label("shl_loop");
                let done_label = self.new_label("shl_done");
                self.emit("    ld a, e");
                self.emit_label(&loop_label);
                self.emit("    or a");
                self.emit(&format!("    jr z, {}", done_label));
                self.emit("    add hl, hl");
                self.emit("    dec a");
                self.emit(&format!("    jr {}", loop_label));
                self.emit_label(&done_label);
            }
            BinOp::Shr => {
                // HL >> DE (use E as count)
                let loop_label = self.new_label("shr_loop");
                let done_label = self.new_label("shr_done");
                self.emit("    ld a, e");
                self.emit_label(&loop_label);
                self.emit("    or a");
                self.emit(&format!("    jr z, {}", done_label));
                self.emit("    srl h");
                self.emit("    rr l");
                self.emit("    dec a");
                self.emit(&format!("    jr {}", loop_label));
                self.emit_label(&done_label);
            }
        }
    }

    fn lookup(&self, name: &str) -> Option<Symbol> {
        self.locals.get(name).cloned().or_else(|| self.globals.get(name).cloned())
    }

    fn load_from_symbol(&mut self, sym: &Symbol) {
        match &sym.kind {
            SymbolKind::Global { addr } => {
                if sym.ty.is_word() {
                    self.emit(&format!("    ld hl, (${:04X})", addr));
                } else {
                    self.emit(&format!("    ld a, (${:04X})", addr));
                    self.emit("    ld l, a");
                    self.emit("    ld h, 0");
                }
            }
            SymbolKind::Local { offset } | SymbolKind::Param { offset } => {
                if sym.ty.is_word() {
                    self.emit(&format!("    ld l, ({})", self.ix_offset(*offset)));
                    self.emit(&format!("    ld h, ({})", self.ix_offset(*offset + 1)));
                } else {
                    self.emit(&format!("    ld l, ({})", self.ix_offset(*offset)));
                    self.emit("    ld h, 0");
                }
            }
            SymbolKind::Const { value } => {
                self.emit(&format!("    ld hl, {}", value));
            }
        }
    }

    fn store_to_symbol(&mut self, sym: &Symbol) {
        match &sym.kind {
            SymbolKind::Global { addr } => {
                if sym.ty.is_word() {
                    self.emit(&format!("    ld (${:04X}), hl", addr));
                } else {
                    self.emit("    ld a, l");
                    self.emit(&format!("    ld (${:04X}), a", addr));
                }
            }
            SymbolKind::Local { offset } | SymbolKind::Param { offset } => {
                if sym.ty.is_word() {
                    self.emit(&format!("    ld ({}), l", self.ix_offset(*offset)));
                    self.emit(&format!("    ld ({}), h", self.ix_offset(*offset + 1)));
                } else {
                    self.emit("    ld a, l");
                    self.emit(&format!("    ld ({}), a", self.ix_offset(*offset)));
                }
            }
            SymbolKind::Const { .. } => {
                // Can't store to const
            }
        }
    }

    fn eval_const(&self, expr: &Expr) -> Option<i32> {
        match expr {
            Expr::IntLit(n, _) => Some(*n),
            Expr::BoolLit(b, _) => Some(if *b { 1 } else { 0 }),
            Expr::CharLit(c, _) => Some(*c as i32),
            Expr::Binary(left, op, right, _) => {
                let l = self.eval_const(left)?;
                let r = self.eval_const(right)?;
                Some(match op {
                    BinOp::Add => l + r,
                    BinOp::Sub => l - r,
                    BinOp::Mul => l * r,
                    BinOp::Div => l / r,
                    BinOp::Mod => l % r,
                    BinOp::BitAnd => l & r,
                    BinOp::BitOr => l | r,
                    BinOp::BitXor => l ^ r,
                    BinOp::Shl => l << r,
                    BinOp::Shr => l >> r,
                    _ => return None,
                })
            }
            Expr::Unary(op, inner, _) => {
                let v = self.eval_const(inner)?;
                Some(match op {
                    UnaryOp::Neg => -v,
                    UnaryOp::BitNot => !v,
                    UnaryOp::Not => if v == 0 { 1 } else { 0 },
                })
            }
            _ => None,
        }
    }

    fn emit_runtime(&mut self) {
        // 16-bit multiply: HL = HL * DE
        self.emit("_mul16:");
        self.emit("    push bc");
        self.emit("    ld b, h");
        self.emit("    ld c, l");
        self.emit("    ld hl, 0");
        self.emit("    ld a, 16");
        self.emit(".mul_loop:");
        self.emit("    add hl, hl");
        self.emit("    rl c");
        self.emit("    rl b");
        self.emit("    jr nc, .mul_skip");
        self.emit("    add hl, de");
        self.emit(".mul_skip:");
        self.emit("    dec a");
        self.emit("    jr nz, .mul_loop");
        self.emit("    pop bc");
        self.emit("    ret");
        self.emit("");

        // 16-bit divide: HL = HL / DE, remainder in BC
        self.emit("_div16:");
        self.emit("    push bc");
        self.emit("    ld bc, 0");
        self.emit("    ld a, 16");
        self.emit(".div_loop:");
        self.emit("    add hl, hl");
        self.emit("    rl c");
        self.emit("    rl b");
        self.emit("    push hl");
        self.emit("    ld h, b");
        self.emit("    ld l, c");
        self.emit("    or a");
        self.emit("    sbc hl, de");
        self.emit("    jr c, .div_skip");
        self.emit("    ld b, h");
        self.emit("    ld c, l");
        self.emit("    pop hl");
        self.emit("    inc l");
        self.emit("    jr .div_next");
        self.emit(".div_skip:");
        self.emit("    pop hl");
        self.emit(".div_next:");
        self.emit("    dec a");
        self.emit("    jr nz, .div_loop");
        self.emit("    pop bc");
        self.emit("    ret");
        self.emit("");

        // 16-bit modulo: HL = HL % DE
        self.emit("_mod16:");
        self.emit("    call _div16");
        self.emit("    push bc");
        self.emit("    pop hl");
        self.emit("    ret");
        self.emit("");

        // 16-bit signed compare: carry set if HL < DE
        self.emit("_cmp16:");
        self.emit("    ld a, h");
        self.emit("    xor d");
        self.emit("    jp p, .cmp_same_sign");
        self.emit("    ld a, h");
        self.emit("    or a");
        self.emit("    ret");  // If H is neg (bit 7 set), carry clear; else carry set
        self.emit(".cmp_same_sign:");
        self.emit("    or a");
        self.emit("    sbc hl, de");
        self.emit("    add hl, de");  // Restore HL
        self.emit("    ret");
        self.emit("");

        // Serial I/O routines for RetroShield
        match self.serial_driver {
            SerialDriver::Acia => {
                // MC6850 ACIA: Control/Status at $80, Data at $81
                // Status bits: 0=RDRF (rx ready), 1=TDRE (tx ready)
                self.emit("; MC6850 ACIA I/O (RetroShield Z80)");
                self.emit("SERIAL_CTRL equ $80");
                self.emit("SERIAL_DATA equ $81");
                self.emit("TX_READY    equ 1");  // Bit 1 = TDRE
                self.emit("RX_READY    equ 0");  // Bit 0 = RDRF
            }
            SerialDriver::Intel8251 => {
                // Intel 8251 USART: Data at $00, Control/Status at $01
                // Status bits: 0=TxRDY (tx ready), 1=RxRDY (rx ready)
                self.emit("; Intel 8251 USART I/O (RetroShield Z80)");
                self.emit("SERIAL_DATA equ $00");
                self.emit("SERIAL_CTRL equ $01");
                self.emit("TX_READY    equ 0");  // Bit 0 = TxRDY
                self.emit("RX_READY    equ 1");  // Bit 1 = RxRDY
            }
        }
        self.emit("");

        self.emit("_printc:");
        self.emit("    ld a, l");
        self.emit("    push af");
        self.emit(".printc_wait:");
        self.emit("    in a, (SERIAL_CTRL)");
        self.emit("    bit TX_READY, a");
        self.emit("    jr z, .printc_wait");
        self.emit("    pop af");
        self.emit("    out (SERIAL_DATA), a");
        self.emit("    ret");
        self.emit("");

        self.emit("_print:");
        self.emit("    ; HL = pointer to null-terminated string");
        self.emit(".print_loop:");
        self.emit("    ld a, (hl)");
        self.emit("    or a");
        self.emit("    ret z");
        self.emit("    push hl");
        self.emit("    ld l, a");
        self.emit("    call _printc");
        self.emit("    pop hl");
        self.emit("    inc hl");
        self.emit("    jr .print_loop");
        self.emit("");

        self.emit("_println:");
        self.emit("    ld l, 13");
        self.emit("    call _printc");
        self.emit("    ld l, 10");
        self.emit("    call _printc");
        self.emit("    ret");
        self.emit("");

        self.emit("_readc:");
        self.emit(".readc_wait:");
        self.emit("    in a, (SERIAL_CTRL)");
        self.emit("    bit RX_READY, a");
        self.emit("    jr z, .readc_wait");
        self.emit("    in a, (SERIAL_DATA)");
        self.emit("    ld l, a");
        self.emit("    ld h, 0");
        self.emit("    ret");
        self.emit("");

        // Print integer
        self.emit("_printi:");
        self.emit("    ; HL = integer to print");
        self.emit("    bit 7, h");
        self.emit("    jr z, .printi_pos");
        self.emit("    push hl");
        self.emit("    ld l, '-'");
        self.emit("    call _printc");
        self.emit("    pop hl");
        self.emit("    ; Negate HL");
        self.emit("    xor a");
        self.emit("    sub l");
        self.emit("    ld l, a");
        self.emit("    sbc a, a");
        self.emit("    sub h");
        self.emit("    ld h, a");
        self.emit(".printi_pos:");
        self.emit("    ; Convert to decimal");
        self.emit("    ld de, 10000");
        self.emit("    call .printi_digit");
        self.emit("    ld de, 1000");
        self.emit("    call .printi_digit");
        self.emit("    ld de, 100");
        self.emit("    call .printi_digit");
        self.emit("    ld de, 10");
        self.emit("    call .printi_digit");
        self.emit("    ld a, l");
        self.emit("    add a, '0'");
        self.emit("    ld l, a");
        self.emit("    jp _printc");
        self.emit(".printi_digit:");
        self.emit("    ld b, '0' - 1");
        self.emit(".printi_sub:");
        self.emit("    inc b");
        self.emit("    or a");
        self.emit("    sbc hl, de");
        self.emit("    jr nc, .printi_sub");
        self.emit("    add hl, de");
        self.emit("    ld a, b");
        self.emit("    cp '0'");
        self.emit("    ret z");  // Skip leading zeros
        self.emit("    push hl");
        self.emit("    ld l, a");
        self.emit("    call _printc");
        self.emit("    pop hl");
        self.emit("    ret");
        self.emit("");
    }
}

impl Default for CodeGen {
    fn default() -> Self {
        Self::new(SerialDriver::Acia)
    }
}
