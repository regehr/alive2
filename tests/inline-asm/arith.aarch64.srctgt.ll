; TEST-ARGS: -disable-undef-input -disable-poison-input
; SKIP-IDENTITY
target triple = "aarch64-unknown-linux-gnu"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32"

; ---- add_reg ----
define i32 @src(i32 %a, i32 %b) {
  %r = add i32 %a, %b
  ret i32 %r
}

define i32 @tgt(i32 %a, i32 %b) {
  %r = call i32 asm "add ${0:w}, ${1:w}, ${2:w}", "=r,r,r"(i32 %a, i32 %b)
  ret i32 %r
}

; ---- sub_reg ----
define i32 @src_sub_reg(i32 %a, i32 %b) {
  %r = sub i32 %a, %b
  ret i32 %r
}

define i32 @tgt_sub_reg(i32 %a, i32 %b) {
  %r = call i32 asm "sub ${0:w}, ${1:w}, ${2:w}", "=r,r,r"(i32 %a, i32 %b)
  ret i32 %r
}

; ---- mul_reg ----
define i32 @src_mul_reg(i32 %a, i32 %b) {
  %r = mul i32 %a, %b
  ret i32 %r
}

define i32 @tgt_mul_reg(i32 %a, i32 %b) {
  %r = call i32 asm "mul ${0:w}, ${1:w}, ${2:w}", "=r,r,r"(i32 %a, i32 %b)
  ret i32 %r
}

; ---- neg ----
define i32 @src_neg(i32 %a) {
  %r = sub i32 0, %a
  ret i32 %r
}

define i32 @tgt_neg(i32 %a) {
  %r = call i32 asm "neg ${0:w}, ${1:w}", "=r,r"(i32 %a)
  ret i32 %r
}

; ---- add_imm ----
define i32 @src_add_imm(i32 %a) {
  %r = add i32 %a, 42
  ret i32 %r
}

define i32 @tgt_add_imm(i32 %a) {
  %r = call i32 asm "add ${0:w}, ${1:w}, #42", "=r,r"(i32 %a)
  ret i32 %r
}

; ---- sub_imm ----
define i32 @src_sub_imm(i32 %a) {
  %r = sub i32 %a, 100
  ret i32 %r
}

define i32 @tgt_sub_imm(i32 %a) {
  %r = call i32 asm "sub ${0:w}, ${1:w}, #100", "=r,r"(i32 %a)
  ret i32 %r
}

; ---- add64 ----
define i64 @src_add64(i64 %a, i64 %b) {
  %r = add i64 %a, %b
  ret i64 %r
}

define i64 @tgt_add64(i64 %a, i64 %b) {
  %r = call i64 asm "add $0, $1, $2", "=r,r,r"(i64 %a, i64 %b)
  ret i64 %r
}

; ---- sub64 ----
define i64 @src_sub64(i64 %a, i64 %b) {
  %r = sub i64 %a, %b
  ret i64 %r
}

define i64 @tgt_sub64(i64 %a, i64 %b) {
  %r = call i64 asm "sub $0, $1, $2", "=r,r,r"(i64 %a, i64 %b)
  ret i64 %r
}

; ---- mul64 ----
define i64 @src_mul64(i64 %a, i64 %b) {
  %r = mul i64 %a, %b
  ret i64 %r
}

define i64 @tgt_mul64(i64 %a, i64 %b) {
  %r = call i64 asm "mul $0, $1, $2", "=r,r,r"(i64 %a, i64 %b)
  ret i64 %r
}

; ---- add_shifted ----
define i64 @src_add_shifted(i64 %a, i64 %b) {
  %shifted = shl i64 %b, 2
  %r = add i64 %a, %shifted
  ret i64 %r
}

define i64 @tgt_add_shifted(i64 %a, i64 %b) {
  %r = call i64 asm "add $0, $1, $2, lsl #2", "=r,r,r"(i64 %a, i64 %b)
  ret i64 %r
}

; ---- madd ----
define i32 @src_madd(i32 %a, i32 %b, i32 %c) {
  %mul = mul i32 %b, %c
  %r = add i32 %a, %mul
  ret i32 %r
}

define i32 @tgt_madd(i32 %a, i32 %b, i32 %c) {
  %r = call i32 asm "madd ${0:w}, ${1:w}, ${2:w}, ${3:w}", "=r,r,r,r"(i32 %b, i32 %c, i32 %a)
  ret i32 %r
}

; ---- multi_inst ----
define i32 @src_multi_inst(i32 %a, i32 %b) {
  %sum = add i32 %a, %b
  %r = sub i32 %sum, %b
  ret i32 %r
}

define i32 @tgt_multi_inst(i32 %a, i32 %b) {
  %r = call i32 asm "add ${0:w}, ${1:w}, ${2:w}\0Asub ${0:w}, ${0:w}, ${2:w}", "=&r,r,r"(i32 %a, i32 %b)
  ret i32 %r
}

; ---- tied_operand ----
define i32 @src_tied_operand(i32 %x) {
  %r = add i32 %x, 1
  ret i32 %r
}

define i32 @tgt_tied_operand(i32 %x) {
  %r = call i32 asm "add ${0:w}, ${1:w}, #1", "=r,0"(i32 %x)
  ret i32 %r
}
