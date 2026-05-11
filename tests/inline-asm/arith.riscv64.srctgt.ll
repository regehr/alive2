; TEST-ARGS: -disable-undef-input -disable-poison-input
; SKIP-IDENTITY
target triple = "riscv64-unknown-linux-gnu"
target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"

; ---- add_reg ----
define i64 @src(i64 %a, i64 %b) {
  %r = add i64 %a, %b
  ret i64 %r
}

define i64 @tgt(i64 %a, i64 %b) {
  %r = call i64 asm "add $0, $1, $2", "=r,r,r"(i64 %a, i64 %b)
  ret i64 %r
}

; ---- sub_reg ----
define i64 @src_sub_reg(i64 %a, i64 %b) {
  %r = sub i64 %a, %b
  ret i64 %r
}

define i64 @tgt_sub_reg(i64 %a, i64 %b) {
  %r = call i64 asm "sub $0, $1, $2", "=r,r,r"(i64 %a, i64 %b)
  ret i64 %r
}

; ---- addi ----
define i64 @src_addi(i64 %a) {
  %r = add i64 %a, 77
  ret i64 %r
}

define i64 @tgt_addi(i64 %a) {
  %r = call i64 asm "addi $0, $1, 77", "=r,r"(i64 %a)
  ret i64 %r
}

; ---- addw ----
define i64 @src_addw(i64 %a, i64 %b) {
  %a32 = trunc i64 %a to i32
  %b32 = trunc i64 %b to i32
  %sum = add i32 %a32, %b32
  %r = sext i32 %sum to i64
  ret i64 %r
}

define i64 @tgt_addw(i64 %a, i64 %b) {
  %r = call i64 asm "addw $0, $1, $2", "=r,r,r"(i64 %a, i64 %b)
  ret i64 %r
}

; ---- mul ----
define i64 @src_mul(i64 %a, i64 %b) {
  %r = mul i64 %a, %b
  ret i64 %r
}

define i64 @tgt_mul(i64 %a, i64 %b) {
  %r = call i64 asm "mul $0, $1, $2", "=r,r,r"(i64 %a, i64 %b)
  ret i64 %r
}
