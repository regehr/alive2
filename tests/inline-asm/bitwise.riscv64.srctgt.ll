; TEST-ARGS: -disable-undef-input -disable-poison-input
; SKIP-IDENTITY
target triple = "riscv64-unknown-linux-gnu"
target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"

; ---- and_reg ----
define i64 @src(i64 %a, i64 %b) {
  %r = and i64 %a, %b
  ret i64 %r
}

define i64 @tgt(i64 %a, i64 %b) {
  %r = call i64 asm "and $0, $1, $2", "=r,r,r"(i64 %a, i64 %b)
  ret i64 %r
}

; ---- or_reg ----
define i64 @src_or_reg(i64 %a, i64 %b) {
  %r = or i64 %a, %b
  ret i64 %r
}

define i64 @tgt_or_reg(i64 %a, i64 %b) {
  %r = call i64 asm "or $0, $1, $2", "=r,r,r"(i64 %a, i64 %b)
  ret i64 %r
}

; ---- xor_reg ----
define i64 @src_xor_reg(i64 %a, i64 %b) {
  %r = xor i64 %a, %b
  ret i64 %r
}

define i64 @tgt_xor_reg(i64 %a, i64 %b) {
  %r = call i64 asm "xor $0, $1, $2", "=r,r,r"(i64 %a, i64 %b)
  ret i64 %r
}

; ---- andn ----
define i64 @src_andn(i64 %a, i64 %b) {
  %not_b = xor i64 %b, -1
  %r = and i64 %a, %not_b
  ret i64 %r
}

define i64 @tgt_andn(i64 %a, i64 %b) {
  %r = call i64 asm "andn $0, $1, $2", "=r,r,r"(i64 %a, i64 %b)
  ret i64 %r
}
