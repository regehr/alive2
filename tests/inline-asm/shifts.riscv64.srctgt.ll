; TEST-ARGS: -disable-undef-input -disable-poison-input
; SKIP-IDENTITY
target triple = "riscv64-unknown-linux-gnu"
target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"

; ---- sll_reg ----
define i64 @src(i64 %a, i64 %b) {
  %mask = and i64 %b, 63
  %r = shl i64 %a, %mask
  ret i64 %r
}

define i64 @tgt(i64 %a, i64 %b) {
  %r = call i64 asm "sll $0, $1, $2", "=r,r,r"(i64 %a, i64 %b)
  ret i64 %r
}

; ---- srl_reg ----
define i64 @src_srl_reg(i64 %a, i64 %b) {
  %mask = and i64 %b, 63
  %r = lshr i64 %a, %mask
  ret i64 %r
}

define i64 @tgt_srl_reg(i64 %a, i64 %b) {
  %r = call i64 asm "srl $0, $1, $2", "=r,r,r"(i64 %a, i64 %b)
  ret i64 %r
}

; ---- srai ----
define i64 @src_srai(i64 %a) {
  %r = ashr i64 %a, 10
  ret i64 %r
}

define i64 @tgt_srai(i64 %a) {
  %r = call i64 asm "srai $0, $1, 10", "=r,r"(i64 %a)
  ret i64 %r
}
