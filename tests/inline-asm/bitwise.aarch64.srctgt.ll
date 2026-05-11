; TEST-ARGS: -disable-undef-input -disable-poison-input
; SKIP-IDENTITY
target triple = "aarch64-unknown-linux-gnu"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32"

; ---- and_reg ----
define i32 @src(i32 %a, i32 %b) {
  %r = and i32 %a, %b
  ret i32 %r
}

define i32 @tgt(i32 %a, i32 %b) {
  %r = call i32 asm "and ${0:w}, ${1:w}, ${2:w}", "=r,r,r"(i32 %a, i32 %b)
  ret i32 %r
}

; ---- orr_reg ----
define i32 @src_orr_reg(i32 %a, i32 %b) {
  %r = or i32 %a, %b
  ret i32 %r
}

define i32 @tgt_orr_reg(i32 %a, i32 %b) {
  %r = call i32 asm "orr ${0:w}, ${1:w}, ${2:w}", "=r,r,r"(i32 %a, i32 %b)
  ret i32 %r
}

; ---- eor_reg ----
define i32 @src_eor_reg(i32 %a, i32 %b) {
  %r = xor i32 %a, %b
  ret i32 %r
}

define i32 @tgt_eor_reg(i32 %a, i32 %b) {
  %r = call i32 asm "eor ${0:w}, ${1:w}, ${2:w}", "=r,r,r"(i32 %a, i32 %b)
  ret i32 %r
}

; ---- mvn ----
define i32 @src_mvn(i32 %a) {
  %r = xor i32 %a, -1
  ret i32 %r
}

define i32 @tgt_mvn(i32 %a) {
  %r = call i32 asm "mvn ${0:w}, ${1:w}", "=r,r"(i32 %a)
  ret i32 %r
}

; ---- bic ----
define i32 @src_bic(i32 %a, i32 %b) {
  %not_b = xor i32 %b, -1
  %r = and i32 %a, %not_b
  ret i32 %r
}

define i32 @tgt_bic(i32 %a, i32 %b) {
  %r = call i32 asm "bic ${0:w}, ${1:w}, ${2:w}", "=r,r,r"(i32 %a, i32 %b)
  ret i32 %r
}

; ---- orn ----
define i32 @src_orn(i32 %a, i32 %b) {
  %not_b = xor i32 %b, -1
  %r = or i32 %a, %not_b
  ret i32 %r
}

define i32 @tgt_orn(i32 %a, i32 %b) {
  %r = call i32 asm "orn ${0:w}, ${1:w}, ${2:w}", "=r,r,r"(i32 %a, i32 %b)
  ret i32 %r
}

; ---- eon ----
define i32 @src_eon(i32 %a, i32 %b) {
  %not_b = xor i32 %b, -1
  %r = xor i32 %a, %not_b
  ret i32 %r
}

define i32 @tgt_eon(i32 %a, i32 %b) {
  %r = call i32 asm "eon ${0:w}, ${1:w}, ${2:w}", "=r,r,r"(i32 %a, i32 %b)
  ret i32 %r
}

; ---- xor_clobber ----
define i32 @src_xor_clobber(i32 %a, i32 %b) {
  %r = xor i32 %a, %b
  ret i32 %r
}

define i32 @tgt_xor_clobber(i32 %a, i32 %b) {
  %r = call i32 asm "eor ${0:w}, ${1:w}, ${2:w}", "=r,r,r,~{x9}"(i32 %a, i32 %b)
  ret i32 %r
}
