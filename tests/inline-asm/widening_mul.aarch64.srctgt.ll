; TEST-ARGS: -disable-undef-input -disable-poison-input
; SKIP-IDENTITY
target triple = "aarch64-unknown-linux-gnu"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32"

; ---- smull ----
define i64 @src(i32 %a, i32 %b) {
  %ea = sext i32 %a to i64
  %eb = sext i32 %b to i64
  %r = mul i64 %ea, %eb
  ret i64 %r
}

define i64 @tgt(i32 %a, i32 %b) {
  %r = call i64 asm "smull $0, ${1:w}, ${2:w}", "=r,r,r"(i32 %a, i32 %b)
  ret i64 %r
}

; ---- umull ----
define i64 @src_umull(i32 %a, i32 %b) {
  %ea = zext i32 %a to i64
  %eb = zext i32 %b to i64
  %r = mul i64 %ea, %eb
  ret i64 %r
}

define i64 @tgt_umull(i32 %a, i32 %b) {
  %r = call i64 asm "umull $0, ${1:w}, ${2:w}", "=r,r,r"(i32 %a, i32 %b)
  ret i64 %r
}

; ---- mulhi_u32 ----
define i32 @src_mulhi_u32(i32 %a, i32 %b) {
  %ea = zext i32 %a to i64
  %eb = zext i32 %b to i64
  %prod = mul i64 %ea, %eb
  %hi = lshr i64 %prod, 32
  %r = trunc i64 %hi to i32
  ret i32 %r
}

define i32 @tgt_mulhi_u32(i32 %a, i32 %b) {
  %r = call i32 asm "umull x0, ${1:w}, ${2:w}\0Alsr x0, x0, #32\0Amov ${0:w}, w0", "=r,r,r,~{x0}"(i32 %a, i32 %b)
  ret i32 %r
}
