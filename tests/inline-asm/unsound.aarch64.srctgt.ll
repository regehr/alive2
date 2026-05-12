; TEST-ARGS: -disable-undef-input -disable-poison-input
; SKIP-IDENTITY
target triple = "aarch64-unknown-linux-gnu"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32"
; CHECK: Transformation doesn't verify!

; ---- unsound ----
define i32 @src(i32 %a, i32 %b) {
  %r = add i32 %a, %b
  ret i32 %r
}

define i32 @tgt(i32 %a, i32 %b) {
  %r = call i32 asm "sub ${0:w}, ${1:w}, ${2:w}", "=r,r,r"(i32 %a, i32 %b)
  ret i32 %r
}

; ---- unsound_and_vs_or ----
define i32 @src_unsound_and_vs_or(i32 %a, i32 %b) {
  %r = and i32 %a, %b
  ret i32 %r
}

define i32 @tgt_unsound_and_vs_or(i32 %a, i32 %b) {
  %r = call i32 asm "orr ${0:w}, ${1:w}, ${2:w}", "=r,r,r"(i32 %a, i32 %b)
  ret i32 %r
}

; ---- unsound_lsl_vs_lsr ----
define i32 @src_unsound_lsl_vs_lsr(i32 %a) {
  %r = shl i32 %a, 2
  ret i32 %r
}

define i32 @tgt_unsound_lsl_vs_lsr(i32 %a) {
  %r = call i32 asm "lsr ${0:w}, ${1:w}, #2", "=r,r"(i32 %a)
  ret i32 %r
}
