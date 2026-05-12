; TEST-ARGS: -disable-undef-input -disable-poison-input
; SKIP-IDENTITY
target triple = "aarch64-unknown-linux-gnu"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32"

declare i32 @llvm.fshr.i32(i32, i32, i32)

; ---- lsl_imm ----
define i32 @src(i32 %a) {
  %r = shl i32 %a, 3
  ret i32 %r
}

define i32 @tgt(i32 %a) {
  %r = call i32 asm "lsl ${0:w}, ${1:w}, #3", "=r,r"(i32 %a)
  ret i32 %r
}

; ---- lsr_imm ----
define i32 @src_lsr_imm(i32 %a) {
  %r = lshr i32 %a, 5
  ret i32 %r
}

define i32 @tgt_lsr_imm(i32 %a) {
  %r = call i32 asm "lsr ${0:w}, ${1:w}, #5", "=r,r"(i32 %a)
  ret i32 %r
}

; ---- asr_imm ----
define i32 @src_asr_imm(i32 %a) {
  %r = ashr i32 %a, 4
  ret i32 %r
}

define i32 @tgt_asr_imm(i32 %a) {
  %r = call i32 asm "asr ${0:w}, ${1:w}, #4", "=r,r"(i32 %a)
  ret i32 %r
}

; ---- ror ----
define i32 @src_ror(i32 %a) {
  %r = call i32 @llvm.fshr.i32(i32 %a, i32 %a, i32 7)
  ret i32 %r
}

define i32 @tgt_ror(i32 %a) {
  %r = call i32 asm "ror ${0:w}, ${1:w}, #7", "=r,r"(i32 %a)
  ret i32 %r
}

; ---- ror_variable ----
define i32 @src_ror_variable(i32 %a, i32 %n) {
  %r = call i32 @llvm.fshr.i32(i32 %a, i32 %a, i32 %n)
  ret i32 %r
}

define i32 @tgt_ror_variable(i32 %a, i32 %n) {
  %r = call i32 asm "ror ${0:w}, ${1:w}, ${2:w}", "=r,r,r"(i32 %a, i32 %n)
  ret i32 %r
}

; ---- extr ----
define i32 @src_extr(i32 %hi, i32 %lo) {
  %r = call i32 @llvm.fshr.i32(i32 %hi, i32 %lo, i32 16)
  ret i32 %r
}

define i32 @tgt_extr(i32 %hi, i32 %lo) {
  %r = call i32 asm "extr ${0:w}, ${1:w}, ${2:w}, #16", "=r,r,r"(i32 %hi, i32 %lo)
  ret i32 %r
}
