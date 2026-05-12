; TEST-ARGS: -disable-undef-input -disable-poison-input
; SKIP-IDENTITY
target triple = "aarch64-unknown-linux-gnu"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32"

; CHECK: callbr (asm-goto) not supported
; callbr / asm goto: branches to a label based on condition inside asm

define i32 @src(i32 %a) {
entry:
  %cmp = icmp eq i32 %a, 0
  br i1 %cmp, label %is_zero, label %not_zero

is_zero:
  ret i32 0

not_zero:
  ret i32 %a
}

define i32 @tgt(i32 %a) {
entry:
  callbr void asm "cbz ${0:w}, ${1:l}", "r,!i"(i32 %a)
    to label %fallthrough [label %is_zero]

fallthrough:
  ret i32 %a

is_zero:
  ret i32 0
}
