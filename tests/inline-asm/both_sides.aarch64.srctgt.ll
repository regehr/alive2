; TEST-ARGS: -disable-undef-input -disable-poison-input
; SKIP-IDENTITY
target triple = "aarch64-unknown-linux-gnu"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32"

; Both src and tgt contain inline asm; both get lifted before comparison.

define i32 @src(i32 %a, i32 %b) {
  %r = call i32 asm "add ${0:w}, ${1:w}, ${2:w}", "=r,r,r"(i32 %a, i32 %b)
  ret i32 %r
}

define i32 @tgt(i32 %a, i32 %b) {
  %r = call i32 asm "add ${0:w}, ${2:w}, ${1:w}", "=r,r,r"(i32 %a, i32 %b)
  ret i32 %r
}
