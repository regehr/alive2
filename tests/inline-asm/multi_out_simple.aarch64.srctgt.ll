; TEST-ARGS: -disable-undef-input -disable-poison-input
; SKIP-IDENTITY
target triple = "aarch64-unknown-linux-gnu"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32"

; Multi-output: return both sum and difference

define i32 @src(i32 %a, i32 %b) {
  %sum = add i32 %a, %b
  %diff = sub i32 %a, %b
  %r = xor i32 %sum, %diff
  ret i32 %r
}

define i32 @tgt(i32 %a, i32 %b) {
  %result = call {i32, i32} asm "add ${0:w}, ${2:w}, ${3:w}\0Asub ${1:w}, ${2:w}, ${3:w}", "=&r,=&r,r,r"(i32 %a, i32 %b)
  %sum = extractvalue {i32, i32} %result, 0
  %diff = extractvalue {i32, i32} %result, 1
  %r = xor i32 %sum, %diff
  ret i32 %r
}
