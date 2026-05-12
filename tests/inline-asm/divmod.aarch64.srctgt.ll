; TEST-ARGS: -disable-undef-input -disable-poison-input
; SKIP-IDENTITY
target triple = "aarch64-unknown-linux-gnu"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32"

; Multi-output: sdiv + msub to get both quotient and remainder

define i32 @src(i32 %a, i32 %b) {
  %q = sdiv i32 %a, %b
  %mul = mul i32 %q, %b
  %rem = sub i32 %a, %mul
  %r = add i32 %q, %rem
  ret i32 %r
}

define i32 @tgt(i32 %a, i32 %b) {
  %result = call {i32, i32} asm "sdiv ${0:w}, ${2:w}, ${3:w}\0Amsub ${1:w}, ${0:w}, ${3:w}, ${2:w}", "=&r,=&r,r,r"(i32 %a, i32 %b)
  %q = extractvalue {i32, i32} %result, 0
  %rem = extractvalue {i32, i32} %result, 1
  %r = add i32 %q, %rem
  ret i32 %r
}
