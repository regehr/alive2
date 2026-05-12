; TEST-ARGS: -disable-undef-input -disable-poison-input
; SKIP-IDENTITY
; XFAIL: Unsupported instruction
target triple = "aarch64-unknown-linux-gnu"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32"

; Count leading sign bits: cls = clz(x ^ (x >> 31)) - 1 for i32

define i32 @src(i32 %a) {
  %shift = ashr i32 %a, 31
  %x = xor i32 %a, %shift
  %clz = call i32 @llvm.ctlz.i32(i32 %x, i1 false)
  %r = sub i32 %clz, 1
  ret i32 %r
}

declare i32 @llvm.ctlz.i32(i32, i1)

define i32 @tgt(i32 %a) {
  %r = call i32 asm "cls ${0:w}, ${1:w}", "=r,r"(i32 %a)
  ret i32 %r
}
