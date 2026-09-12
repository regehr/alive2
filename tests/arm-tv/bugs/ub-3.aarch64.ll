; XFAIL: ERROR: Value mismatch
; LLVM currently overwrites the dividend with the quotient in x8, then emits
; "msub w8, w8, w1, w8". With initial w8 = 2 and divisor x1 = 2, this
; returns 63, while the source remainder can only be 0 or 1. Preserving
; unknown register values through optimization exposes this codegen bug.

target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "aarch64-linux-gnu"

define zeroext i6 @f(i32 %0, i64 %1) {
  %3 = freeze i8 poison
  %4 = sext i8 %3 to i64
  %5 = urem i64 %4, %1
  %6 = trunc i64 %5 to i6
  ret i6 %6
}
