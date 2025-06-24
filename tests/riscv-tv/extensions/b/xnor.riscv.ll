target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define i16 @tf_0_foo(i16 %0, i16 %1) {
entry:
  %2 = xor i16 %1, %0
  %3 = xor i16 %2, -1
  ret i16 %3
}
