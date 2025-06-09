target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define i8 @tf_0_foo(i8 %conv13) {
entry:
  %shl = shl i8 1, %conv13
  ret i8 %shl
}
