target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define i64 @tf_0_foo(i64 %conv2) {
entry:
  %shr = lshr i64 1, %conv2
  ret i64 %shr
}
