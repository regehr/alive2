target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define i64 @tf_0_foo(i64 %0) {
entry:
  %mul33 = mul i64 %0, 3
  ret i64 %mul33
}
