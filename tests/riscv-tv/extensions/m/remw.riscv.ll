target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define i32 @tf_0_foo(i32 %0, i32 %conv33) {
entry:
  %rem = srem i32 %0, %conv33
  ret i32 %rem
}
