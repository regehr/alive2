target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define i32 @tf_0_foo(i32 %0, i32 %conv18) {
entry:
  %q = udiv i32 %0, %conv18
  ret i32 %q
}
