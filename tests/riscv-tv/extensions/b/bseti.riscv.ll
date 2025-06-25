target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define i32 @tf_0_foo(i32 %0) {
entry:
  %or23 = or i32 %0, 16777216
  ret i32 %or23
}
