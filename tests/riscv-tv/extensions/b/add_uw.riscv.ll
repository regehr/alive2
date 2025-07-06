target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define i64 @tf_0_foo(i64 %0) {
entry:
  %sub71 = add i64 %0, 4294967295
  ret i64 %sub71
}
