target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define i1 @tf_0_foo(i64 %0) {
entry:
  %cmp63 = icmp eq i64 %0, -6541085465
  ret i1 %cmp63
}
