target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define i1 @tf_0_foo(i64 %0) {
entry:
  %1 = icmp ugt i64 %0, -8935141660703064066
  ret i1 %1
}
