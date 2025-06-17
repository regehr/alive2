target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define i1 @tf_0_foo(i64 %0, i64 %b) {
entry:
  %mul7 = mul i64 %0, %b
  %1 = and i64 %mul7, 4294967295
  %tobool68.not = icmp eq i64 %1, 0
  ret i1 %tobool68.not
}
