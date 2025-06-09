target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define i1 @tf_0_foo(ptr %p) {
entry:
  %0 = load i16, ptr %p, align 2
  %tobool.not = icmp eq i16 %0, 0
  ret i1 %tobool.not
}
