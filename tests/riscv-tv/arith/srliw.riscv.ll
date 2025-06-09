target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define i1 @tf_0_foo(i32 %conv1) {
entry:
  %tobool.not = icmp ult i32 %conv1, 32768
  ret i1 %tobool.not
}
