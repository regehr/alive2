target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define void @tf_0_foo(i64 %0, i64 %1) {
entry:
  %cmp102 = icmp ult i64 %0, %1
  br i1 %cmp102, label %if.then104, label %if.else

if.then104:                                       ; preds = %entry
  store i64 0, ptr null, align 8
  ret void

if.else:                                          ; preds = %entry
  ret void
}
