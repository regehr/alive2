target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define void @tf_0_foo(i32 %0, i32 %1, ptr %p) {
entry:
  %cmp118 = icmp slt i32 %1, %0
  br i1 %cmp118, label %if.then, label %if.end272

if.then:                                          ; preds = %entry
  store i16 0, ptr %p, align 2
  br label %if.end272

if.end272:                                        ; preds = %if.then, %entry
  ret void
}
