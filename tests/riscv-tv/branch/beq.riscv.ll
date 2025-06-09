target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define void @tf_0_foo(i1 %0) {
entry:
  br i1 %0, label %cond.false34, label %cond.true24

cond.true24:                                      ; preds = %entry
  ret void

cond.false34:                                     ; preds = %entry
  store i64 0, ptr null, align 8
  ret void
}
