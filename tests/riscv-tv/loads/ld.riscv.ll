target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define i64 @tf_0_foo(ptr %0) #0 {
entry:
  %1 = load i64, ptr %0, align 8
  ret i64 %1
}

attributes #0 = { "target-features"="+c" }
