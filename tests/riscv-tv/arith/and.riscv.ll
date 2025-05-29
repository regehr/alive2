target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define i64 @tf_0_foo(i64 %0) #0 {
entry:
  %and = and i64 %0, 63488
  ret i64 %and
}

attributes #0 = { "target-features"="+c" }
