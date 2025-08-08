target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define void @tf_0_foo() #0 {
entry:
  ret void
}

attributes #0 = { "frame-pointer"="all" "target-features"="+c" }
