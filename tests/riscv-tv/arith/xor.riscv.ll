target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define i1 @tf_0_foo(i64 %0, i64 %conv6) #0 {
entry:
  %cmp7 = icmp eq i64 %0, %conv6
  ret i1 %cmp7
}

attributes #0 = { "target-features"="+c" }
