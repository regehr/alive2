target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define i1 @tf_0_foo(i64 %0, i64 %1, i64 %div) #0 {
entry:
  %mul21 = mul i64 %0, %0
  %mul32 = mul i64 %div, %0
  %rem = mul i64 %mul21, %mul32
  %cmp = icmp ult i64 %1, %rem
  ret i1 %cmp
}

attributes #0 = { "target-features"="+c" }
