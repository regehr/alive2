target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define i64 @tf_0_foo(i64 %0, i64 %conv7, i64 %1) #0 {
entry:
  %mul = mul i64 %0, %0
  %mul12 = mul i64 %1, %conv7
  %add13 = or i64 %0, %mul12
  %mul14 = mul i64 %mul, %add13
  ret i64 %mul14
}

attributes #0 = { "target-features"="+c" }
