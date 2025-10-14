target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

@var_21 = external global i64, align 8

define void @test(i16 %var_1, i64 %var_2, i16 %var_3, i64 %var_4, i16 %var_5, i64 %var_8, i64 %var_10, i16 %var_11, i64 %var_15, i64 %foo, i64 %lor.ext) #0 {
entry:
  store i64 %lor.ext, ptr @var_21, align 8
  ret void
}

attributes #0 = { "target-features"="+c" }
