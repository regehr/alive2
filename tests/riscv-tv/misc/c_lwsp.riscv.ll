target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define void @test(i64 %var_0, i32 %var_1, i32 %var_2, i64 %var_3, i1 %var_4, i8 %var_5, i1 %var_6, i8 %var_7, i32 %zero) #0 {
entry:
  notail call void (...) @llvm.fake.use(i32 %zero)
  ret void
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare void @llvm.fake.use(...) #1

attributes #0 = { "target-features"="+c" }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }
