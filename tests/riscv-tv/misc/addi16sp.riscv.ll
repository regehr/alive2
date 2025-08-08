target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64"

define void @test(i64 %var_0, i32 %var_1, i32 %var_2, i64 %var_3, i8 %var_5, i8 %var_7, i64 %var_10, i32 %zero, ptr %arr_0, ptr %arr_1, ptr %arr_2, ptr %arr_5, ptr %var_15, i64 %conv145, i64 %0, i8 %storedv1, i8 %storedv) #0 {
entry:
  store i32 1, ptr %var_15, align 4
  notail call void (...) @llvm.fake.use(i64 %conv145)
  notail call void (...) @llvm.fake.use(i64 %0)
  notail call void (...) @llvm.fake.use(ptr %arr_5)
  notail call void (...) @llvm.fake.use(ptr %arr_2)
  notail call void (...) @llvm.fake.use(ptr %arr_1)
  notail call void (...) @llvm.fake.use(ptr %arr_0)
  notail call void (...) @llvm.fake.use(i32 %zero)
  notail call void (...) @llvm.fake.use(i64 %var_10)
  notail call void (...) @llvm.fake.use(i8 %var_7)
  notail call void (...) @llvm.fake.use(i8 %storedv1)
  notail call void (...) @llvm.fake.use(i8 %var_5)
  notail call void (...) @llvm.fake.use(i8 %storedv)
  notail call void (...) @llvm.fake.use(i64 %var_3)
  notail call void (...) @llvm.fake.use(i32 %var_2)
  notail call void (...) @llvm.fake.use(i32 %var_1)
  notail call void (...) @llvm.fake.use(i64 %var_0)
  ret void
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare void @llvm.fake.use(...) #1

; uselistorder directives
uselistorder ptr @llvm.fake.use, { 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 }

attributes #0 = { "target-features"="+c" }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }
