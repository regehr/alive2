; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i64 @llvm.vector.reduce.add.v8i64(<8 x i64>) #0

define i64 @f(ptr %0) {
  %2 = load <8 x i32>, ptr %0, align 32
  %3 = sext <8 x i32> %2 to <8 x i64>
  %4 = call i64 @llvm.vector.reduce.add.v8i64(<8 x i64> %3)
  ret i64 %4
}