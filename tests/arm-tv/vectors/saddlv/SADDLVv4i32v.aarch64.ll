define i64 @f(<4 x i32> %0, i64 %1) {
  %3 = sext <4 x i32> %0 to <4 x i64>
  %4 = call i64 @llvm.vector.reduce.add.v4i64(<4 x i64> %3)
  %5 = add i64 %4, %1
  ret i64 %5
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i64 @llvm.vector.reduce.add.v4i64(<4 x i64>) #0