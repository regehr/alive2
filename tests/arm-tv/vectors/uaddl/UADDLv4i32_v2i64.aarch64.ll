define i64 @f(<8 x i8> %0) {
  %2 = zext <8 x i8> %0 to <8 x i64>
  %3 = call i64 @llvm.vector.reduce.add.v8i64(<8 x i64> %2)
  ret i64 %3
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i64 @llvm.vector.reduce.add.v8i64(<8 x i64>) #0