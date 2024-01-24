define i32 @f(<8 x i8> %0, i32 %1) {
  %3 = sext <8 x i8> %0 to <8 x i32>
  %4 = call i32 @llvm.vector.reduce.add.v8i32(<8 x i32> %3)
  %5 = add i32 %4, %1
  ret i32 %5
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i32 @llvm.vector.reduce.add.v8i32(<8 x i32>) #0