define i32 @f(<16 x i8> %0, <16 x i8> %1) {
  %3 = zext <16 x i8> %0 to <16 x i32>
  %4 = zext <16 x i8> %1 to <16 x i32>
  %5 = mul nuw nsw <16 x i32> %4, %3
  %6 = call i32 @llvm.vector.reduce.add.v16i32(<16 x i32> %5)
  ret i32 %6
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i32 @llvm.vector.reduce.add.v16i32(<16 x i32>) #0