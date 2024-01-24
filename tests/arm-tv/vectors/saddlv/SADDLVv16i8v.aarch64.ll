define signext i16 @f(<16 x i8> %0, i16 %1) {
  %3 = sext <16 x i8> %0 to <16 x i16>
  %4 = call i16 @llvm.vector.reduce.add.v16i16(<16 x i16> %3)
  %5 = add i16 %4, %1
  ret i16 %5
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i16 @llvm.vector.reduce.add.v16i16(<16 x i16>) #0