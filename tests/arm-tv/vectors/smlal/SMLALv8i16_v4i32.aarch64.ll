define i32 @f(ptr %0, ptr %1) {
  %3 = load <8 x i16>, ptr %0, align 2
  %4 = sext <8 x i16> %3 to <8 x i32>
  %5 = load <8 x i16>, ptr %1, align 2
  %6 = sext <8 x i16> %5 to <8 x i32>
  %7 = mul nsw <8 x i32> %6, %4
  %8 = call i32 @llvm.vector.reduce.add.v8i32(<8 x i32> %7)
  ret i32 %8
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i32 @llvm.vector.reduce.add.v8i32(<8 x i32>) #0