; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i32 @llvm.vector.reduce.add.v8i32(<8 x i32>) #0

define i32 @f(<8 x i8> %0, <8 x i8> %1, <8 x i8> %2, <8 x i8> %3) {
  %5 = sext <8 x i8> %0 to <8 x i32>
  %6 = call i32 @llvm.vector.reduce.add.v8i32(<8 x i32> %5)
  %7 = sext <8 x i8> %2 to <8 x i32>
  %8 = call i32 @llvm.vector.reduce.add.v8i32(<8 x i32> %7)
  %9 = add i32 %6, %8
  ret i32 %9
}