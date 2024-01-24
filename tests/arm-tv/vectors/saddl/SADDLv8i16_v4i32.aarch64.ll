; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i32 @llvm.vector.reduce.add.v16i32(<16 x i32>) #0

define i32 @f(ptr nocapture readonly %0) {
  %2 = load <16 x i8>, ptr %0, align 16
  %3 = sext <16 x i8> %2 to <16 x i32>
  %4 = call i32 @llvm.vector.reduce.add.v16i32(<16 x i32> %3)
  ret i32 %4
}