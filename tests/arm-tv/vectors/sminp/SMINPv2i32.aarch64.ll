define i32 @f(<2 x i32> %0) {
  %2 = call i32 @llvm.vector.reduce.smin.v2i32(<2 x i32> %0)
  ret i32 %2
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i32 @llvm.vector.reduce.smin.v2i32(<2 x i32>) #0