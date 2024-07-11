; Function Attrs: nounwind
define dso_local i32 @f(<2 x i32> %0) local_unnamed_addr {
  %2 = call i32 @llvm.vector.reduce.umax.v2i32(<2 x i32> %0)
  ret i32 %2
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i32 @llvm.vector.reduce.umax.v2i32(<2 x i32>) #1