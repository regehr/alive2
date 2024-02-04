; Function Attrs: nounwind
define <4 x i32> @f(<4 x i32> %0, <4 x i32> %1) {
  %3 = sub nsw <4 x i32> %0, %1
  %4 = call <4 x i32> @llvm.abs.v4i32(<4 x i32> %3, i1 false)
  ret <4 x i32> %4
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare <4 x i32> @llvm.abs.v4i32(<4 x i32>, i1 immarg) #1