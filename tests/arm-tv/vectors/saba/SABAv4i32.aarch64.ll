define <4 x i32> @f(<4 x i32> %0, <4 x i32> %1, <4 x i32> %2) {
  %4 = sub nsw <4 x i32> %1, %2
  %5 = call <4 x i32> @llvm.abs.v4i32(<4 x i32> %4, i1 true)
  %6 = add <4 x i32> %0, %5
  ret <4 x i32> %6
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare <4 x i32> @llvm.abs.v4i32(<4 x i32>, i1 immarg) #0