define <2 x i32> @f(<2 x i32> %0, <2 x i32> %1, <2 x i32> %2) {
  %4 = sub nsw <2 x i32> %1, %2
  %5 = call <2 x i32> @llvm.abs.v2i32(<2 x i32> %4, i1 true)
  %6 = add <2 x i32> %0, %5
  ret <2 x i32> %6
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare <2 x i32> @llvm.abs.v2i32(<2 x i32>, i1 immarg) #0