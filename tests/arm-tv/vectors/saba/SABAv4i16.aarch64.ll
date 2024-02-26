define <4 x i16> @f(<4 x i16> %0, <4 x i16> %1, <4 x i16> %2) {
  %4 = sub nsw <4 x i16> %1, %2
  %5 = call <4 x i16> @llvm.abs.v4i16(<4 x i16> %4, i1 true)
  %6 = add <4 x i16> %0, %5
  ret <4 x i16> %6
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare <4 x i16> @llvm.abs.v4i16(<4 x i16>, i1 immarg) #0
