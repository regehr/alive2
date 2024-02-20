define <8 x i16> @f(<8 x i16> %0, <8 x i16> %1, <8 x i16> %2) {
  %4 = sub nsw <8 x i16> %1, %2
  %5 = call <8 x i16> @llvm.abs.v8i16(<8 x i16> %4, i1 true)
  %6 = add <8 x i16> %0, %5
  ret <8 x i16> %6
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare <8 x i16> @llvm.abs.v8i16(<8 x i16>, i1 immarg) #0