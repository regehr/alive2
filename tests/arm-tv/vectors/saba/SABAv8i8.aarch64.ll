define <8 x i8> @f(<8 x i8> %0, <8 x i8> %1, <8 x i8> %2) {
  %4 = sub nsw <8 x i8> %1, %2
  %5 = call <8 x i8> @llvm.abs.v8i8(<8 x i8> %4, i1 true)
  %6 = add <8 x i8> %0, %5
  ret <8 x i8> %6
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare <8 x i8> @llvm.abs.v8i8(<8 x i8>, i1 immarg) #0