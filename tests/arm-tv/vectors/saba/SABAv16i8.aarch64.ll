define <16 x i8> @f(<16 x i8> %0, <16 x i8> %1, <16 x i8> %2) {
  %4 = sub nsw <16 x i8> %1, %2
  %5 = call <16 x i8> @llvm.abs.v16i8(<16 x i8> %4, i1 true)
  %6 = add <16 x i8> %0, %5
  ret <16 x i8> %6
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare <16 x i8> @llvm.abs.v16i8(<16 x i8>, i1 immarg) #0