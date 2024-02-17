define <16 x i8> @f(<16 x i8> %0, <16 x i8> %1) {
  %3 = zext <16 x i8> %0 to <16 x i16>
  %4 = zext <16 x i8> %1 to <16 x i16>
  %5 = sub <16 x i16> %3, %4
  %6 = call <16 x i16> @llvm.abs.v16i16(<16 x i16> %5, i1 true)
  %7 = trunc <16 x i16> %6 to <16 x i8>
  ret <16 x i8> %7
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare <16 x i16> @llvm.abs.v16i16(<16 x i16>, i1 immarg) #1