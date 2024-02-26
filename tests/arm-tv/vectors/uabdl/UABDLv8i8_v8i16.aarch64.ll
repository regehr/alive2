define <8 x i16> @f(<8 x i8> %0, <8 x i8> %1) {
  %3 = zext <8 x i8> %0 to <8 x i16>
  %4 = zext <8 x i8> %1 to <8 x i16>
  %5 = sub <8 x i16> %3, %4
  %6 = call <8 x i16> @llvm.abs.v8i16(<8 x i16> %5, i1 true)
  ret <8 x i16> %6
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare <8 x i16> @llvm.abs.v8i16(<8 x i16>, i1 immarg) #1