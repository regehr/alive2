define <4 x i16> @f(<4 x i16> %0, <4 x i16> %1) {
  %3 = sext <4 x i16> %0 to <4 x i32>
  %4 = sext <4 x i16> %1 to <4 x i32>
  %5 = sub <4 x i32> %3, %4
  %6 = call <4 x i32> @llvm.abs.v4i32(<4 x i32> %5, i1 true)
  %7 = trunc <4 x i32> %6 to <4 x i16>
  ret <4 x i16> %7
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare <4 x i32> @llvm.abs.v4i32(<4 x i32>, i1 immarg) #1