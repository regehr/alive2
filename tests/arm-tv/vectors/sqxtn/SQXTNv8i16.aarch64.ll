define <8 x i16> @f(<4 x i16> %0, <4 x i32> %1) {
  %3 = call <4 x i32> @llvm.smax.v4i32(<4 x i32> %1, <4 x i32> <i32 -32768, i32 -32768, i32 -32768, i32 -32768>)
  %4 = call <4 x i32> @llvm.smin.v4i32(<4 x i32> %3, <4 x i32> <i32 32767, i32 32767, i32 32767, i32 32767>)
  %5 = trunc <4 x i32> %4 to <4 x i16>
  %6 = shufflevector <4 x i16> %0, <4 x i16> %5, <8 x i32> <i32 0, i32 1, i32 2, i32 3, i32 4, i32 5, i32 6, i32 7>
  ret <8 x i16> %6
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare <4 x i32> @llvm.smin.v4i32(<4 x i32>, <4 x i32>) #0

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare <4 x i32> @llvm.smax.v4i32(<4 x i32>, <4 x i32>) #0