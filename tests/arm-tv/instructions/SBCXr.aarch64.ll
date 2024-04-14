define <2 x i64> @f(<2 x i64> %0, <2 x i64> %1) {
  %3 = zext <2 x i64> %0 to <2 x i128>
  %4 = zext <2 x i64> %1 to <2 x i128>
  %5 = sub <2 x i128> %3, %4
  %6 = call <2 x i128> @llvm.abs.v2i128(<2 x i128> %5, i1 true)
  %7 = trunc <2 x i128> %6 to <2 x i64>
  ret <2 x i64> %7
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare <2 x i128> @llvm.abs.v2i128(<2 x i128>, i1 immarg) #1