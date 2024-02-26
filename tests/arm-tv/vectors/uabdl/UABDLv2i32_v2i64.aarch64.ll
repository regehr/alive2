define <2 x i64> @f(<2 x i32> %0, <2 x i32> %1) {
  %3 = zext <2 x i32> %0 to <2 x i64>
  %4 = zext <2 x i32> %1 to <2 x i64>
  %5 = sub <2 x i64> %3, %4
  %6 = call <2 x i64> @llvm.abs.v2i64(<2 x i64> %5, i1 true)
  ret <2 x i64> %6
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare <2 x i64> @llvm.abs.v2i64(<2 x i64>, i1 immarg) #1