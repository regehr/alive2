define <2 x i32> @f(<2 x double> %0) {
   %2 = fptoui <2 x double> %0 to <2 x i64>
   %3 = call <2 x i64> @llvm.umin.v2i64(<2 x i64> %2, <2 x i64> splat (i64 4294967295))
   %4 = trunc <2 x i64> %3 to <2 x i32>
   ret <2 x i32> %4
}
; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)

declare <2 x i64> @llvm.umin.v2i64(<2 x i64>, <2 x i64>) #0
attributes #0 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
