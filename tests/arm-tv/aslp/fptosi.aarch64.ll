
define i64 @f(float %0) {
   %2 = fptosi float %0 to i128
   %3 = call i128 @llvm.smin.i128(i128 %2, i128 9223372036854775807)
   %4 = call i128 @llvm.smax.i128(i128 %3, i128 -9223372036854775808)
   %5 = trunc i128 %4 to i64
   ret i64 %5
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i128 @llvm.smin.i128(i128, i128) #0

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i128 @llvm.smax.i128(i128, i128) #0

attributes #0 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
