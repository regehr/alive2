; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i32 @llvm.fshl.i32(i32, i32, i32) #0

; Function Attrs: nounwind
define signext i32 @f(i32 signext %0, i32 signext %1, i32 signext %2) {
  %4 = and i32 %2, 31
  %5 = tail call i32 @llvm.fshl.i32(i32 %0, i32 %0, i32 %4)
  %6 = tail call i32 @llvm.fshl.i32(i32 %1, i32 %1, i32 %4)
  %7 = add i32 %5, %6
  ret i32 %7
}
