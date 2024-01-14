; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i64 @llvm.vector.reduce.add.v64i64(<64 x i64>) #0

; Function Attrs: nounwind
define i64 @f(ptr %0) {
  %2 = load <64 x i64>, ptr %0, align 512
  %3 = call i64 @llvm.vector.reduce.add.v64i64(<64 x i64> %2)
  ret i64 %3
}