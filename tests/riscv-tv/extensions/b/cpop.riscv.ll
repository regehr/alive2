target triple = "aarch64-unknown-linux-gnu"

define i64 @tf_0_foo(i64 %land.ext) {
entry:
  %0 = tail call i64 @llvm.ctpop.i64(i64 %land.ext)
  ret i64 %0
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i64 @llvm.ctpop.i64(i64) #0

attributes #0 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
