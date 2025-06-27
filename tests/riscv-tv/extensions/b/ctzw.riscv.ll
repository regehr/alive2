target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "aarch64-unknown-linux-gnu"

define i32 @tf_0_foo(i32 %land.ext) {
entry:
  %0 = tail call i32 @llvm.cttz.i32(i32 %land.ext, i1 false)
  ret i32 %0
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i32 @llvm.cttz.i32(i32, i1 immarg) #0

attributes #0 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }
