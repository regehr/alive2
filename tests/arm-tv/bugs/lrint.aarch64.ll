define i32 @f(float %0) {
  %2 = tail call i32 @llvm.lrint.i32.f32(float %0)
  ret i32 %2
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i32 @llvm.lrint.i32.f32(float) #0
