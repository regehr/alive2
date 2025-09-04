define i1 @f(half %0) {
  %2 = call half @llvm.fabs.f16(half %0)
  %3 = fcmp one half %2, 0xH7C00
  %4 = fcmp one half %0, 0xH0000
  %5 = and i1 %4, %3
  ret i1 %5
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare half @llvm.fabs.f16(half) #0
