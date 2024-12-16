; ModuleID = '<stdin>'
source_filename = "<stdin>"

; Function Attrs: nounwind
define float @f(float %0, float %1) {
  %3 = call nnan float @llvm.minnum.f32(float %0, float %1) #2
  ret float %3
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare float @llvm.minnum.f32(float, float) #1




