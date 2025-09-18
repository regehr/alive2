; ModuleID = '<stdin>'
source_filename = "<stdin>"

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare float @llvm.exp.f32(float) #0

define float @f(float nofpclass(inf nzero) %0) {
  %2 = call float @llvm.exp.f32(float %0)
  ret float %2
}
