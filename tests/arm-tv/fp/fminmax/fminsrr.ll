; ModuleID = '<stdin>'
source_filename = "<stdin>"

define float @f(float %0, float %1, float %2) {
  %4 = call float @llvm.fabs.f32(float %0)
  %5 = call float @llvm.minimum.f32(float %4, float %1)
  %6 = call float @llvm.minimum.f32(float %5, float %2)
  ret float %6
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare float @llvm.fabs.f32(float) #0

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare float @llvm.minimum.f32(float, float) #0


