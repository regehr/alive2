; ModuleID = '<stdin>'
source_filename = "<stdin>"

define double @f(i16 %0) {
  %2 = uitofp i16 %0 to float
  %3 = tail call fast float @llvm.pow.f32(float 7.000000e+00, float %2)
  %4 = fpext float %3 to double
  ret double %4
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare float @llvm.pow.f32(float, float) #0


