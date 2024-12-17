; ModuleID = '<stdin>'
source_filename = "<stdin>"

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare double @llvm.minimum.f64(double, double) #0

define double @f(double %0, double %1) {
  %3 = call double @llvm.minimum.f64(double %0, double %1)
  ret double %3
}


