; ModuleID = '<stdin>'
source_filename = "<stdin>"

define double @f(double %0) {
  %2 = call double @llvm.maximum.f64(double %0, double 8.000000e+00)
  %3 = call double @llvm.maximum.f64(double %2, double 1.600000e+01)
  ret double %3
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare double @llvm.maximum.f64(double, double) #0


