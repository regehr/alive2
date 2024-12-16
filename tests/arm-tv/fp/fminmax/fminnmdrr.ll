; ModuleID = '<stdin>'
source_filename = "<stdin>"

; Function Attrs: strictfp
define double @f(double %0, double %1) {
  %3 = call double @llvm.experimental.constrained.minnum.f64(double %0, double %1, metadata !"fpexcept.strict") #0
  ret double %3
}

; Function Attrs: nocallback nofree nosync nounwind strictfp willreturn memory(inaccessiblemem: readwrite)
declare double @llvm.experimental.constrained.minnum.f64(double, double, metadata) #1



