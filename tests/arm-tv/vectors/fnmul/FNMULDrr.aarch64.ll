; Function Attrs: strictfp
define double @_ZN3pov10f_umbrellaEPdj() #0 {
entry:
  %mul6 = tail call double @llvm.experimental.constrained.fmul.f64(double 0.000000e+00, double 0.000000e+00, metadata !"round.tonearest", metadata !"fpexcept.strict") #0
  %neg = fneg double %mul6
  %0 = tail call double @llvm.experimental.constrained.fmuladd.f64(double 0.000000e+00, double 0.000000e+00, double %neg, metadata !"round.tonearest", metadata !"fpexcept.strict") #0
  %fneg = fneg double %0
  %mul = tail call double @llvm.experimental.constrained.fmul.f64(double 0.000000e+00, double %fneg, metadata !"round.tonearest", metadata !"fpexcept.strict") #0
  ret double 0.000000e+00
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare double @llvm.experimental.constrained.fmul.f64(double, double, metadata, metadata) #1

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare double @llvm.experimental.constrained.fmuladd.f64(double, double, double, metadata, metadata) #1

attributes #0 = { strictfp }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }