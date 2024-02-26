; Function Attrs: strictfp
define double @_ZN11xalanc_1_1013DoubleSupport6divideEdd() #0 {
entry:
  %div = tail call double @llvm.experimental.constrained.fdiv.f64(double 0.000000e+00, double 0.000000e+00, metadata !"round.tonearest", metadata !"fpexcept.strict") #0
  ret double %div
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare double @llvm.experimental.constrained.fdiv.f64(double, double, metadata, metadata) #1

attributes #0 = { strictfp }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }