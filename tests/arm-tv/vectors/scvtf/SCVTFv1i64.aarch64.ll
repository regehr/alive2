; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare i1 @llvm.experimental.constrained.fcmp.f64(double, double, metadata, metadata) #0

; Function Attrs: strictfp
define double @_ZN11xalanc_1_1013DoubleSupport7modulusEdd() #1 {
entry:
  %conv10 = tail call i64 @llvm.experimental.constrained.fptosi.i64.f64(double 0.000000e+00, metadata !"fpexcept.strict") #1
  %conv11 = tail call double @llvm.experimental.constrained.sitofp.f64.i64(i64 %conv10, metadata !"round.tonearest", metadata !"fpexcept.strict") #1
  %cmp12 = tail call i1 @llvm.experimental.constrained.fcmp.f64(double %conv11, double 0.000000e+00, metadata !"oeq", metadata !"fpexcept.strict") #1
  ret double 0.000000e+00
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare i64 @llvm.experimental.constrained.fptosi.i64.f64(double, metadata) #0

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare double @llvm.experimental.constrained.sitofp.f64.i64(i64, metadata, metadata) #0

attributes #0 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }
attributes #1 = { strictfp }