; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare i1 @llvm.experimental.constrained.fcmps.f64(double, double, metadata, metadata) #0

; Function Attrs: strictfp
define double @_ZN3pov13Triangle_WaveEd() #1 {
entry:
  %cmp = tail call i1 @llvm.experimental.constrained.fcmps.f64(double 0.000000e+00, double 0.000000e+00, metadata !"oge", metadata !"fpexcept.strict") #1
  ret double 0.000000e+00
}

attributes #0 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }
attributes #1 = { strictfp }