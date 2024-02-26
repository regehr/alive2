; Function Attrs: strictfp
define double @_ZN3pov5NoiseEPdPNS_14Pattern_StructE() #0 {
entry:
  %0 = tail call double @llvm.experimental.constrained.floor.f64(double 0.000000e+00, metadata !"fpexcept.strict") #0
  ret double 0.000000e+00
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare double @llvm.experimental.constrained.floor.f64(double, metadata) #1

attributes #0 = { strictfp }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }