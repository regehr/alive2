; Function Attrs: strictfp
define i32 @_ZN3povL13Inside_HFieldEPdPNS_13Object_StructE() #0 {
entry:
  %0 = call double @llvm.experimental.constrained.ceil.f64(double 0.000000e+00, metadata !"fpexcept.strict") #0
  ret i32 0
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare double @llvm.experimental.constrained.ceil.f64(double, metadata) #1

attributes #0 = { strictfp }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }