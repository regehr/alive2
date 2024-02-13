; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare double @llvm.experimental.constrained.fpext.f64.f32(float, metadata) #0

; Function Attrs: strictfp
define i32 @_ZN3povL9compboxesEPKvS1_() #1 {
entry:
  %conv = tail call double @llvm.experimental.constrained.fpext.f64.f32(float 0.000000e+00, metadata !"fpexcept.strict") #1
  ret i32 0
}

attributes #0 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }
attributes #1 = { strictfp }