; Function Attrs: strictfp
define ptr @_ZN3pov13Create_FinishEv() #0 {
entry:
  %conv = tail call float @llvm.experimental.constrained.fptrunc.f32.f64(double 0.000000e+00, metadata !"round.tonearest", metadata !"fpexcept.strict") #0
  ret ptr null
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare float @llvm.experimental.constrained.fptrunc.f32.f64(double, metadata, metadata) #1

attributes #0 = { strictfp }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }