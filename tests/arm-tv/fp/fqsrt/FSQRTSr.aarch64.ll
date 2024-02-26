; Function Attrs: strictfp
define float @dist_MinkovskyH() #0 {
entry:
  %0 = tail call float @llvm.experimental.constrained.sqrt.f32(float 0.000000e+00, metadata !"round.tonearest", metadata !"fpexcept.strict") #0
  ret float %0
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare float @llvm.experimental.constrained.sqrt.f32(float, metadata, metadata) #1

attributes #0 = { strictfp }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }