; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare float @llvm.experimental.constrained.fdiv.f32(float, float, metadata, metadata) #0

; Function Attrs: strictfp
define float @rule_follow_leader() #1 {
entry:
  %div = tail call float @llvm.experimental.constrained.fdiv.f32(float 0.000000e+00, float 0.000000e+00, metadata !"round.tonearest", metadata !"fpexcept.strict") #1
  ret float %div
}

attributes #0 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }
attributes #1 = { strictfp }