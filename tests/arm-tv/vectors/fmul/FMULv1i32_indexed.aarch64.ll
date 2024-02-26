; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare float @llvm.experimental.constrained.fmul.f32(float, float, metadata, metadata) #0

; Function Attrs: strictfp
define float @shade_input_set_normals(ptr %op1) #1 {
entry:
  %0 = load <2 x float>, ptr %op1, align 4
  %1 = extractelement <2 x float> %0, i64 1
  %mul4.i = tail call float @llvm.experimental.constrained.fmul.f32(float %1, float 0.000000e+00, metadata !"round.tonearest", metadata !"fpexcept.strict") #1
  ret float %mul4.i
}

attributes #0 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }
attributes #1 = { strictfp }