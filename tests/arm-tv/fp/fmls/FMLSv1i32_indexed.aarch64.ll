; Function Attrs: strictfp
define float @ED_view3d_clipping_calc(ptr %vec) #0 {
entry:
  %0 = load <2 x float>, ptr %vec, align 4
  %1 = fneg <2 x float> %0
  %2 = extractelement <2 x float> %1, i64 0
  %3 = call float @llvm.experimental.constrained.fmuladd.f32(float %2, float 0.000000e+00, float 0.000000e+00, metadata !"round.tonearest", metadata !"fpexcept.strict") #0
  ret float %3
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare float @llvm.experimental.constrained.fmuladd.f32(float, float, float, metadata, metadata) #1

attributes #0 = { strictfp }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }