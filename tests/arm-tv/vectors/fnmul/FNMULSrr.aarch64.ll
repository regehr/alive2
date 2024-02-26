; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare float @llvm.experimental.constrained.fmul.f32(float, float, metadata, metadata) #0

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare float @llvm.experimental.constrained.fmuladd.f32(float, float, float, metadata, metadata) #0

; Function Attrs: strictfp
define float @sub_qt_qtqt(float %0) #1 {
entry:
  %fneg = fneg float %0
  %mul4.i = tail call float @llvm.experimental.constrained.fmul.f32(float 0.000000e+00, float 0.000000e+00, metadata !"round.tonearest", metadata !"fpexcept.strict") #1
  %neg.i = fneg float %mul4.i
  %1 = tail call float @llvm.experimental.constrained.fmuladd.f32(float 0.000000e+00, float %fneg, float %neg.i, metadata !"round.tonearest", metadata !"fpexcept.strict") #1
  ret float %1
}

attributes #0 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }
attributes #1 = { strictfp }