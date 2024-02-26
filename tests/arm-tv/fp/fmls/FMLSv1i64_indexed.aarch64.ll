; Function Attrs: strictfp
define double @ED_view3d_clipping_calc(ptr %vec) #0 {
entry:
  %0 = load <2 x double>, ptr %vec, align 4
  %1 = fneg <2 x double> %0
  %2 = extractelement <2 x double> %1, i64 0
  %3 = call double @llvm.experimental.constrained.fmuladd.f32(double %2, double 0.000000e+00, double 0.000000e+00, metadata !"round.tonearest", metadata !"fpexcept.strict") #0
  ret double %3
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare double @llvm.experimental.constrained.fmuladd.f32(double, double, double, metadata, metadata) #1

attributes #0 = { strictfp }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }