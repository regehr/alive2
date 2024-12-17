; ModuleID = '<stdin>'
source_filename = "<stdin>"

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare float @llvm.ceil.f32(float) #0

; Function Attrs: nounwind
define <4 x float> @f(<4 x float> %0, <4 x float> %1, <4 x float> %2) {
  %4 = fcmp oeq <4 x float> %0, %1
  %5 = extractelement <4 x i1> %4, i64 0
  %6 = extractelement <4 x float> %0, i64 0
  %7 = tail call float @llvm.ceil.f32(float %6)
  %8 = extractelement <4 x float> %2, i64 0
  %9 = select i1 %5, float %7, float %8
  %10 = insertelement <4 x float> %1, float %9, i64 0
  ret <4 x float> %10
}



