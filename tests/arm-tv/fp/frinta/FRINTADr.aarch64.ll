; Function Attrs: strictfp
define void @x264_weights_analyse() #0 {
entry:
  %0 = tail call double @llvm.experimental.constrained.round.f64(double 0.000000e+00, metadata !"fpexcept.strict") #0
  ret void
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare double @llvm.experimental.constrained.round.f64(double, metadata) #1

attributes #0 = { strictfp }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }