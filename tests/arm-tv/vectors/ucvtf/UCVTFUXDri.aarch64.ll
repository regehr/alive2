; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare double @llvm.experimental.constrained.uitofp.f64.i64(i64, metadata, metadata) #0

; Function Attrs: strictfp
define double @spec_genrand_res53() #1 {
entry:
  %conv = tail call double @llvm.experimental.constrained.uitofp.f64.i64(i64 0, metadata !"round.tonearest", metadata !"fpexcept.strict") #1
  ret double 0.000000e+00
}

attributes #0 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }
attributes #1 = { strictfp }