; Function Attrs: strictfp
define double @_ZNK11xalanc_1_108XBoolean12stringLengthEv() #0 {
entry:
  %conv3 = tail call double @llvm.experimental.constrained.uitofp.f64.i32(i32 0, metadata !"round.tonearest", metadata !"fpexcept.strict") #0
  ret double 0.000000e+00
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare double @llvm.experimental.constrained.uitofp.f64.i32(i32, metadata, metadata) #1

attributes #0 = { strictfp }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }