; Function Attrs: strictfp
define double @_ZNK11xalanc_1_107XString12stringLengthEv() #0 {
entry:
  %conv = tail call double @llvm.experimental.constrained.uitofp.f64.i32(i32 1, metadata !"round.tonearest", metadata !"fpexcept.strict") #0
  ret double %conv
}

; Function Attrs: nocallback nofree nosync nounwind strictfp willreturn memory(inaccessiblemem: readwrite)
declare double @llvm.experimental.constrained.uitofp.f64.i32(i32, metadata, metadata) #1

attributes #0 = { strictfp }
attributes #1 = { nocallback nofree nosync nounwind strictfp willreturn memory(inaccessiblemem: readwrite) }