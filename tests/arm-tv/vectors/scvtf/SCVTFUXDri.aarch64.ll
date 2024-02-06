; Function Attrs: strictfp
define i32 @S_sv_ncmp() #0 {
entry:
  %conv = tail call double @llvm.experimental.constrained.sitofp.f64.i64(i64 0, metadata !"round.tonearest", metadata !"fpexcept.strict") #0
  ret i32 0
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare double @llvm.experimental.constrained.sitofp.f64.i64(i64, metadata, metadata) #1

attributes #0 = { strictfp }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }