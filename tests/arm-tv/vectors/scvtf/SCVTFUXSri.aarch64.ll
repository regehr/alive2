; Function Attrs: strictfp
define void @initialize_particle() #0 {
entry:
  %conv = tail call float @llvm.experimental.constrained.sitofp.f32.i64(i64 0, metadata !"round.tonearest", metadata !"fpexcept.strict") #0
  ret void
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare float @llvm.experimental.constrained.sitofp.f32.i64(i64, metadata, metadata) #1

attributes #0 = { strictfp }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }