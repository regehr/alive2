; Function Attrs: strictfp
define void @rgb_uchar_to_float() #0 {
entry:
  %conv = tail call float @llvm.experimental.constrained.uitofp.f32.i8(i8 0, metadata !"round.tonearest", metadata !"fpexcept.strict") #0
  ret void
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare float @llvm.experimental.constrained.uitofp.f32.i8(i8, metadata, metadata) #1

attributes #0 = { strictfp }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }