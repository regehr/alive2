; Function Attrs: strictfp
define void @BLI_rcti_rctf_copy() #0 {
entry:
  %0 = tail call float @llvm.experimental.constrained.floor.f32(float 0.000000e+00, metadata !"fpexcept.strict") #0
  ret void
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare float @llvm.experimental.constrained.floor.f32(float, metadata) #1

attributes #0 = { strictfp }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }