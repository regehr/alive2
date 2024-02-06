; Function Attrs: strictfp
define void @_ZN11xercesc_2_78XMLFloat13checkBoundaryEPc() #0 {
entry:
  %conv2 = tail call float @llvm.experimental.constrained.sitofp.f32.i32(i32 0, metadata !"round.tonearest", metadata !"fpexcept.strict") #0
  ret void
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare float @llvm.experimental.constrained.sitofp.f32.i32(i32, metadata, metadata) #1

attributes #0 = { strictfp }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }