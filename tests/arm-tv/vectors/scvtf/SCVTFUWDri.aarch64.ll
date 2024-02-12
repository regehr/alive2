; Function Attrs: strictfp
define ptr @_ZN11xercesc_2_722XMLAbstractDoubleFloatC2EPNS_13MemoryManagerE() #0 {
entry:
  %conv = tail call double @llvm.experimental.constrained.sitofp.f64.i32(i32 0, metadata !"round.tonearest", metadata !"fpexcept.strict") #0
  ret ptr null
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare double @llvm.experimental.constrained.sitofp.f64.i32(i32, metadata, metadata) #1

attributes #0 = { strictfp }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }