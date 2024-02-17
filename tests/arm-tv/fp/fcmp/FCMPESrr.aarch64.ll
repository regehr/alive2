; Function Attrs: strictfp
define void @_ZN3pov17colour2photonRgbeEPhPf() #0 {
entry:
  %cmp4 = tail call i1 @llvm.experimental.constrained.fcmps.f32(float 0.000000e+00, float 1.000000e+00, metadata !"ogt", metadata !"fpexcept.strict") #0
  ret void
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare i1 @llvm.experimental.constrained.fcmps.f32(float, float, metadata, metadata) #1

attributes #0 = { strictfp }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }