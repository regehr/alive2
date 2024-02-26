; Function Attrs: strictfp
define void @cloth_clear_cache() #0 {
entry:
  %conv = call i32 @llvm.experimental.constrained.fptoui.i32.f32(float 0.000000e+00, metadata !"fpexcept.strict") #0
  ret void
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare i32 @llvm.experimental.constrained.fptoui.i32.f32(float, metadata) #1

attributes #0 = { strictfp }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }