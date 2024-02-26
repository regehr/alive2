; Function Attrs: strictfp
define void @ptcache_particle_interpolate() #0 {
entry:
  %conv = tail call i32 @llvm.experimental.constrained.fptosi.i32.f32(float 0.000000e+00, metadata !"fpexcept.strict") #0
  %conv1 = tail call float @llvm.experimental.constrained.sitofp.f32.i32(i32 %conv, metadata !"round.tonearest", metadata !"fpexcept.strict") #0
  %cmp3 = tail call i1 @llvm.experimental.constrained.fcmps.f32(float %conv1, float 0.000000e+00, metadata !"olt", metadata !"fpexcept.strict") #0
  ret void
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare float @llvm.experimental.constrained.sitofp.f32.i32(i32, metadata, metadata) #1

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare i32 @llvm.experimental.constrained.fptosi.i32.f32(float, metadata) #1

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite)
declare i1 @llvm.experimental.constrained.fcmps.f32(float, float, metadata, metadata) #1

attributes #0 = { strictfp }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(inaccessiblemem: readwrite) }