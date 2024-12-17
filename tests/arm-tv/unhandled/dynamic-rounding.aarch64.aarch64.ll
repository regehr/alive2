declare float @llvm.experimental.constrained.uitofp.f32.i32(i32, metadata, metadata) #0

define float @uitofp_sext_i32_to_f32(i32 %0) {
  %2 = call float @llvm.experimental.constrained.uitofp.f32.i32(i32 %0, metadata !"round.dynamic", metadata !"fpexcept.strict")
  ret float %2
}

attributes #0 = { nocallback nofree nosync nounwind strictfp willreturn memory(inaccessiblemem: readwrite) }