define float @roundeven_float(float noundef %0) {
  %2 = call float @llvm.roundeven.f32(float %0)
  ret float %2
}
