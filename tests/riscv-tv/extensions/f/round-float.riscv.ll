define float @round_float(float noundef %0) {
  %2 = call float @llvm.round.f32(float %0)
  ret float %2
}
