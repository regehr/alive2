define float @rint_float(float noundef %0) {
  %2 = call float @llvm.rint.f32(float %0)
  ret float %2
}
