define float @floor_float(float noundef %0) {
  %2 = call float @llvm.floor.f32(float %0)
  ret float %2
}
