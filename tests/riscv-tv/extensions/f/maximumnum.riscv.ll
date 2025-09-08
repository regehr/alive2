define float @f(float %0, float %1) {
  %3 = call float @llvm.maximumnum.f32(float %0, float %1)
  ret float %3
}
