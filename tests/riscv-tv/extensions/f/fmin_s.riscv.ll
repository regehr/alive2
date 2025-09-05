define dso_local noundef float @mins(float noundef %x, float noundef %y, float noundef %z) local_unnamed_addr #0 {
entry:
  %1 = call float @llvm.minnum.f32(float %x, float %y)
  %2 = call float @llvm.minnum.f32(float %z, float %1)
  ret float %2
}
