define dso_local noundef float @sqrt(float noundef %x) local_unnamed_addr #0 {
entry:
  %1 = call float @llvm.sqrt.f32(float %x)
  ret float %1
}
