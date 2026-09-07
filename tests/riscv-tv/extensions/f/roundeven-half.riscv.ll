define half @roundeven_half(half noundef %0) {
  %2 = call half @llvm.roundeven.f16(half %0)
  ret half %2
}
