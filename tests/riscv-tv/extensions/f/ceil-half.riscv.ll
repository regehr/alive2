define half @ceil_half(half noundef %0) {
  %2 = call half @llvm.ceil.f16(half %0)
  ret half %2
}
