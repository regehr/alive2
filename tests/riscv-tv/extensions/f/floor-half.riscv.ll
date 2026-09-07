define half @floor_half(half noundef %0) {
  %2 = call half @llvm.floor.f16(half %0)
  ret half %2
}
