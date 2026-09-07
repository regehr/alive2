define half @nearbyint_half(half noundef %0) {
  %2 = call half @llvm.nearbyint.f16(half %0)
  ret half %2
}
