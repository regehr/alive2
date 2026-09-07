define half @rint_half(half noundef %0) {
  %2 = call half @llvm.rint.f16(half %0)
  ret half %2
}
