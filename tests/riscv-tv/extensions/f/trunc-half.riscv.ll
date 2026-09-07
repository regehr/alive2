define half @trunc_half(half noundef %0) {
  %2 = call half @llvm.trunc.f16(half %0)
  ret half %2
}
