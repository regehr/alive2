; Symbolic operands cover finite values, infinities, NaNs, and signed zeros.
define half @maximum_half(half noundef %a, half noundef %b) {
  %r = call half @llvm.maximum.f16(half %a, half %b)
  ret half %r
}
