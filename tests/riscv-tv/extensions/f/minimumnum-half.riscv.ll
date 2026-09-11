; Symbolic operands cover finite values, infinities, NaNs, and signed zeros.
define half @minimumnum_half(half noundef %a, half noundef %b) {
  %r = call half @llvm.minimumnum.f16(half %a, half %b)
  ret half %r
}
