; Symbolic operands cover finite values, infinities, NaNs, and signed zeros.
define float @maxnum_float(float noundef %a, float noundef %b) {
  %r = call float @llvm.maxnum.f32(float %a, float %b)
  ret float %r
}
